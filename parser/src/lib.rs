use ariadne::{Color, Fmt, Label, Report, ReportKind};
use chumsky::{
    prelude::*,
    text::{ident, keyword, Character},
};
use std::str::FromStr;

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Name {
    Topic,
    Env,
    Packages,
    Files,
    Deploy,
    Remove,
    Capture,
    Other(String),
}

impl FromStr for Name {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let res = match s {
            "topic" => Self::Topic,
            "env" => Self::Env,
            "packages" => Self::Packages,
            "files" => Self::Files,
            "deploy" => Self::Deploy,
            "remove" => Self::Remove,
            "capture" => Self::Capture,
            _ => Self::Other(s.to_owned()),
        };
        Ok(res)
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Name::Topic => f.write_str("topic"),
            Name::Env => f.write_str("env"),
            Name::Packages => f.write_str("packages"),
            Name::Files => f.write_str("files"),
            Name::Deploy => f.write_str("deploy"),
            Name::Remove => f.write_str("remove"),
            Name::Capture => f.write_str("capture"),
            Name::Other(other) => f.write_str(&other),
        }
    }
}

/// Tokens for the lexer.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    /// Control character
    Ctrl(char),
    /// String
    Str(String),
    /// Bool
    Bool(bool),
    /// String delimited by `"`
    DelimStr(String),
    /// A variable
    Var(String),
    /// A list
    List(Vec<Spanned<Token>>),
    /// An identifier
    Ident(String),
    /// A comment
    Comment(String),
    /// One or more whitespace characters
    Space(String),
    /// One newline
    Line,
    /// A single key value pair
    Param(Vec<Spanned<Token>>),
    /// A list of params
    Params(Vec<Spanned<Token>>),
    /// The body of a block
    Body(Vec<Spanned<Token>>),
    /// The name of a block
    Name(Name),
    /// A top-level block
    Block(Vec<Spanned<Token>>),
    Error,
}

impl Token {
    fn kind(&self) -> TokenKind {
        self.into()
    }
}

#[derive(Debug, Clone, Copy)]
enum TokenKind {
    Ctrl(char),
    Str,
    Bool,
    Var,
    List,
    Ident,
    Comment,
    Space,
    Line,
    Param,
    Params,
    Body,
    Name,
    Block,
    Error,
}

impl From<&Token> for TokenKind {
    fn from(token: &Token) -> Self {
        match token {
            Token::Ctrl(char) => TokenKind::Ctrl(*char),
            Token::Str(_) => TokenKind::Str,
            Token::Bool(_) => TokenKind::Bool,
            Token::DelimStr(_) => TokenKind::Str,
            Token::Var(_) => TokenKind::Var,
            Token::List(_) => TokenKind::List,
            Token::Ident(_) => TokenKind::Ident,
            Token::Comment(_) => TokenKind::Comment,
            Token::Space(_) => TokenKind::Space,
            Token::Line => TokenKind::Line,
            Token::Param(_) => TokenKind::Param,
            Token::Params(_) => TokenKind::Params,
            Token::Body(_) => TokenKind::Body,
            Token::Name(_) => TokenKind::Name,
            Token::Block(_) => TokenKind::Block,
            Token::Error => TokenKind::Error,
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Ctrl(char) => write!(f, "'{}'", char),
            TokenKind::Str => f.write_str("String"),
            TokenKind::Bool => f.write_str("Boolean"),
            TokenKind::Var => f.write_str("Variable"),
            TokenKind::List => f.write_str("List"),
            TokenKind::Ident => f.write_str("Identifier"),
            TokenKind::Comment => f.write_str("Comment"),
            TokenKind::Space => f.write_str("Space"),
            TokenKind::Line => f.write_str("Newline"),
            TokenKind::Param => f.write_str("Parameter"),
            TokenKind::Params => f.write_str("Parameters"),
            TokenKind::Body => f.write_str("Body"),
            TokenKind::Name => f.write_str("Identifier"),
            TokenKind::Block => f.write_str("Block"),
            TokenKind::Error => f.write_str("Error"),
        }
    }
}

use spanner::{SpanExt, Spanner};

/// A utility module for working with spans.
mod spanner {
    use chumsky::{
        combinator::{Map, MapWithSpan},
        Error, Parser,
    };

    /// A utility module for binding tokens with their spans.
    pub(super) trait Spanner<I: Clone, O>: Parser<I, O>
    where
        Self: Sized,
    {
        /// Map the parser's output from `O` to to `(O, Span)`
        fn spanned(
            self,
        ) -> MapWithSpan<
            Self,
            fn(O, <Self::Error as Error<I>>::Span) -> (O, <Self::Error as Error<I>>::Span),
            O,
        > {
            self.map_with_span(|tok, span| (tok, span))
        }

        /// Map the parser's output with `f` and place it into a tuplu with its span.
        fn with_span<F, T>(
            self,
            f: F,
        ) -> MapWithSpan<
            Map<Self, F, O>,
            fn(T, <Self::Error as Error<I>>::Span) -> (T, <Self::Error as Error<I>>::Span),
            T,
        >
        where
            F: Fn(O) -> T,
        {
            self.map(f).spanned()
        }
    }

    impl<P, I: Clone, O> Spanner<I, O> for P where P: Parser<I, O> {}

    pub(super) trait SpanExt<T, S> {
        fn token(&self) -> &T;
        fn span(&self) -> &S;
    }

    /// A utility trait for accessing the `token` and `span` components of a `Spanned` token.
    impl<T> SpanExt<T, super::Span> for super::Spanned<T> {
        fn token(&self) -> &T {
            &self.0
        }

        fn span(&self) -> &super::Span {
            &self.1
        }
    }
}

/// Lex text input into tokens for use by the parser. The lexer capture some extra information that
/// isn't required by the parser, so that the text can be later re-serialized without ruining the
/// user's formatting.
fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let reserved_chars = [
        ':', ',', ';', '(', ')', '[', ']', '{', '}', '"', '\'', '#', '\n', '$',
    ];

    // Characters allowed as whitespace
    let space = one_of(" \t")
        .repeated()
        .at_least(1)
        .collect()
        .with_span(Token::Space);

    let line = text::newline().with_span(|_| Token::Line);

    // Comments start with `#` and end in a newline
    let comment = just('#')
        .chain(take_until(text::newline()).map(|(a, _)| a))
        .collect()
        .with_span(Token::Comment)
        .labelled("comment");

    // Variables are in the form `$identifier`
    let variable = just('$')
        .ignore_then(text::ident())
        .with_span(Token::Var)
        .labelled("variable");

    // A string can either be delimited by `"` or or be undelimited if it doesn't contain any
    // reserved characters. Some of the reserved characters could be allowed without parser
    // ambiguity, but the user might be confused by which characters are allowd, so we take a more
    // conservative approach.
    let spaced_string = none_of('"')
        .repeated()
        .delimited_by(just('\"'), just('\"'))
        .collect()
        .with_span(Token::DelimStr)
        .or(filter(move |c: &char| !reserved_chars.contains(c))
            .repeated()
            .at_least(1)
            .collect()
            .with_span(Token::Str))
        .labelled("string");

    // Strings inside lists are a bit more limited since they are delimited by spaces.
    let word_string = none_of('"')
        .repeated()
        .delimited_by(just('\"'), just('\"'))
        .collect()
        .with_span(Token::DelimStr)
        .or(
            filter(move |c: &char| !reserved_chars.contains(c) && !c.is_whitespace())
                .repeated()
                .at_least(1)
                .collect()
                .with_span(Token::Str),
        )
        .labelled("word_string");

    let bool = keyword("yes")
        .to(Token::Bool(true))
        .or(keyword("no").to(Token::Bool(false)))
        .spanned();

    // Lists are delimited by `[]` and contain whitespace-separated expressions.
    let list = recursive(|list| {
        choice((list, variable.clone(), word_string.clone(), space.clone()))
            .repeated()
            .delimited_by(just('['), just(']'))
            .collect()
            .with_span(Token::List)
            .recover_with(nested_delimiters('[', ']', [], |span| (Token::Error, span)))
    })
    .labelled("list");

    // An expression can be either a string, a list, or a variable.
    let expression = choice((
        variable.clone(),
        list.clone(),
        bool.clone(),
        spaced_string.clone(),
    ))
    .labelled("expression");

    // A C-like identifier
    let ident = ident().with_span(Token::Ident);

    // A parameter takes the form of `ident~:~ expr`. `~` signifies optional whitespace.
    let param = ident
        .clone()
        .chain(space.clone().repeated())
        .chain(just(':').with_span(Token::Ctrl))
        .chain(space.clone().repeated())
        .chain(expression.clone())
        .collect()
        .with_span(Token::Param);

    // Parameters are a parentheses-delimited, comma-separated list of params with optional
    // newlines and comments in-between.
    let params = choice((
        one_of(",").with_span(Token::Ctrl),
        param.clone(),
        comment.clone(),
        line.clone(),
        space.clone(),
    ))
    .repeated()
    .delimited_by(just('('), just(')'))
    .collect()
    .with_span(Token::Params)
    .recover_with(nested_delimiters('(', ')', [('[', ']')], |span| {
        (Token::Error, span)
    }))
    .labelled("params");

    // A block is `ident~(params)~{body}`

    // A block containing key-value pairs in the form `key~:~value`, separated by newlines and
    // optional comments.
    let map = choice((just("topic"), just("env")))
        .from_str()
        .unwrapped()
        .with_span(Token::Name)
        .chain(space.clone().repeated())
        .chain(params.clone().repeated().at_most(1))
        .chain(space.clone().repeated())
        .chain(
            choice((space.clone(), line.clone(), comment.clone(), param.clone()))
                .repeated()
                .delimited_by(just('{'), just('}'))
                .with_span(Token::Body)
                .recover_with(nested_delimiters('{', '}', [('[', ']')], |span| {
                    (Token::Error, span)
                }))
                .labelled("map"),
        )
        .collect()
        .with_span(Token::Block);

    // A block of items, which are word-strings with an optional `params` block, separated by
    // newlines with optional comments.
    let items = choice((just("files"), just("packages")))
        .from_str()
        .unwrapped()
        .with_span(Token::Name)
        .chain(space.clone().repeated())
        .chain(params.clone().repeated().at_most(1))
        .chain(space.clone().repeated())
        .chain(
            choice((
                space.clone(),
                line.clone(),
                comment.clone(),
                params.clone(),
                word_string.clone(),
            ))
            .repeated()
            .delimited_by(just('{'), just('}'))
            .with_span(Token::Body)
            .recover_with(nested_delimiters(
                '{',
                '}',
                [('(', ')'), ('[', ']')],
                |span| (Token::Error, span),
            ))
            .labelled("items"),
        )
        .collect()
        .with_span(Token::Block);

    // Captures all input between braces as a string.
    let content = recursive(|content| {
        choice((
            just(vec!['{', '}']),
            just('{')
                .chain(content.repeated().flatten())
                .chain(just('}')),
            none_of("{}").repeated().at_least(1),
        ))
    })
    .repeated()
    .flatten()
    .collect()
    .with_span(Token::Str)
    .delimited_by(just('{'), just('}'))
    .map(|s| vec![s])
    .with_span(Token::Body)
    .recover_with(nested_delimiters(
        '{',
        '}',
        [('(', ')'), ('[', ']')],
        |span| (Token::Error, span),
    ))
    .labelled("content");

    // A block which capture all of its body's content a a string.
    let routine = text::ident()
        .from_str()
        .unwrapped()
        .with_span(Token::Name)
        .chain(space.clone().repeated())
        .chain(params.clone().repeated().at_most(1))
        .chain(space.clone().repeated())
        .chain(content)
        .collect()
        .with_span(Token::Block);

    choice((space, line, comment, map, items, routine))
        .repeated()
        .then_ignore(end())
}

use stream_ext::{map_tokens, token_filter, token_filter_enumerator};
mod stream_ext {
    use chumsky::Stream;

    use super::{Span, Spanned, Token};

    /// A utility iterator which filters `Token`s. The lifetime `'a` is present so that the
    /// type checker is satisfied when we create a `Stream`.
    pub struct TokenFilter<'a, I: Iterator>(I, std::marker::PhantomData<&'a ()>);

    impl<'a, I: Iterator> Iterator for TokenFilter<'a, I> {
        type Item = I::Item;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.next()
        }
    }

    /// Filter out tokens with no impact on parsing, such as spaces, lines, and comments.
    pub(super) fn token_filter(
        tokens: Vec<Spanned<Token>>,
    ) -> impl Iterator<Item = Spanned<Token>> {
        tokens.into_iter().filter_map(|(token, span)| match token {
            Token::Space(_) | Token::Comment(_) | Token::Line => None,
            Token::DelimStr(s) => Some((Token::Str(s), span)),
            _ => Some((token, span)),
        })
    }

    /// Filter out tokens with no impact on parsing, such as spaces, lines, and comments, and
    /// enumerate the result.
    pub(super) fn token_filter_enumerator(
        tokens: Vec<Spanned<Token>>,
    ) -> impl Iterator<Item = Spanned<(usize, Token)>> {
        token_filter(tokens)
            .enumerate()
            .map(|(i, (token, span))| ((i, token), span))
    }

    /// Convert a vector of tokens into a stream through an iterator.
    pub(super) fn map_tokens<
        'a,
        F: FnOnce(Vec<Spanned<Token>>) -> I,
        I: Iterator<Item = Spanned<T>>,
        T,
    >(
        tokens: Vec<Spanned<Token>>,
        span: Span,
        func: F,
    ) -> Stream<'a, T, Span, TokenFilter<'a, I>> {
        let end = span.end;
        Stream::from_iter(
            end..end + 1,
            TokenFilter(func(tokens), std::marker::PhantomData),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    Variable(Spanned<String>),
    String(Spanned<String>),
    Bool(Spanned<bool>),
    List(Spanned<Vec<Expr>>),
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Spanned<String>,
    pub val: Expr,
}

type Params = Spanned<Vec<Spanned<Param>>>;

#[derive(Debug, Clone)]
pub struct Item {
    pub name: Spanned<String>,
    pub args: Params,
}

#[derive(Debug, Clone)]
pub enum Block {
    Map {
        name: Name,
        params: Params,
        body: Params,
    },
    Items {
        name: Name,
        params: Params,
        items: Spanned<Vec<Item>>,
    },
    Routine {
        name: Name,
        params: Params,
        content: String,
    },
}

#[derive(Debug, Clone)]
enum ErrorKind {
    Unexpected(Span, Vec<Option<TokenKind>>, Option<TokenKind>),
    Multiple(Vec<ErrorKind>),
    ExpectedTopic(Span, Name),
    MisplacedTopic(Span),
    MisplacedEnv(Span),
    UnexpectedBlock(Span, Name),
    LexError(Simple<char>),
}

impl ErrorKind {
    fn expected(expected: Vec<TokenKind>, found: TokenKind, span: Span) -> Self {
        Self::Unexpected(
            span,
            expected.into_iter().map(Option::Some).collect(),
            Some(found),
        )
    }

    fn multiple(errors: Vec<Self>) -> Self {
        Self::Multiple(errors)
    }

    fn span(&self) -> Option<Span> {
        match self {
            ErrorKind::Unexpected(span, _, _) => Some(span).cloned(),
            ErrorKind::Multiple(_) => None,
            ErrorKind::ExpectedTopic(span, _) => Some(span).cloned(),
            ErrorKind::MisplacedTopic(span) => Some(span).cloned(),
            ErrorKind::MisplacedEnv(span) => Some(span).cloned(),
            ErrorKind::UnexpectedBlock(span, _) => Some(span).cloned(),
            ErrorKind::LexError(err) => Some(err.span()),
        }
    }
}

impl chumsky::error::Error<Token> for ErrorKind {
    type Span = Span;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token>,
    ) -> Self {
        Self::expected(
            expected
                .into_iter()
                .map(|t| t.as_ref().map(Token::kind).unwrap_or(TokenKind::Error))
                .collect(),
            found.as_ref().map(Token::kind).unwrap_or(TokenKind::Error),
            span,
        )
    }

    fn with_label(self, _label: Self::Label) -> Self {
        unimplemented!()
    }

    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::Multiple(mut e1), Self::Multiple(e2)) => {
                e1.extend_from_slice(&e2);
                Self::Multiple(e1)
            }
            (Self::Unexpected(span, mut expected1, found), Self::Unexpected(_, expected2, _)) => {
                expected1.extend_from_slice(&expected2);
                Self::Unexpected(span, expected1, found)
            }
            (err, Self::Unexpected(_, _, _)) => err,
            (Self::Unexpected(_, _, _), err) => err,
            (err1, _) => err1,
        }
    }
}

impl chumsky::error::Error<(usize, Token)> for ErrorKind {
    type Span = Span;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<(usize, Token)>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<(usize, Token)>,
    ) -> Self {
        Self::expected_input_found(
            span,
            expected.into_iter().map(|t| t.map(|(_, t)| t)),
            found.map(|(_, t)| t),
        )
    }

    fn with_label(self, label: Self::Label) -> Self {
        chumsky::error::Error::<Token>::with_label(self, label)
    }

    fn merge(self, other: Self) -> Self {
        chumsky::error::Error::<Token>::merge(self, other)
    }
}

fn block_parser() -> impl Parser<Token, Block, Error = ErrorKind> {
    // They said that I should flatten the input and I am starting to see why, but it WORKS!

    // # Expr

    let expr = recursive(|expr| {
        let list_parser = expr.repeated().spanned();

        filter_map(move |span, token| match token {
            Token::Str(s) => Ok(Expr::String((s, span))),
            Token::Bool(b) => Ok(Expr::Bool((b, span))),
            Token::List(list) => {
                let tokens = map_tokens(list, span, token_filter);
                list_parser
                    .parse(tokens)
                    .map(Expr::List)
                    .map_err(ErrorKind::multiple)
            }
            Token::Var(var) => Ok(Expr::Variable((var, span))),
            token => Err(ErrorKind::expected(
                vec![TokenKind::Str, TokenKind::List, TokenKind::Var],
                token.kind(),
                span,
            )),
        })
    });

    // # Param

    let param = filter_map(|span, token| match token {
        Token::Ident(ident) => Ok((ident, span)),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Ident],
            token.kind(),
            span,
        )),
    })
    .then_ignore(just(Token::Ctrl(':')))
    .then(expr)
    .map(|(param, val)| Param { name: param, val })
    .spanned();

    let params_iner_parser = param
        .clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing();

    let params_parser = filter_map(move |span, token| match token {
        Token::Param(tokens) => {
            let tokens = map_tokens(tokens, span, token_filter);
            params_iner_parser
                .parse(tokens)
                .map_err(ErrorKind::multiple)
        }
        token => Err(ErrorKind::expected(
            vec![TokenKind::Params],
            token.kind(),
            span,
        )),
    });

    let params = filter_map(move |span, token| match token {
        Token::Params(params) => {
            let tokens = map_tokens(params, span, token_filter);
            params_parser.parse(tokens).map_err(ErrorKind::multiple)
        }
        token => Err(ErrorKind::expected(
            vec![TokenKind::Params],
            token.kind(),
            span,
        )),
    })
    .or_not()
    .map(Option::unwrap_or_default)
    .spanned();

    // # Map

    let map_body_parser = param.clone().repeated().spanned();

    let map = filter_map(|span, token| match token {
        Token::Name(name @ Name::Topic) | Token::Name(name @ Name::Env) => Ok(name),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    })
    .then(params.clone())
    .then(filter_map(move |span, token| match token {
        Token::Body(body) => {
            let tokens = map_tokens(body, span, token_filter);
            map_body_parser.parse(tokens).map_err(ErrorKind::multiple)
        }
        token => Err(ErrorKind::expected(
            vec![TokenKind::Body],
            token.kind(),
            span,
        )),
    }))
    .map(|((name, params), body)| Block::Map { name, params, body });

    // # Items

    let items_body_parser = filter_map(|span, token| match token {
        Token::Str(s) => Ok((s, span)),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Str],
            token.kind(),
            span,
        )),
    })
    .then(params.clone())
    .map(|(item, params)| Item {
        name: item,
        args: params,
    })
    .repeated()
    .spanned();

    let items = filter_map(|span, token| match token {
        Token::Name(name @ Name::Packages) | Token::Name(name @ Name::Files) => Ok(name),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    })
    .then(params.clone())
    .then(filter_map(move |span, token| match token {
        Token::Body(body) => {
            let tokens = map_tokens(body, span, token_filter);
            items_body_parser.parse(tokens).map_err(ErrorKind::multiple)
        }
        token => Err(ErrorKind::expected(
            vec![TokenKind::Body],
            token.kind(),
            span,
        )),
    }))
    .map(|((name, params), items)| Block::Items {
        name,
        params,
        items,
    });

    // # Routines

    let routine_content_parser = filter_map(|span, token| match token {
        Token::Str(s) => Ok(s),
        _ => Err(ErrorKind::expected(
            vec![TokenKind::Str],
            token.kind(),
            span,
        )),
    });

    let routines = filter_map(|span, token| match token {
        Token::Name(name @ Name::Deploy)
        | Token::Name(name @ Name::Remove)
        | Token::Name(name @ Name::Capture) => Ok(name),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    })
    .then(params.clone())
    .then(filter_map(move |span, token| match token {
        Token::Body(body) => {
            let tokens = map_tokens(body, span, token_filter);
            routine_content_parser
                .parse(tokens)
                .map_err(ErrorKind::multiple)
        }
        token => Err(ErrorKind::expected(
            vec![TokenKind::Body],
            token.kind(),
            span,
        )),
    }))
    .map(|((name, params), content)| Block::Routine {
        name,
        params,
        content,
    });

    // # Other handling

    let other = filter_map(|span, token| match token {
        Token::Name(name @ Name::Other(_)) => Err(ErrorKind::UnexpectedBlock(span, name)),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    });

    choice((map, items, routines, other))
}

fn parser() -> impl Parser<(usize, Token), Vec<Option<Block>>, Error = ErrorKind> {
    let block_parser = block_parser();

    any()
        .validate(move |token, span, emit| match token {
            (n, Token::Block(tokens)) => {
                let mut stream = map_tokens(tokens, span, token_filter);
                let first = stream.fetch_tokens().next().expect("Found empty block");

                // Check if the order of the blocks is correct
                match first {
                    (Token::Name(name), name_span) => {
                        // The first block should be a topic declaration
                        if name != Name::Topic && n == 0 {
                            emit(ErrorKind::ExpectedTopic(name_span.clone(), name.clone()));
                        }

                        // The topic block should be the first in file
                        if name == Name::Topic && n > 0 {
                            emit(ErrorKind::MisplacedTopic(name_span.clone()));
                        }

                        // The env block should follow the topic declaration
                        if name == Name::Env && n > 1 {
                            emit(ErrorKind::MisplacedEnv(name_span.clone()));
                        }
                    }
                    (token, span) => emit(ErrorKind::expected(
                        vec![TokenKind::Name],
                        token.kind(),
                        span,
                    )),
                }

                match block_parser.parse(stream) {
                    Ok(block) => Some(block),
                    Err(errors) => {
                        emit(ErrorKind::multiple(errors));
                        None
                    }
                }
            }
            (_, token) => {
                emit(ErrorKind::expected(
                    vec![TokenKind::Block],
                    token.kind(),
                    span,
                ));
                None
            }
        })
        .repeated()
        .then_ignore(end())
}

pub struct Error(ErrorKind);

impl Error {
    pub fn gen_report<'a>(self, filename: &'a str) -> Report<(&'a str, Span)> {
        let span = self.0.span().expect("Expected spanned error");
        let report = Report::build(ReportKind::Error, filename, span.start);

        fn token_or_end<T: ToString>(t: &Option<T>) -> String {
            match t {
                Some(t) => t.to_string(),
                None => "end of file".to_string(),
            }
        }

        fn some_or_end<'a, T: ToString>(t: &Option<T>, s: &'a str) -> &'a str {
            match t {
                Some(_) => s,
                None => "end of file",
            }
        }

        match self.0 {
            ErrorKind::Unexpected(span, expected, found) => report
                .with_message(format!(
                    "Unexpected {}, expected {}",
                    some_or_end(&found, "token"),
                    match expected.len() {
                        0 => "something else".to_string(),
                        1 => token_or_end(&expected[0]),
                        _ => format!(
                            "one of {}",
                            expected
                                .iter()
                                .map(|t| format!("'{}'", token_or_end(t)))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                    }
                ))
                .with_label(
                    Label::new((filename, span))
                        .with_message(format!(
                            "Unexpected{} '{}'",
                            some_or_end(&found, " token"),
                            token_or_end(&found).fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                )
                .finish(),
            ErrorKind::ExpectedTopic(span, name) => report
                .with_message("The file should start with the topic declaration")
                .with_label(
                    Label::new((filename, span))
                        .with_message(format!("Expected 'topic', found '{}'", name).fg(Color::Red))
                        .with_color(Color::Red),
                )
                .finish(),
            ErrorKind::MisplacedTopic(span) => report
                .with_message("The topic declaration should be located at the start of the file")
                .with_label(
                    Label::new((filename, span))
                        .with_message("'topic' expected at the top of file".fg(Color::Red))
                        .with_color(Color::Red),
                )
                .finish(),
            ErrorKind::MisplacedEnv(span) => report
                .with_message("The env block should be placed directly after the topic declaration")
                .with_label(
                    Label::new((filename, span))
                        .with_message("Unexpected 'env'".fg(Color::Red))
                        .with_color(Color::Red),
                )
                .finish(),
            ErrorKind::UnexpectedBlock(span, name) => report
                .with_message(format!("Unknown block '{}'", name))
                .with_label(
                    Label::new((filename, span))
                        .with_message(
                            format!(
                                "Expected one of {}, found '{}'",
                                [
                                    Name::Topic,
                                    Name::Env,
                                    Name::Packages,
                                    Name::Files,
                                    Name::Deploy,
                                    Name::Remove,
                                    Name::Capture,
                                ]
                                .map(|name| format!("'{}'", name))
                                .join(","),
                                name,
                            )
                            .fg(Color::Red),
                        )
                        .with_color(Color::Red),
                )
                .finish(),
            ErrorKind::Multiple(_) => panic!("Expected flat error"),
            ErrorKind::LexError(err) => match err.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!("Unclosed delimiter {}", delimiter.fg(Color::Red)))
                    .with_label(
                        Label::new((filename, span.clone()))
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new((filename, err.span()))
                            .with_message(format!(
                                "Must be closed before '{}'",
                                err.found()
                                    .map(ToString::to_string)
                                    .unwrap_or("end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    )
                    .finish(),
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "Unexpected {}, expected {}",
                        some_or_end(&err.found(), "token"),
                        match err.expected().len() {
                            0 => "something else".to_string(),
                            1 => token_or_end(&err.expected().next().unwrap()),
                            _ => format!(
                                "one of {}",
                                err.expected()
                                    .map(|t| format!("'{}'", token_or_end(t)))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        }
                    ))
                    .with_label(
                        Label::new((filename, err.span()))
                            .with_message(format!(
                                "Unexpected token {}",
                                err.found()
                                    .map(ToString::to_string)
                                    .unwrap_or("end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    )
                    .finish(),
                chumsky::error::SimpleReason::Custom(msg) => report
                    .with_message(msg)
                    .with_label(
                        Label::new((filename, err.span()))
                            .with_message(format!("{}", msg.fg(Color::Red)))
                            .with_color(Color::Red),
                    )
                    .finish(),
            },
        }
    }
}

pub fn parse(source: &str) -> (Vec<Block>, Vec<Error>) {
    let res = lexer().parse_recovery(source);
    let (tokens, lex_errors) = match res {
        (Some(tokens), errors) => (tokens, errors),
        (None, errors) => (vec![], errors),
    };

    let span = match tokens.len() {
        0 => 0..0,
        _ => {
            tokens.first().expect("Expected tokens").span().start
                ..tokens.last().expect("Expected tokens").span().end
        }
    };
    let tokens = map_tokens(tokens, span, token_filter_enumerator);
    let parser = parser();
    let (blocks, parse_errors) = parser.parse_recovery(tokens);

    fn flatten(err: ErrorKind, vec: &mut Vec<Error>) {
        match err {
            ErrorKind::Multiple(errors) => {
                for err in errors {
                    flatten(err, vec)
                }
            }
            err => vec.push(Error(err)),
        }
    }

    let mut errors = Vec::new();
    for err in parse_errors {
        flatten(err, &mut errors);
    }

    errors.extend(
        lex_errors
            .into_iter()
            .map(|err| Error(ErrorKind::LexError(err))),
    );
    (
        blocks
            .unwrap_or_default()
            .into_iter()
            .filter_map(|b| b)
            .collect(),
        errors,
    )
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    #[test]
    fn lexer() {
        let source = std::fs::read_to_string("config_example").unwrap();
        let tokens = super::lexer().parse(source);
        println!("{:#?}", tokens);
    }

    #[test]
    fn parser() {
        let path = std::path::PathBuf::from("config_example");
        let source = std::fs::read_to_string(&path).unwrap();
        super::parse(&source);
    }
}
