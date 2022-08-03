#![allow(dead_code)]

use chumsky::{
    prelude::*,
    text::{ident, Character},
};

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

/// Tokens for the lexer.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    /// Control character
    Ctrl(char),
    /// String
    Str(String),
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
    /// A top-level block
    Block(Vec<Spanned<Token>>),
}

use spanner::Spanner;

use self::spanner::SpanExt;

/// A utility module for working with spans.
mod spanner {
    use chumsky::{
        combinator::{Map, MapWithSpan},
        Error, Parser,
    };

    /// A utility module for binding tokens with their spans.
    pub trait Spanner<I: Clone, O>: Parser<I, O>
    where
        Self: Sized,
    {
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
            self.map(f).map_with_span(|tok, span| (tok, span))
        }
    }

    impl<P, I: Clone, O> Spanner<I, O> for P where P: Parser<I, O> {}

    pub trait SpanExt<T, S> {
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

    // Lists are delimited by `[]` and contain whitespace-separated expressions.
    let list = recursive(|list| {
        choice((list, variable.clone(), word_string.clone(), space.clone()))
            .repeated()
            .delimited_by(just('['), just(']'))
            .collect()
            .with_span(Token::List)
    })
    .labelled("list");

    // An expression can be either a string, a list, or a variable.
    let expression =
        choice((variable.clone(), list.clone(), spaced_string.clone())).labelled("expression");

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
    .with_span(Token::Params);

    // A block is `ident~(params)~{body}`

    // A block containing key-value pairs in the form `key~:~value`, separated by newlines and
    // optional comments.
    let map = choice((just("topic"), just("env")))
        .map(ToOwned::to_owned)
        .with_span(Token::Ident)
        .chain(space.clone().repeated())
        .chain(params.clone().repeated().at_most(1))
        .chain(space.clone().repeated())
        .chain(
            choice((space.clone(), line.clone(), comment.clone(), param.clone()))
                .repeated()
                .delimited_by(just('{'), just('}'))
                .with_span(Token::Body)
                .labelled("map"),
        )
        .collect()
        .with_span(Token::Block);

    // A block of items, which are word-strings with an optional `params` block, separated by
    // newlines with optional comments.
    let items = choice((just("files"), just("packages")))
        .map(ToOwned::to_owned)
        .with_span(Token::Ident)
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
    .labelled("content");

    // A block which capture all of its body's content a a string.
    let routine = choice((just("deploy"), just("remove"), just("capture")))
        .map(ToOwned::to_owned)
        .with_span(Token::Ident)
        .chain(space.clone().repeated())
        .chain(params.clone().repeated().at_most(1))
        .chain(space.clone().repeated())
        .chain(content)
        .collect()
        .with_span(Token::Block);

    choice((space, line, comment, map, items, routine))
        .recover_with(skip_then_retry_until([]))
        .repeated()
        .then_ignore(end())
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Expression {
    Variable(String),
    String(String),
    List(Vec<Expression>),
}

#[derive(Debug, Clone)]
struct Param {
    param: String,
    val: Expression,
}

#[derive(Debug, Clone)]
struct Package {
    name: String,
    params: Vec<Param>,
}

#[derive(Debug, Clone)]
enum Block {
    Topic {
        params: Vec<Param>,
        body: Vec<Param>,
    },
    Env {
        params: Vec<Param>,
        body: Vec<Param>,
    },
    Packages {
        params: Vec<Param>,
        packages: Vec<Package>,
    },
    Files {
        params: Vec<Param>,
        files: Vec<Package>,
    },
    Deploy {
        params: Vec<Param>,
        content: String,
    },
    Remove {
        params: Vec<Param>,
        content: String,
    },
    Capture {
        params: Vec<Param>,
        content: String,
    },
}

use filter_ext::filter_tokens;
mod filter_ext {
    use chumsky::Stream;

    use super::{Span, SpanExt, Spanned, Token};

    /// A utility iterator which filters `Token`s. The lifetime `'a` is present so that the
    /// type checker is satisfied when we create a `Stream`.
    pub(super) struct TokenFilter<'a>(
        std::iter::Filter<std::vec::IntoIter<(Token, Span)>, fn(&(Token, Span)) -> bool>,
        std::marker::PhantomData<&'a ()>,
    );

    impl<'a> Iterator for TokenFilter<'a> {
        type Item = (Token, Span);

        fn next(&mut self) -> Option<Self::Item> {
            self.0.next()
        }
    }

    /// Filter out tokens with no impact on parsing, such as spaces, lines, and comments.
    fn token_filter(t: &Spanned<Token>) -> bool {
        match t.token() {
            Token::Space(_) | Token::Comment(_) | Token::Line => false,
            _ => true,
        }
    }

    /// Convert a vector of tokens into a stream which discards superfluous tokens.
    pub(super) fn filter_tokens<'a>(
        tokens: Vec<Spanned<Token>>,
    ) -> Stream<'a, Token, Span, TokenFilter<'a>> {
        let end = tokens.last().unwrap().span().end;

        Stream::from_iter(
            end..end + 1,
            TokenFilter(
                tokens.into_iter().filter(token_filter),
                std::marker::PhantomData,
            ),
        )
    }
}

fn parser() {
    let source = std::fs::read_to_string("config_example").unwrap();
    let res = lexer().parse_recovery(source);
    let (tokens, errors) = match res {
        (Some(tokens), errors) => (tokens, errors),
        (None, _errors) => todo!(),
    };

    let tokens = filter_tokens(tokens);
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    #[test]
    fn lexer() {
        let source = std::fs::read_to_string("config_example").unwrap();
        let ast = super::lexer().parse(source);
        println!("{:#?}", ast);
    }

    #[test]
    fn parser() {
        super::parser()
    }
}
