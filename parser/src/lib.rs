mod lexer;
mod parser;

use ariadne::{Color, Fmt, Label, Report, ReportKind};
use chumsky::prelude::*;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// A utility trait for binding tokens with their spans.
trait Spanner<I: Clone, O>: Parser<I, O>
where
    Self: Sized,
{
    /// Map the parser's output from `O` to to `(O, Span)`
    fn spanned(
        self,
    ) -> chumsky::combinator::MapWithSpan<
        Self,
        fn(
            O,
            <Self::Error as chumsky::Error<I>>::Span,
        ) -> (O, <Self::Error as chumsky::Error<I>>::Span),
        O,
    > {
        self.map_with_span(|tok, span| (tok, span))
    }

    /// Map the parser's output with `f` and place it into a tuplu with its span.
    fn with_span<F, T>(
        self,
        f: F,
    ) -> chumsky::combinator::MapWithSpan<
        chumsky::combinator::Map<Self, F, O>,
        fn(
            T,
            <Self::Error as chumsky::Error<I>>::Span,
        ) -> (T, <Self::Error as chumsky::Error<I>>::Span),
        T,
    >
    where
        F: Fn(O) -> T,
    {
        self.map(f).spanned()
    }
}

impl<P, I: Clone, O> Spanner<I, O> for P where P: Parser<I, O> {}

trait SpanExt<T, S> {
    fn token(&self) -> &T;
    fn span(&self) -> &S;
}

/// A utility trait for accessing the `token` and `span` components of a `Spanned` token.
impl<T> SpanExt<T, Span> for Spanned<T> {
    fn token(&self) -> &T {
        &self.0
    }

    fn span(&self) -> &Span {
        &self.1
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    Variable(Spanned<String>),
    String(Spanned<String>),
    Bool(Spanned<bool>),
    List(Spanned<Vec<Expr>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Spanned<String>,
    pub val: Expr,
}

type Params = Spanned<Vec<Spanned<Param>>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Item {
    pub name: Spanned<String>,
    pub args: Params,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl PartialEq for ErrorKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unexpected(l0, l1, l2), Self::Unexpected(r0, r1, r2)) => {
                l0 == r0 && l1 == r1 && l2 == r2
            }
            (Self::Multiple(l0), Self::Multiple(r0)) => l0 == r0,
            (Self::ExpectedTopic(l0, l1), Self::ExpectedTopic(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::MisplacedTopic(l0), Self::MisplacedTopic(r0)) => l0 == r0,
            (Self::MisplacedEnv(l0), Self::MisplacedEnv(r0)) => l0 == r0,
            (Self::UnexpectedBlock(l0, l1), Self::UnexpectedBlock(r0, r1)) => l0 == r0 && l1 == r1,
            // Not comparable
            (Self::LexError(_), Self::LexError(_)) => false,
            _ => false,
        }
    }
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
    let res = lexer::lexer().parse_recovery(source);
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
    let tokens = parser::map_tokens(tokens, span, parser::token_filter_enumerator);
    let parser = parser::parser();
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
