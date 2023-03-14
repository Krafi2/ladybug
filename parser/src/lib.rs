#[macro_use]
mod test_utils;
mod lexer;
mod parser;
pub mod span;

use ariadne::{Color, Fmt, Label, Report, ReportKind};
use chumsky::{prelude::*, Flat};
use lexer::{Token, TokenTree};

use crate::span::AriadneSpan;
pub use crate::{
    parser::{Block, Body, Expr, Ident, Param},
    span::{Span, Spanned},
};

impl Token {
    fn kind(&self) -> TokenKind {
        self.into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokenKind {
    Ctrl(char),
    Ident,
    IdentOrStr,
    Str,
    Bool,
    Var,
    Code,
    Comment,
    Space,
    Line,
    Error,
    End,
}

impl From<&Token> for TokenKind {
    fn from(token: &Token) -> Self {
        match token {
            Token::Ctrl(char) => TokenKind::Ctrl(*char),
            Token::Ident(_) => TokenKind::Ident,
            Token::IdentOrStr(_) => TokenKind::IdentOrStr,
            Token::Str(_) => TokenKind::Str,
            Token::DelimStr(_) => TokenKind::Str,
            Token::Bool(_) => TokenKind::Bool,
            Token::Var(_) => TokenKind::Var,
            Token::Code(_) => TokenKind::Code,
            Token::Comment(_) => TokenKind::Comment,
            Token::Space(_) => TokenKind::Space,
            Token::Line => TokenKind::Line,
            Token::Error => TokenKind::Error,
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Ctrl(char) => write!(f, "'{}'", char),
            TokenKind::Ident => f.write_str("Ident"),
            TokenKind::IdentOrStr => f.write_str("IdentOrStr"),
            TokenKind::Str => f.write_str("String"),
            TokenKind::Bool => f.write_str("Boolean"),
            TokenKind::Var => f.write_str("Variable"),
            TokenKind::Code => f.write_str("Code"),
            TokenKind::Comment => f.write_str("Comment"),
            TokenKind::Space => f.write_str("Space"),
            TokenKind::Line => f.write_str("Newline"),
            TokenKind::End => f.write_str("End"),
            TokenKind::Error => f.write_str("Error"),
        }
    }
}

#[derive(Debug, Clone)]
enum ErrorKind {
    Parse(parser::Error),
    Lex(Simple<char, Span>),
}

#[derive(Debug)]
pub struct Error(ErrorKind);

impl Error {
    pub fn into_report<'a>(self, filename: &'a str) -> Report<span::AriadneSpan> {
        fn token_or_end<T: ToString>(t: &Option<T>) -> String {
            match t {
                Some(t) => {
                    let s = t.to_string();
                    match s.as_str() {
                        "\n" => "\\n".into(),
                        "\r" => "\\r".into(),
                        "\t" => "\\t".into(),
                        "\x0B" => "\\x0B".into(),
                        "\x0C" => "\\x0C".into(),
                        "\u{0085}" => "\\u{0085}".into(),
                        "\u{2028}" => "\\u{2028}".into(),
                        "\u{2029}" => "\\u{2029}".into(),
                        _ => s,
                    }
                }
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
            ErrorKind::Parse(err) => match err {
                parser::Error::UnexpectedToken(span, expected, found) => {
                    Report::build(ReportKind::Error, filename, span.start)
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
                            Label::new(AriadneSpan::new(filename, span))
                                .with_message(format!(
                                    "Unexpected{} '{}'",
                                    some_or_end(&found, " token"),
                                    token_or_end(&found).fg(Color::Red)
                                ))
                                .with_color(Color::Red),
                        )
                        .finish()
                }
                parser::Error::InvalidIdent(span, ident) => Report::build(
                    ReportKind::Error,
                    filename,
                    span.start,
                )
                .with_message(format!("Invalid identifier '{ident}'"))
                .with_label(
                    Label::new(AriadneSpan::new(filename, span))
                        .with_message(format!("This identifier is not valid"))
                        .with_color(Color::Red),
                )
                .with_help("Identifiers cannot contain whitespace and must not begin with a number")
                .finish(),
            },
            ErrorKind::Lex(err) => {
                let span = err.span();
                let report = Report::build(ReportKind::Error, filename, span.start);

                match err.reason() {
                    chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                        .with_message(format!("Unclosed delimiter {}", delimiter.fg(Color::Red)))
                        .with_label(
                            Label::new(AriadneSpan::new(filename, span.clone()))
                                .with_message(format!(
                                    "Unclosed delimiter {}",
                                    delimiter.fg(Color::Red)
                                ))
                                .with_color(Color::Red),
                        )
                        .with_label(
                            Label::new(AriadneSpan::new(filename, err.span()))
                                .with_message(format!(
                                    "Must be closed before '{}'",
                                    token_or_end(&err.found()).fg(Color::Red)
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
                                _ => format!(
                                    "one of {}",
                                    err.expected()
                                        .map(|t| format!("'{}'", token_or_end(&t)))
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                ),
                            }
                        ))
                        .with_label(
                            Label::new(AriadneSpan::new(filename, err.span()))
                                .with_message(format!(
                                    "Unexpected token '{}'",
                                    token_or_end(&err.found()).fg(Color::Red)
                                ))
                                .with_color(Color::Red),
                        )
                        .finish(),
                    chumsky::error::SimpleReason::Custom(msg) => report
                        .with_message(msg)
                        .with_label(
                            Label::new(AriadneSpan::new(filename, err.span()))
                                .with_message(format!("{}", msg.fg(Color::Red)))
                                .with_color(Color::Red),
                        )
                        .finish(),
                }
            }
        }
    }
}

pub struct Parser {
    lexer: Box<dyn chumsky::Parser<char, Vec<Spanned<TokenTree>>, Error = Simple<char, Span>>>,
    parser: Box<dyn chumsky::Parser<Token, Vec<Spanned<Block>>, Error = parser::Error>>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            lexer: Box::new(lexer::lexer()),
            parser: Box::new(parser::parser()),
        }
    }

    pub fn parse(&self, source: &str) -> (Vec<Spanned<Block>>, Vec<Error>) {
        let stream = stream_from_str(source);
        let res = chumsky::Parser::parse_recovery(&self.lexer, stream);
        let (tts, lex_errors) = match res {
            (Some(tts), errors) => (tts, errors),
            (None, errors) => (vec![], errors),
        };

        let stream = tts_to_stream(tts);

        let (blocks, parse_errors) = chumsky::Parser::parse_recovery(&self.parser, stream);
        let errors = lex_errors
            .into_iter()
            .map(ErrorKind::Lex)
            .chain(parse_errors.into_iter().map(ErrorKind::Parse))
            .map(Error)
            .collect();

        (blocks.unwrap_or_default(), errors)
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

fn filter_token(token: Token) -> Option<Token> {
    match token {
        Token::Space(_) | Token::Comment(_) | Token::Line => None,
        Token::DelimStr(s) => Some(Token::Str(s)),
        _ => Some(token),
    }
}

fn tree_to_iter(tts: Vec<Spanned<TokenTree>>) -> impl Iterator<Item = (TokenTree, Span)> {
    tts.into_iter().map(|spanned| (spanned.inner, spanned.span))
}

fn tts_to_stream(tts: Vec<Spanned<TokenTree>>) -> chumsky::BoxStream<'static, Token, Span> {
    let end = tts.last().map(|t| t.span.end).unwrap_or(0);
    chumsky::BoxStream::from_nested(
        (end..end).into(),
        tree_to_iter(tts),
        |(tt, span)| match tt {
            TokenTree::Single(token) => match filter_token(token) {
                Some(token) => Flat::Single((token, span)),
                None => Flat::Many(tree_to_iter(Vec::new())),
            },
            TokenTree::Tree(tree) => Flat::Many(tree_to_iter(tree)),
        },
    )
}

fn stream_from_str<'a>(
    src: &'a str,
) -> chumsky::Stream<
    'a,
    char,
    Span,
    std::iter::Map<std::iter::Enumerate<std::str::Chars>, fn((usize, char)) -> (char, Span)>,
> {
    let len = src.chars().count();
    chumsky::Stream::<_, Span, _>::from_iter(
        Span::new(len, len),
        src.chars()
            .enumerate()
            .map(|(i, c)| (c, Span::new(i, i + 1))),
    )
}
