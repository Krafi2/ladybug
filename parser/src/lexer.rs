use chumsky::{prelude::*, text::keyword, Parser};

use crate::span::{Span, Spanned, Spanner};

/// Tokens for the lexer.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum Token {
    /// Control character
    Ctrl(char),
    /// Identifier
    Ident(String),
    /// Either an identifier or a string
    IdentOrStr(String),
    /// String
    Str(String),
    /// String delimited by `"`
    DelimStr(String),
    /// Bool
    Bool(bool),
    /// A variable
    Var(String),
    /// A code block
    Code(String),
    /// A comment
    Comment(String),
    /// One or more whitespace characters
    Space(String),
    /// One newline
    Line,
    Error,
}

#[derive(Debug, PartialEq)]
pub(crate) enum TokenTree {
    Single(Token),
    Tree(Vec<Spanned<TokenTree>>),
}

impl From<Spanned<Token>> for Token {
    fn from(tok: Spanned<Token>) -> Self {
        tok.inner
    }
}

trait TreeExt: Parser<char, Token, Error = Simple<char, Span>> + Sized
where
    Self::Error: chumsky::Error<char, Span = Span>,
{
    fn single(self) -> chumsky::combinator::Map<Self, fn(Token) -> TokenTree, Token> {
        self.map(TokenTree::Single)
    }
}

impl<P> TreeExt for P
where
    P: Parser<char, Token, Error = Simple<char, Span>>,
    Self::Error: chumsky::Error<char, Span = Span>,
{
}

trait TokParser: Parser<char, TokenTree, Error = Simple<char, Span>> + Clone {}
impl<T: Parser<char, TokenTree, Error = Simple<char, Span>> + Clone> TokParser for T {}

const RESERVED_CHARS: &[char] = &[
    ':', ',', ';', '(', ')', '[', ']', '{', '}', '"', '#', '\n', '$',
];

// Characters allowed as whitespace
fn space() -> impl TokParser {
    one_of(" \t")
        .repeated()
        .at_least(1)
        .collect()
        .map(Token::Space)
        .single()
}

fn line() -> impl TokParser {
    text::newline().map(|_| Token::Line).single()
}

// Comments start with `#` and end in a newline
fn comment() -> impl TokParser {
    just('#')
        .ignore_then(take_until(text::newline()).map(|(a, _)| a))
        .collect()
        .map(Token::Comment)
        .single()
        .labelled("comment")
}

// Variables are in the form `$identifier`
fn variable() -> impl TokParser {
    just('$')
        .ignore_then(text::ident())
        .map(Token::Var)
        .single()
        .labelled("variable")
}

fn delim_str() -> impl TokParser {
    none_of('"')
        .repeated()
        .delimited_by(just('\"'), just('\"'))
        .collect()
        .map(Token::DelimStr)
        .labelled("delim_str")
        .single()
}

fn string_chars() -> impl Parser<char, Vec<char>, Error = Simple<char, Span>> + Clone {
    filter(move |c: &char| !RESERVED_CHARS.contains(c) && !c.is_whitespace())
        .repeated()
        .at_least(1)
}

fn word_string() -> impl TokParser {
    delim_str().or(string_chars().collect().map(Token::Str).single())
}

fn ident_or_string() -> impl TokParser {
    delim_str().or(
        // parse words separated by whitespace
        string_chars()
            .chain::<char, _, _>(
                // parse multiple `space` `word` sequences
                recursive(|space_word| {
                    one_of(" \t")
                        .repeated()
                        .at_least(1)
                        .chain::<char, _, _>(string_chars())
                        // repeat or exit
                        .chain::<char, _, _>(space_word.or_not().flatten())
                })
                .or_not()
                .flatten(),
            )
            .collect()
            .map(Token::IdentOrStr)
            .single()
            .labelled("string"),
    )
}

fn boolean() -> impl TokParser {
    keyword("yes")
        .to(Token::Bool(true))
        .or(keyword("no").to(Token::Bool(false)))
        .single()
}

// Captures all input between braces as a string.
fn code() -> impl TokParser {
    recursive(|code| {
        choice((
            just(vec!['{', '}']),
            just('{').chain(code.repeated().flatten()).chain(just('}')),
            none_of("{}").repeated().at_least(1),
        ))
    })
    .repeated()
    .flatten()
    .collect()
    .delimited_by(just(['{', '{']), just(['}', '}']))
    .map(Token::Code)
    .single()
    .labelled("code")
}

fn control(c: char) -> impl TokParser {
    just(c).map(Token::Ctrl).single()
}

fn options(expr: impl TokParser) -> impl TokParser {
    control('(')
        .spanned()
        .chain(
            choice((
                control(','),
                control(':'),
                comment(),
                line(),
                space(),
                expr,
                ident_or_string(),
            ))
            .spanned()
            .repeated(),
        )
        .chain(control(')').spanned())
        .map(TokenTree::Tree)
}

// Lists are delimited by `[]` and contain whitespace-separated expressions.
fn list(expr: impl TokParser) -> impl TokParser {
    control('[')
        .spanned()
        .chain(
            choice((comment(), line(), space(), expr, word_string()))
                .spanned()
                .repeated(),
        )
        .chain(control(']').spanned())
        .map(TokenTree::Tree)
        .recover_with(nested_delimiters(
            '[',
            ']',
            [('(', ')'), ('{', ')')],
            |_| TokenTree::Single(Token::Error),
        ))
        .labelled("list")
}

fn map(expr: impl TokParser) -> impl TokParser {
    control('{')
        .spanned()
        .chain(
            choice((
                control(':'),
                comment(),
                line(),
                space(),
                expr,
                ident_or_string(),
            ))
            .spanned()
            .repeated(),
        )
        .chain(control('}').spanned())
        .map(TokenTree::Tree)
        .recover_with(nested_delimiters(
            '{',
            '}',
            [('(', ')'), ('[', ']')],
            |_| TokenTree::Single(Token::Error),
        ))
        .labelled("list")
}

/// Parse an expression token. Please note that strings aren't parsed and should be handled by the calling parser.
fn expr() -> impl TokParser {
    recursive(|expr| {
        choice((
            list(expr.clone()),
            map(expr.clone()),
            options(expr),
            code(),
            variable(),
            boolean(),
        ))
    })
}

fn ident() -> impl TokParser {
    text::ident().map(Token::Ident).single()
}

/// Lex text input into tokens for use by the parser. The lexer capture some extra information that
/// isn't required by the parser, so that the text can be later re-serialized without ruining the
/// user's formatting.
pub(crate) fn lexer(
) -> impl chumsky::Parser<char, Vec<Spanned<TokenTree>>, Error = Simple<char, Span>> {
    let expr = expr();
    choice((
        line(),
        space(),
        comment(),
        code(),
        options(expr.clone()),
        list(expr.clone()),
        map(expr),
        ident(),
    ))
    .spanned()
    .repeated()
    .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::{TokParser, Token, TokenTree};
    type Error = chumsky::error::Simple<char, crate::Span>;
    type Result<T> = std::result::Result<T, Vec<Error>>;

    fn ok<T>(t: T) -> Result<T> {
        Ok(t)
    }

    fn unwrap_single(tok: Result<TokenTree>) -> Result<Token> {
        tok.map(|tok| match tok {
            TokenTree::Single(tok) => tok,
            TokenTree::Tree(_) => unreachable!(),
        })
    }

    test_parsers! {
        @stream: crate::stream_from_str;
        @expected: ok;
        @output: unwrap_single;

        space {
            "  " => Token::Space("  ".into()),
            "\t" => Token::Space("\t".into()),
        }
        line {
            "\n" => Token::Line,
        }
        comment {
            "# A comment!\n" => Token::Comment(" A comment!".into()),
        }
        variable {
            "$var" => Token::Var("var".into()),
        }
        boolean {
            "yes" => Token::Bool(true),
            "no" => Token::Bool(false),
        }
        ident_or_string {
            "\"a string\"" => Token::DelimStr("a string".into()),
            "a string" => Token::IdentOrStr("a string".into()),
            "g'day" => Token::IdentOrStr("g'day".into()),
        }
        word_string {
            "\"a string\"" => Token::DelimStr("a string".into()),
            "string" => Token::Str("string".into()),
        }
        code {
            "{{{}}}" => Token::Code("{}".into()),
            "{{foo {bar}{{}}}}" => Token::Code("foo {bar}{{}}".into()),
        }
    }

    fn list() -> impl TokParser {
        super::list(super::expr())
    }
    fn map() -> impl TokParser {
        super::map(super::expr())
    }
    fn options() -> impl TokParser {
        super::options(super::expr())
    }

    fn flatten_tts(tts: Result<TokenTree>) -> Result<Vec<Token>> {
        fn flatten(tt: TokenTree, buf: &mut Vec<Token>) {
            match tt {
                TokenTree::Single(tok) => buf.push(tok),
                TokenTree::Tree(tts) => {
                    for tt in tts {
                        flatten(tt.inner, buf)
                    }
                }
            }
        }

        tts.map(|tt| {
            let mut buf = Vec::new();
            flatten(tt, &mut buf);
            buf
        })
    }

    mod collections {
        use super::*;

        test_parsers! {
            @stream: crate::stream_from_str;
            @expected: ok;
            @output: flatten_tts;

            list {
                "[a b \"c d\"]" => vec![
                    Token::Ctrl('['),
                    Token::Str("a".into()),
                    Token::Space(" ".into()),
                    Token::Str("b".into()),
                    Token::Space(" ".into()),
                    Token::DelimStr("c d".into()),
                    Token::Ctrl(']'),
                ],
                "[a[b[c[]]]]" => vec![
                    Token::Ctrl('['),
                    Token::Str("a".into()),
                    Token::Ctrl('['),
                    Token::Str("b".into()),
                    Token::Ctrl('['),
                    Token::Str("c".into()),
                    Token::Ctrl('['),
                    Token::Ctrl(']'),
                    Token::Ctrl(']'),
                    Token::Ctrl(']'),
                    Token::Ctrl(']'),
                ],
                "[#comment\nno comment]" => vec![
                    Token::Ctrl('['),
                    Token::Comment("comment".into()),
                    Token::Bool(false),
                    Token::Space(" ".into()),
                    Token::Str("comment".into()),
                    Token::Ctrl(']'),
                ]
            }
            map {
                "{foo: bar bar\nfoo: item(param: val)}" => vec![
                    Token::Ctrl('{'),
                    Token::IdentOrStr("foo".into()),
                    Token::Ctrl(':'),
                    Token::Space(" ".into()),
                    Token::IdentOrStr("bar bar".into()),
                    Token::Line,
                    Token::IdentOrStr("foo".into()),
                    Token::Ctrl(':'),
                    Token::Space(" ".into()),
                    Token::IdentOrStr("item".into()),
                    Token::Ctrl('('),
                    Token::IdentOrStr("param".into()),
                    Token::Ctrl(':'),
                    Token::Space(" ".into()),
                    Token::IdentOrStr("val".into()),
                    Token::Ctrl(')'),
                    Token::Ctrl('}'),
                ]
            }
            options {
                "(foo: $bar, hello: world(), with_spaces: hello spaces)" => vec![
                    Token::Ctrl('('),
                    Token::IdentOrStr("foo".into()),
                    Token::Ctrl(':'),
                    Token::Space(" ".into()),
                    Token::Var("bar".into()),
                    Token::Ctrl(','),
                    Token::Space(" ".into()),
                    Token::IdentOrStr("hello".into()),
                    Token::Ctrl(':'),
                    Token::Space(" ".into()),
                    Token::IdentOrStr("world".into()),
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    Token::Ctrl(','),
                    Token::Space(" ".into()),
                    Token::IdentOrStr("with_spaces".into()),
                    Token::Ctrl(':'),
                    Token::Space(" ".into()),
                    Token::IdentOrStr("hello spaces".into()),
                    Token::Ctrl(')'),
                ]
            }
        }
    }
}
