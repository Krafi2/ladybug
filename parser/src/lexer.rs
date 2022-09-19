use super::{Spanned, Spanner, Token};
use chumsky::{prelude::*, text::keyword, Parser};

trait MyParser: chumsky::Parser<char, Spanned<Token>, Error = Simple<char>> {}
impl<T: chumsky::Parser<char, Spanned<Token>, Error = Simple<char>>> MyParser for T {}

const RESERVED_CHARS: &[char] = &[
    ':', ',', ';', '(', ')', '[', ']', '{', '}', '"', '\'', '#', '\n', '$',
];

// Characters allowed as whitespace
fn space() -> impl MyParser {
    one_of(" \t")
        .repeated()
        .at_least(1)
        .collect()
        .with_span(Token::Space)
}

fn line() -> impl MyParser {
    text::newline().with_span(|_| Token::Line)
}

// Comments start with `#` and end in a newline
fn comment() -> impl MyParser {
    just('#')
        .chain(take_until(text::newline()).map(|(a, _)| a))
        .collect()
        .with_span(Token::Comment)
        .labelled("comment")
}

// Variables are in the form `$identifier`
fn variable() -> impl MyParser {
    just('$')
        .ignore_then(text::ident())
        .with_span(Token::Var)
        .labelled("variable")
}

fn spaced_string() -> impl MyParser {
    none_of('"')
        .repeated()
        .delimited_by(just('\"'), just('\"'))
        .collect()
        .with_span(Token::DelimStr)
        .or(filter(move |c: &char| !RESERVED_CHARS.contains(c))
            .repeated()
            .at_least(1)
            .collect()
            .with_span(Token::Str))
        .labelled("string")
}

// Strings inside lists are a bit more limited since they are delimited by spaces.
fn word_string() -> impl MyParser {
    none_of('"')
        .repeated()
        .delimited_by(just('\"'), just('\"'))
        .collect()
        .with_span(Token::DelimStr)
        .or(
            filter(move |c: &char| !RESERVED_CHARS.contains(c) && !c.is_whitespace())
                .repeated()
                .at_least(1)
                .collect()
                .with_span(Token::Str),
        )
        .labelled("word_string")
}

fn boolean() -> impl MyParser {
    keyword("yes")
        .to(Token::Bool(true))
        .or(keyword("no").to(Token::Bool(false)))
        .spanned()
}

fn list() -> impl MyParser {
    // Lists are delimited by `[]` and contain whitespace-separated expressions.
    recursive(|list| {
        choice((list, variable(), word_string(), space(), line()))
            .repeated()
            .delimited_by(just('['), just(']'))
            .collect()
            .with_span(Token::List)
            .recover_with(nested_delimiters('[', ']', [], |span| (Token::Error, span)))
    })
    .labelled("list")
}

// An expression can be either a string, a list, or a variable.
fn expression() -> impl MyParser {
    choice((variable(), list(), boolean(), spaced_string())).labelled("expression")
}

// A C-like identifier
fn ident() -> impl MyParser {
    text::ident().with_span(Token::Ident)
}

// A parameter takes the form of `ident~:~ expr`. `~` signifies optional whitespace.
fn param() -> impl MyParser {
    ident()
        .chain(space().repeated())
        .chain(just(':').with_span(Token::Ctrl))
        .chain(space().repeated())
        .chain(expression())
        .collect()
        .with_span(Token::Param)
}

// Parameters are a parentheses-delimited, comma-separated list of params with optional
// newlines and comments in-between.
fn params() -> impl MyParser {
    choice((
        one_of(",").with_span(Token::Ctrl),
        param(),
        comment(),
        line(),
        space(),
    ))
    .repeated()
    .delimited_by(just('('), just(')'))
    .collect()
    .with_span(Token::Params)
    .recover_with(nested_delimiters('(', ')', [('[', ']')], |span| {
        (Token::Error, span)
    }))
    .labelled("params")
}
// A block is `ident~(params)~{body}`

// A block containing key-value pairs in the form `key~:~value`, separated by newlines and
// optional comments.
fn map() -> impl MyParser {
    choice((just("unit"), just("env")))
        .from_str()
        .unwrapped()
        .with_span(Token::Name)
        .chain(space().repeated())
        .chain(params().repeated().at_most(1))
        .chain(space().repeated())
        .chain(
            choice((space(), line(), comment(), param()))
                .repeated()
                .delimited_by(just('{'), just('}'))
                .with_span(Token::Body)
                .recover_with(nested_delimiters('{', '}', [('[', ']')], |span| {
                    (Token::Error, span)
                }))
                .labelled("map"),
        )
        .collect()
        .with_span(Token::Block)
}

// A block of items, which are word-strings with an optional `params` block, separated by
// newlines with optional comments.
fn items() -> impl MyParser {
    choice((just("files"), just("packages")))
        .from_str()
        .unwrapped()
        .with_span(Token::Name)
        .chain(space().repeated())
        .chain(params().repeated().at_most(1))
        .chain(space().repeated())
        .chain(
            choice((space(), line(), comment(), params(), word_string()))
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
        .with_span(Token::Block)
}
// Captures all input between braces as a string.
fn content() -> impl MyParser {
    recursive(|content| {
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
    .labelled("content")
}

// A block which capture all of its body's content a a string.
fn routine() -> impl MyParser {
    text::ident()
        .from_str()
        .unwrapped()
        .with_span(Token::Name)
        .chain(space().repeated())
        .chain(params().repeated().at_most(1))
        .chain(space().repeated())
        .chain(content())
        .collect()
        .with_span(Token::Block)
}

/// Lex text input into tokens for use by the parser. The lexer capture some extra information that
/// isn't required by the parser, so that the text can be later re-serialized without ruining the
/// user's formatting.
pub(super) fn lexer() -> impl chumsky::Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    choice((space(), line(), comment(), map(), items(), routine()))
        .repeated()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::Token;

    test_parsers! {
        space {
            "  " => Token::Space("  ".into()),
            "\t" => Token::Space("\t".into()),
        }
        line {
            "\n" => Token::Line,
        }
        comment {
            "# A comment!\n" => Token::Comment("# A comment!".into()),
        }
        variable {
            "$var" => Token::Var("var".into()),
        }
        spaced_string {
            "\"String :)\"" => Token::DelimStr("String :)".into()),
            "This string contains spaces" => Token::Str("This string contains spaces".into()),
        }
        word_string {
            "No_spaces_allowed_here" => Token::Str("No_spaces_allowed_here".into()),
            "\"But they are allowed here!\"" => Token::DelimStr("But they are allowed here!".into())
        }
        boolean {
            "yes" => Token::Bool(true),
            "no" => Token::Bool(false),
        }
        list {
            "[a b \"abc\"]" => Token::List(vec![
                (Token::Str("a".into()), 1..2),
                (Token::Space(" ".into()), 2..3),
                (Token::Str("b".into()), 3..4),
                (Token::Space(" ".into()), 4..5),
                (Token::DelimStr("abc".into()), 5..10),
            ]),
        }
        ident {
            "foo" => Token::Ident("foo".into()),
        }
        param {
            "foo: bar" => Token::Param(vec![
                (Token::Ident("foo".into()), 0..3),
                (Token::Ctrl(':'), 3..4),
                (Token::Space(" ".into()), 4..5),
                (Token::Str("bar".into()), 5..8),
            ])
        }
        params {
            "(a: Hello spaces, b: [list list],\n# Comment\nc: \"Hi!\")" => Token::Params(vec![
                (
                    Token::Param(vec![
                        (Token::Ident( "a".into()), 1..2),
                        (Token::Ctrl( ':'), 2..3),
                        (Token::Space( " ".into()), 3..4),
                        (Token::Str( "Hello spaces".into()), 4..16),
                    ]),
                    1..16,
                ),
                (Token::Ctrl(','), 16..17),
                (Token::Space(" ".into()), 17..18),
                (
                    Token::Param(vec![
                        (Token::Ident("b".into()), 18..19),
                        (Token::Ctrl(':'), 19..20),
                        (Token::Space(" ".into()), 20..21),
                        (Token::List(vec![
                                (Token::Str("list".into()), 22..26),
                                (Token::Space(" ".into()), 26..27),
                                (Token::Str("list".into()), 27..31),
                            ]),
                            21..32,
                        ),
                    ]),
                    18..32,
                ),
                (Token::Ctrl(','), 32..33),
                (Token::Line, 33..34),
                (Token::Comment("# Comment".into()), 34..44),
                (
                    Token::Param(vec![
                            (Token::Ident("c".into()), 44..45),
                            (Token::Ctrl(':'), 45..46),
                            (Token::Space(" ".into()), 46..47),
                            (Token::DelimStr("Hi!".into()), 47..52),
                        ],
                    ),
                    44..52,
                ),
            ])
        }
        map {}
        items {}
        content {}
        routine {}
        parser {}
    }
}
