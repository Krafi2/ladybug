use super::{Spanned, Spanner, Token};
use chumsky::{prelude::*, text::keyword, Parser};

/// Lex text input into tokens for use by the parser. The lexer capture some extra information that
/// isn't required by the parser, so that the text can be later re-serialized without ruining the
/// user's formatting.
pub(super) fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
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
    let ident = text::ident().with_span(Token::Ident);

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
