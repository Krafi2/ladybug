#![allow(dead_code)]

use chumsky::{
    prelude::*,
    text::{ident, keyword, Character},
};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Ctrl(char),
    Str(String),
    DelimStr(String),
    Var(String),
    List(Vec<Token>),
    Ident(String),
    Comment(String),
    Space(String),
    Line,
    Param(Vec<Token>),
    Params(Vec<Token>),
    Body(Vec<Token>),
    Block(Vec<Token>),
}

/// Lex text input into tokens for use by the parser. The lexer capture some extra information that
/// isn't required by the parser, so that the text can be later re-serialized without ruining the
/// user's formatting.
fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let reserved_chars = [
        ':', ',', ';', '(', ')', '[', ']', '{', '}', '"', '\'', '#', '\n', '$',
    ];

    // Characters allowed as whitespace
    let space = one_of(" \t")
        .repeated()
        .at_least(1)
        .collect()
        .map(Token::Space);

    let line = text::newline().map(|_| Token::Line);

    // Comments start with `#` and end in a newline
    let comment = just('#')
        .chain(take_until(text::newline()).map(|(a, _)| a))
        .collect()
        .map(Token::Comment)
        .labelled("comment");

    // Variables are in the form `$identifier`
    let variable = just('$')
        .ignore_then(text::ident())
        .map(Token::Var)
        .labelled("variable");

    // A string can either be delimited by `"` or or be undelimited if it doesn't contain any
    // reserved characters. Some of the reserved characters could be allowed without parser
    // ambiguity, but the user might be confused by which characters are allowd, so we take a more
    // conservative approach.
    let spaced_string = none_of('"')
        .repeated()
        .delimited_by(just('\"'), just('\"'))
        .collect()
        .map(Token::DelimStr)
        .or(filter(move |c: &char| !reserved_chars.contains(c))
            .repeated()
            .at_least(1)
            .collect()
            .map(Token::Str))
        .labelled("string");

    // Strings inside lists are a bit more limited since they are delimited by spaces.
    let word_string = none_of('"')
        .repeated()
        .delimited_by(just('\"'), just('\"'))
        .collect()
        .map(Token::DelimStr)
        .or(
            filter(move |c: &char| !reserved_chars.contains(c) && !c.is_whitespace())
                .repeated()
                .at_least(1)
                .collect()
                .map(Token::Str),
        )
        .labelled("word_string");

    // Lists are delimited by `[]` and contain whitespace-separated expressions.
    let list = recursive(|list| {
        choice((list, variable.clone(), word_string.clone(), space.clone()))
            .repeated()
            .delimited_by(just('['), just(']'))
            .collect()
            .map(Token::List)
    })
    .labelled("list");

    // An expression can be either a string, a list, or a variable.
    let expression = variable
        .clone()
        .or(list.clone())
        .or(spaced_string.clone())
        .labelled("expression");

    let ident = ident().map(Token::Ident);

    // Parameters are a parentheses-delimited, comma-separated list of params with optional
    // newlines in-between.
    let params = choice((
        one_of(":,").map(Token::Ctrl),
        ident.clone(),
        line.clone(),
        space.clone(),
        expression.clone(),
    ))
    .repeated()
    .at_least(1)
    .delimited_by(just('('), just(')'))
    .collect()
    .map(Token::Params);

    let map = keyword("topic")
        .to("topic".to_owned())
        .or(keyword("env").to("env".to_owned()))
        .map(Token::Ident)
        .chain(space.clone().repeated())
        .chain(params.clone().repeated().at_most(1))
        .chain(space.clone().repeated())
        .chain(
            space
                .clone()
                .or(line.clone())
                .or(comment.clone())
                .or(ident
                    .clone()
                    .chain(space.clone().repeated())
                    .chain(just(':').map(Token::Ctrl))
                    .chain(space.clone().repeated())
                    .chain(expression.clone())
                    .collect()
                    .map(Token::Param))
                .repeated()
                .delimited_by(just('{'), just('}'))
                .map(Token::Body)
                .labelled("map"),
        )
        .collect()
        .map(Token::Block);

    let items = keyword("files")
        .to("files".to_owned())
        .or(keyword("packages").to("packages".to_owned()))
        .map(Token::Ident)
        .chain(space.clone().repeated())
        .chain(params.clone().repeated().at_most(1))
        .chain(space.clone().repeated())
        .chain::<Token, _, _>(
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
        .map(Token::Block);

    let content = recursive(|content| {
        just(vec!['{', '}'])
            .or(just('{')
                .chain(content.repeated().flatten())
                .chain(just('}')))
            .or(none_of("{}").repeated().at_least(1))
    })
    .repeated()
    .flatten()
    .delimited_by(just('{'), just('}'))
    .collect()
    .map(Token::Str)
    .map(|s| Token::Body(vec![s]))
    .labelled("content");

    let routine = ident
        .clone()
        .chain(space.clone().repeated())
        .chain(params.clone().repeated().at_most(1))
        .chain(space.clone().repeated())
        .chain(content)
        .collect()
        .map(Token::Block);

    choice((space, line, comment, map, items, routine))
        // .recover_with(skip_then_retry_until([]))
        .map_with_span(|tok, span| (tok, span))
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

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    #[test]
    fn lexer() {
        let input = std::fs::read_to_string("config_example").unwrap();
        let ast = super::lexer().parse(input);
        println!("{:#?}", ast);
    }

    // #[test]
    // fn parser() {
    //     let input = std::fs::read_to_string("config_example").unwrap();
    //     let ast = super::parser().parse(input);
    //     println!("{:#?}", ast);
    // }
}
