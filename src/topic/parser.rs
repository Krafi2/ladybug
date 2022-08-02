#![allow(dead_code)]

use chumsky::{
    prelude::*,
    text::{ident, keyword, Character},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Ctrl(Span<char>),
    Str(Span<String>),
    DelimStr(Span<String>),
    Var(Span<String>),
    List(Span<Vec<Token>>),
    Ident(Span<String>),
    Comment(Span<String>),
    Space(Span<String>),
    Line(Span<()>),
    Param(Span<Vec<Token>>),
    Params(Span<Vec<Token>>),
    Body(Span<Vec<Token>>),
    Block(Span<Vec<Token>>),
}

type Span<T> = spanner::Span<T, std::ops::Range<usize>>;
use spanner::Spanner;

/// A utility module for binding tokens with their spans
mod spanner {
    use chumsky::{
        combinator::{Map, MapWithSpan},
        Error, Parser,
    };

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Span<T, S>(T, S);

    pub trait Spanner<I: Clone, O>: Parser<I, O>
    where
        Self: Sized,
    {
        fn with_span<F, T>(
            self,
            f: F,
        ) -> Map<
            MapWithSpan<
                Self,
                fn(O, <Self::Error as Error<I>>::Span) -> Span<O, <Self::Error as Error<I>>::Span>,
                O,
            >,
            F,
            Span<O, <Self::Error as Error<I>>::Span>,
        >
        where
            F: Fn(Span<O, <Self::Error as Error<I>>::Span>) -> T,
        {
            self.map_with_span(Span as fn(_, _) -> _).map(f)
        }
    }

    impl<P, I: Clone, O> Spanner<I, O> for P where P: Parser<I, O> {}
}

/// Lex text input into tokens for use by the parser. The lexer capture some extra information that
/// isn't required by the parser, so that the text can be later re-serialized without ruining the
/// user's formatting.
fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let reserved_chars = [
        ':', ',', ';', '(', ')', '[', ']', '{', '}', '"', '\'', '#', '\n', '$',
    ];

    // Characters allowed as whitespace
    let space = one_of(" \t")
        .repeated()
        .at_least(1)
        .collect()
        .with_span(Token::Space);

    let line = text::newline().with_span(Token::Line);

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
    let expression = variable
        .clone()
        .or(list.clone())
        .or(spaced_string.clone())
        .labelled("expression");

    let ident = ident().with_span(Token::Ident);

    // Parameters are a parentheses-delimited, comma-separated list of params with optional
    // newlines in-between.
    let params = choice((
        one_of(":,").with_span(Token::Ctrl),
        ident.clone(),
        line.clone(),
        space.clone(),
        expression.clone(),
    ))
    .repeated()
    .at_least(1)
    .delimited_by(just('('), just(')'))
    .collect()
    .with_span(Token::Params);

    let map = keyword("topic")
        .to("topic".to_owned())
        .or(keyword("env").to("env".to_owned()))
        .with_span(Token::Ident)
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
                    .chain(just(':').with_span(Token::Ctrl))
                    .chain(space.clone().repeated())
                    .chain(expression.clone())
                    .collect()
                    .with_span(Token::Param))
                .repeated()
                .delimited_by(just('{'), just('}'))
                .with_span(Token::Body)
                .labelled("map"),
        )
        .collect()
        .with_span(Token::Block);

    let items = keyword("files")
        .to("files".to_owned())
        .or(keyword("packages").to("packages".to_owned()))
        .with_span(Token::Ident)
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
        .with_span(Token::Block);

    let content = recursive(|content| {
        just(vec!['{', '}'])
            .or(just('{')
                .chain(content.repeated().flatten())
                .chain(just('}')))
            .or(none_of("{}").repeated().at_least(1))
    })
    .repeated()
    .flatten()
    .collect()
    .with_span(Token::Str)
    .delimited_by(just('{'), just('}'))
    .map(|s| vec![s])
    .with_span(Token::Body)
    .labelled("content");

    let routine = ident
        .clone()
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
