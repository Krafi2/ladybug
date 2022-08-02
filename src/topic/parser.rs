#![allow(dead_code)]

use chumsky::{
    prelude::*,
    text::{keyword, Character},
};

#[derive(Debug, Clone)]
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

fn parser() -> impl Parser<char, Vec<Block>, Error = Simple<char>> {
    let reserved_chars = [
        ':', ',', ';', '(', ')', '[', ']', '{', '}', '"', '\'', '#', '\n', '$',
    ];
    let space_chars = one_of(" \t").repeated();

    let string = none_of('"')
        .repeated()
        .delimited_by(just('\"'), just('\"'))
        .or(filter(move |c: &char| !reserved_chars.contains(c)).repeated())
        .padded_by(space_chars.clone())
        .collect()
        .map(Expression::String)
        .labelled("string");

    let variable = just('$')
        .ignore_then(text::ident())
        .padded_by(space_chars.clone())
        .map(Expression::Variable)
        .labelled("variable");

    let list_string = none_of('"')
        .repeated()
        .delimited_by(just('\"'), just('\"'))
        .or(
            filter(move |c: &char| !reserved_chars.contains(c) && !c.is_whitespace())
                .repeated()
                .at_least(1),
        )
        .padded_by(space_chars.clone())
        .collect()
        .map(Expression::String)
        .labelled("list_string");

    let list = recursive(|list| {
        variable
            .clone()
            .or(list)
            .or(list_string)
            .padded_by(space_chars.clone())
            .repeated()
            .delimited_by(just('['), just(']'))
            .padded_by(space_chars.clone())
            .collect()
            .map(Expression::List)
    })
    .labelled("list");

    let expression = variable
        .clone()
        .or(list.clone())
        .or(string.clone())
        .labelled("expression");

    let comment = just('#')
        .ignore_then(take_until(text::newline()).ignored())
        .padded_by(space_chars.clone())
        .labelled("comment");

    let param = text::ident()
        .padded_by(space_chars.clone())
        .then_ignore(just(':'))
        .padded_by(space_chars.clone())
        .then(expression)
        .padded_by(space_chars.clone())
        .map(|(param, val)| Param { param, val })
        .labelled("param");

    let params = param
        .clone()
        .padded_by(text::whitespace())
        .separated_by(just(','))
        .allow_trailing()
        .padded_by(text::whitespace())
        .delimited_by(just('('), just(')'))
        .or_not()
        .map(|o| o.unwrap_or_default())
        .padded_by(space_chars.clone())
        .labelled("params");

    let map = param
        .clone()
        .padded_by(comment.clone().repeated())
        .separated_by(text::newline().repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .padded_by(comment.clone().repeated())
        .delimited_by(just('{'), just('}'))
        .labelled("map");

    let topic = text::keyword("topic")
        .ignore_then(params.clone())
        .then(map.clone())
        .map(|(params, body)| Block::Topic { params, body })
        .labelled("topic");

    let env = text::keyword("env")
        .ignore_then(params.clone())
        .then(map.clone())
        .map(|(params, body)| Block::Env { params, body })
        .labelled("env");

    let item = none_of('"')
        .repeated()
        .delimited_by(just('\"'), just('\"'))
        .or(filter(|c: &char| c.is_alphanumeric() || ['_', '-', '.'].contains(c)).repeated())
        .collect()
        .padded_by(space_chars.clone())
        .labelled("item");

    let items = item
        .then(params.clone())
        .map(|(name, params)| Package { name, params })
        .padded_by(comment.clone().repeated())
        .padded_by(space_chars.clone())
        .separated_by(text::newline().repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .padded_by(comment.clone().repeated())
        .delimited_by(just('{'), just('}'))
        .labelled("items");

    let files = keyword("files")
        .ignore_then(params.clone())
        .then(items.clone())
        .map(|(params, files)| Block::Files { params, files })
        .labelled("files");

    let packages = keyword("packages")
        .ignore_then(params.clone())
        .then(items.clone())
        .map(|(params, packages)| Block::Packages { params, packages })
        .labelled("packages");

    // TODO: This may have poor performance?
    // Capture all input between curly braces.
    // This has to match braces.
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
    .labelled("content")
    .collect::<String>();

    let deploy = keyword("deploy")
        .ignore_then(params.clone())
        .then(content.clone())
        .map(|(params, content)| Block::Deploy { params, content })
        .labelled("deploy");

    let remove = keyword("remove")
        .ignore_then(params.clone())
        .then(content.clone())
        .map(|(params, content)| Block::Remove { params, content })
        .labelled("remove");

    let capture = keyword("capture")
        .ignore_then(params.clone())
        .then(content.clone())
        .map(|(params, content)| Block::Capture { params, content })
        .labelled("capture");

    topic
        .padded()
        .padded_by(comment.clone().repeated())
        .chain(
            choice((env, files, packages, deploy, remove, capture))
                .padded()
                .padded_by(comment.clone().repeated())
                .repeated(),
        )
        .then_ignore(end())
}

fn main() {
    let input = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let out = parser().parse(input);
    println!("{:#?}", out);
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    #[test]
    fn parser() {
        let input = r#" topic { name: My } "#;
        //             env {
        //                 arch: x64
        //             }
        //
        //             packages(provider: zypper, bla: a) {
        //                 myapp(lock: true)
        //             }
        //
        //             files(method: link, conflicts: rename, source: ${arch}, target: there) {
        //                 hello.txt
        //                 hi.txt
        //             }
        //
        //             deploy(stdin: false, stdout: true, shell: [python -c %c]) {
        //                 echo "1" > ~/hello
        //             }
        //
        //             remove {
        //                 rm ~/hello
        //             }
        //
        //             capture {
        //                 save "hello"
        // }

        let ast = super::parser().parse(input);
        println!("{:#?}", ast);
    }
}
