use super::{Block, ErrorKind, Expr, Item, Name, Param, Span, Spanned, Spanner, Token, TokenKind};
use chumsky::{prelude::*, Stream};

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
pub(super) fn token_filter(tokens: Vec<Spanned<Token>>) -> impl Iterator<Item = Spanned<Token>> {
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

fn expr() -> impl Parser<Token, Expr, Error = ErrorKind> {
    recursive(|expr| {
        let list_parser = expr.repeated().then_ignore(end()).spanned();

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
    })
}

fn param() -> impl Parser<Token, Spanned<Param>, Error = ErrorKind> {
    let param_inner = filter_map(|span, token| match token {
        Token::Ident(ident) => Ok((ident, span)),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Ident],
            token.kind(),
            span,
        )),
    })
    .then_ignore(just(Token::Ctrl(':')))
    .then(expr())
    .then_ignore(end())
    .map(|(param, val)| Param { name: param, val })
    .spanned();

    filter_map(move |span, token| match token {
        Token::Param(tokens) => {
            let tokens = map_tokens(tokens, span, token_filter);
            param_inner.parse(tokens).map_err(ErrorKind::multiple)
        }
        token => Err(ErrorKind::expected(
            vec![TokenKind::Params],
            token.kind(),
            span,
        )),
    })
}

fn params() -> impl Parser<Token, Spanned<Vec<Spanned<Param>>>, Error = ErrorKind> {
    let params_parser = param()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .then_ignore(end());

    let params_exist = filter_map(move |span, token| match token {
        Token::Params(params) => Ok(params),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Params],
            token.kind(),
            span,
        )),
    });

    params_exist
        .or_not()
        .try_map(move |params, span| match params {
            Some(params) => {
                let tokens = map_tokens(params, span, token_filter);
                params_parser.parse(tokens).map_err(ErrorKind::multiple)
            }
            None => Ok(Vec::new()),
        })
        .spanned()
}

fn map() -> impl Parser<Token, Block, Error = ErrorKind> {
    let map_body_parser = param().repeated().spanned().then_ignore(end());

    filter_map(|span, token| match token {
        Token::Name(name @ Name::Unit) | Token::Name(name @ Name::Env) => Ok(name),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    })
    .then(params())
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
    .map(|((name, params), body)| Block::Map { name, params, body })
}

fn items() -> impl Parser<Token, Block, Error = ErrorKind> {
    let items_body_parser = filter_map(|span, token| match token {
        Token::Str(s) => Ok((s, span)),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Str],
            token.kind(),
            span,
        )),
    })
    .then(params())
    .map(|(item, params)| Item {
        name: item,
        args: params,
    })
    .repeated()
    .then_ignore(end())
    .spanned();

    filter_map(|span, token| match token {
        Token::Name(name @ Name::Packages) | Token::Name(name @ Name::Files) => Ok(name),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    })
    .then(params())
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
    })
}

fn routine() -> impl Parser<Token, Block, Error = ErrorKind> {
    let routine_content_parser = filter_map(|span, token| match token {
        Token::Str(s) => Ok(s),
        _ => Err(ErrorKind::expected(
            vec![TokenKind::Str],
            token.kind(),
            span,
        )),
    })
    .then_ignore(end());

    filter_map(|span, token| match token {
        Token::Name(name @ Name::Deploy)
        | Token::Name(name @ Name::Remove)
        | Token::Name(name @ Name::Capture) => Ok(name),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    })
    .then(params())
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
    })
}

fn block_parser() -> impl Parser<Token, Block, Error = ErrorKind> {
    let other = filter_map(|span, token| match token {
        Token::Name(name @ Name::Other(_)) => Err(ErrorKind::UnexpectedBlock(span, name)),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    });

    choice((map(), items(), routine(), other))
}

pub(super) fn parser() -> impl Parser<(usize, Token), Vec<Option<Block>>, Error = ErrorKind> {
    let block_parser = block_parser().then_ignore(end());

    any()
        .validate(move |token, span, emit| match token {
            (n, Token::Block(tokens)) => {
                let mut stream = map_tokens(tokens, span, token_filter);
                let first = stream.fetch_tokens().next().expect("Found empty block");

                // Check if the order of the blocks is correct
                match first {
                    (Token::Name(name), name_span) => {
                        // The first block should be a topic declaration
                        if name != Name::Unit && n == 0 {
                            emit(ErrorKind::ExpectedUnit(name_span.clone(), name.clone()));
                        }

                        // The topic block should be the first in file
                        if name == Name::Unit && n > 0 {
                            emit(ErrorKind::MisplacedUnit(name_span.clone()));
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

#[cfg(test)]
mod tests {
    use crate::Params;

    use super::*;

    test_parsers! {
        expr {
            [Token::List(vec![
                (Token::Str("Hello".into()), 0..5),
                (Token::Space(" ".into()), 5..6),
                (Token::Var("var".into()), 6..9),
            ])] => Expr::List((
                vec![
                    Expr::String(("Hello".into(), 0..5)),
                    Expr::Variable(("var".into(), 6..9)),
                ],
                0..9
            ))
        }
        param {
            [
                Token::Ident("foo".into()),
                Token::Ctrl(':'),
                Token::Str("Bar".into()),
            ] => (
                Param {
                    name: ("foo".into(), 0..1),
                    val: Expr::String(("Bar".into(), 2..3)),
                },
                0..3
            )
        }
        params {
            [
                Token::Params(vec![
                    (
                        Token::Param(vec![
                            (Token::Comment("# A comment".into()), 0..1),
                            (Token::Space(" ".into()), 1..2),
                            (Token::Ident("foo".into()), 2..3),
                            (Token::Space(" ".into()), 3..4),
                            (Token::Ctrl(':'), 4..5),
                            (Token::Space(" ".into()), 5..6),
                            (Token::Bool(true), 6..7),
                        ]),
                        0..7
                    ),
                    (Token::Line, 7..8),
                    (Token::Ctrl(','), 8..9),
                    (
                        Token::Param(vec![
                            (Token::Ident("moo".into()), 9..10),
                            (Token::Str("bar".into()), 11..12),
                        ]),
                        9..12
                    ),
                ])
            ] => (
                vec![
                    (Param { name: ("foo".into(), 2..3), val: Expr::Bool((true, 6..7)) }, 2..7),
                    (Param { name: ("moo".into(), 9..10), val: Expr::String(("bar".into(), 11..12)) }, 9..12 ),
                ],
                0..1,
            )
        }
        map {}
        items {}
        reoutine {}
    }
}
