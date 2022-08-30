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

fn block_parser() -> impl Parser<Token, Block, Error = ErrorKind> {
    // They said that I should flatten the input and I am starting to see why, but it WORKS!

    // # Expr

    let expr = recursive(|expr| {
        let list_parser = expr.repeated().spanned();

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
    });

    // # Param

    let param = filter_map(|span, token| match token {
        Token::Ident(ident) => Ok((ident, span)),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Ident],
            token.kind(),
            span,
        )),
    })
    .then_ignore(just(Token::Ctrl(':')))
    .then(expr)
    .map(|(param, val)| Param { name: param, val })
    .spanned();

    let params_iner_parser = param
        .clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing();

    let params_parser = filter_map(move |span, token| match token {
        Token::Param(tokens) => {
            let tokens = map_tokens(tokens, span, token_filter);
            params_iner_parser
                .parse(tokens)
                .map_err(ErrorKind::multiple)
        }
        token => Err(ErrorKind::expected(
            vec![TokenKind::Params],
            token.kind(),
            span,
        )),
    });

    let params = filter_map(move |span, token| match token {
        Token::Params(params) => {
            let tokens = map_tokens(params, span, token_filter);
            params_parser.parse(tokens).map_err(ErrorKind::multiple)
        }
        token => Err(ErrorKind::expected(
            vec![TokenKind::Params],
            token.kind(),
            span,
        )),
    })
    .or_not()
    .map(Option::unwrap_or_default)
    .spanned();

    // # Map

    let map_body_parser = param.clone().repeated().spanned();

    let map = filter_map(|span, token| match token {
        Token::Name(name @ Name::Unit) | Token::Name(name @ Name::Env) => Ok(name),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    })
    .then(params.clone())
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
    .map(|((name, params), body)| Block::Map { name, params, body });

    // # Items

    let items_body_parser = filter_map(|span, token| match token {
        Token::Str(s) => Ok((s, span)),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Str],
            token.kind(),
            span,
        )),
    })
    .then(params.clone())
    .map(|(item, params)| Item {
        name: item,
        args: params,
    })
    .repeated()
    .spanned();

    let items = filter_map(|span, token| match token {
        Token::Name(name @ Name::Packages) | Token::Name(name @ Name::Files) => Ok(name),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    })
    .then(params.clone())
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
    });

    // # Routines

    let routine_content_parser = filter_map(|span, token| match token {
        Token::Str(s) => Ok(s),
        _ => Err(ErrorKind::expected(
            vec![TokenKind::Str],
            token.kind(),
            span,
        )),
    });

    let routines = filter_map(|span, token| match token {
        Token::Name(name @ Name::Deploy)
        | Token::Name(name @ Name::Remove)
        | Token::Name(name @ Name::Capture) => Ok(name),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    })
    .then(params.clone())
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
    });

    // # Other handling

    let other = filter_map(|span, token| match token {
        Token::Name(name @ Name::Other(_)) => Err(ErrorKind::UnexpectedBlock(span, name)),
        token => Err(ErrorKind::expected(
            vec![TokenKind::Name],
            token.kind(),
            span,
        )),
    });

    choice((map, items, routines, other))
}

pub(super) fn parser() -> impl Parser<(usize, Token), Vec<Option<Block>>, Error = ErrorKind> {
    let block_parser = block_parser();

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
