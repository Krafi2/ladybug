use chumsky::prelude::*;

use crate::{span::Spanner, Span, Spanned, Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Error {
    UnexpectedToken(Span, Vec<Option<TokenKind>>, Option<TokenKind>),
    InvalidIdent(Span, String),
}

impl Error {
    fn expected(expected: Vec<TokenKind>, found: TokenKind, span: Span) -> Self {
        Self::UnexpectedToken(
            span,
            expected.into_iter().map(Option::Some).collect(),
            Some(found),
        )
    }
}

impl chumsky::error::Error<Token> for Error {
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
                .map(|t| t.as_ref().map(Token::kind).unwrap_or(TokenKind::End))
                .collect(),
            found.as_ref().map(Token::kind).unwrap_or(TokenKind::End),
            span,
        )
    }

    fn with_label(self, _label: Self::Label) -> Self {
        unimplemented!()
    }

    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (
                Self::UnexpectedToken(span, mut expected1, found),
                Self::UnexpectedToken(_, expected2, _),
            ) => {
                expected1.extend_from_slice(&expected2);
                Self::UnexpectedToken(span, expected1, found)
            }
            (err, Self::UnexpectedToken(_, _, _)) => err,
            (Self::UnexpectedToken(_, _, _), err) => err,
            (err1, _) => err1,
        }
    }
}

trait MyParser<O>: Parser<Token, O, Error = Error> + Clone {}
impl<O, T: Parser<Token, O, Error = Error> + Clone> MyParser<O> for T {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Variable(Ident),
    String(String),
    Bool(bool),
    List(Vec<Spanned<Expr>>),
    Map(Vec<Spanned<Param>>),
    Code(String),
    Item(Spanned<Ident>, Spanned<Params>),
}

pub enum Body {
    Map(Vec<Spanned<Param>>),
    List(Vec<Spanned<Expr>>),
    Code(String),
}

pub struct Block {
    pub ident: Spanned<Ident>,
    pub params: Option<Spanned<Params>>,
    pub body: Spanned<Body>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Spanned<Ident>,
    pub val: Spanned<Expr>,
}

type Params = Vec<Spanned<Param>>;

fn ident() -> impl MyParser<Ident> {
    filter_map(|span, token| match token {
        Token::IdentOrStr(ident) => {
            for (i, c) in ident.chars().enumerate() {
                if !c.is_alphabetic() && (c.is_numeric() && i == 0) {
                    return Err(Error::InvalidIdent(span, ident));
                }
            }
            Ok(Ident(ident))
        }
        Token::Ident(ident) => Ok(Ident(ident)),
        token => Err(Error::expected(vec![TokenKind::Ident], token.kind(), span)),
    })
}

fn param(expr: impl MyParser<Expr>) -> impl MyParser<Param> {
    ident()
        .spanned()
        .then_ignore(just(Token::Ctrl(':')))
        .then(expr.spanned())
        .map(|(name, val)| Param { name, val })
}

fn params(expr: impl MyParser<Expr>) -> impl MyParser<Params> {
    param(expr)
        .spanned()
        .repeated()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
}

fn list(expr: impl MyParser<Expr>) -> impl MyParser<Vec<Spanned<Expr>>> {
    expr.spanned()
        .repeated()
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
}

fn map(expr: impl MyParser<Expr>) -> impl MyParser<Vec<Spanned<Param>>> {
    param(expr)
        .spanned()
        .repeated()
        .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
}

fn code() -> impl MyParser<String> {
    filter_map(|span, token| match token {
        Token::Code(code) => Ok(code),
        other => Err(Error::expected(vec![TokenKind::Code], other.kind(), span)),
    })
}

fn expr() -> impl MyParser<Expr> {
    recursive(|expr| {
        let list = list(expr.clone()).map(Expr::List);
        let map = map(expr.clone()).map(Expr::Map);
        let code = code().map(Expr::Code);
        let item = ident()
            .spanned()
            .then(params(expr.clone()).spanned())
            .map(|(ident, params)| Expr::Item(ident, params));

        let simple_expr = filter_map(move |span, token| match token {
            Token::Str(s) | Token::DelimStr(s) => Ok(Expr::String(s)),
            Token::Bool(b) => Ok(Expr::Bool(b)),
            Token::Var(var) => Ok(Expr::Variable(Ident(var))),
            token => Err(Error::expected(
                vec![TokenKind::Str, TokenKind::Bool, TokenKind::Var],
                token.kind(),
                span,
            )),
        });
        choice((list, map, code, item, simple_expr))
    })
}

fn body(expr: impl MyParser<Expr>) -> impl MyParser<Body> {
    let list = list(expr.clone()).map(Body::List);
    let map = map(expr).map(Body::Map);
    let code = code().map(Body::Code);
    choice((list, map, code))
}

pub(crate) fn parser() -> impl Parser<Token, Vec<Spanned<Block>>, Error = Error> {
    let expr = expr();
    let ident = ident();
    let params = params(expr.clone());
    let body = body(expr);
    ident
        .spanned()
        .then(params.spanned().or_not())
        .then(body.spanned())
        .map(|((ident, params), body)| Block {
            ident,
            params,
            body,
        })
        .spanned()
        .repeated()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    //TODO: Implement tests
    test_parsers! {
        @expected: std::convert::identity;
        @output: std::convert::identity;

        ident {}
        param {}
        params {}
        list {}
        map {}
        code {}
        expr {}
        body {}
    }
}
