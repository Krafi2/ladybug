use super::{Arg, Span, Type, Value};
use crate::eval::Ctx;
use ariadne::{Color, Fmt, ReportKind};
use common::rel_path::RelPath;
use std::path::PathBuf;

pub(super) enum ParamError {
    Unused { span: Span, name: String },
    NotFound { span: Span, name: &'static str },
}

report! {
    ParamError {
        ParamError::Unused { span, name } => {
            report(ReportKind::Error, span.start);
            message("Unknown argument '{name}'");
            label(span, Color::Red, "Unknown argument");
        }
        ParamError::NotFound { span, name } => {
            report(ReportKind::Error, span.start);
            message("Argument '{name}' not found");
            label(span, Color::Red, "Expected argument '{}'", name.fg(Color::Red));
        }
    }
}

pub(super) enum ConvertError {
    ValueErr {
        span: Span,
        err: Box<dyn std::error::Error>,
    },
    TypeErr {
        span: Span,
        expected: Type,
        found: Type,
    },
    EvalErr {
        span: Span,
        err: super::EvalError,
    },
}

report! {
    ConvertError {
        ConvertError::ValueErr { span, err } => {
            report(ReportKind::Error, span.start);
            message("Value Error");
            label(span, Color::Red, "{}", err);
        }
        ConvertError::TypeErr { span, expected, found } => {
            report(ReportKind::Error, span.start);
            message("Type Error");
            label(span, Color::Red, "Expected {expected} but found {found}");
        }
        ConvertError::EvalErr { span, err } => {
            delegate((err, span));
        }
    }
}

pub(super) trait FromValue: Sized {
    fn from_value(value: Value, ctx: &mut Ctx) -> Result<Self, ()>;
}

impl FromValue for String {
    fn from_value(value: Value, ctx: &mut Ctx) -> Result<Self, ()> {
        match value {
            Value::String((s, _)) => Ok(s),
            Value::Error((err, span)) => Err(ConvertError::EvalErr { span, err }),
            other => Err(ConvertError::TypeErr {
                span: other.span(),
                expected: Type::String,
                found: other.get_type(),
            }),
        }
        .map_err(|err| {
            ctx.emit(err);
        })
    }
}

impl FromValue for PathBuf {
    fn from_value(value: Value, ctx: &mut Ctx) -> Result<Self, ()> {
        <String as FromValue>::from_value(value, ctx).map(|s| PathBuf::from(s))
    }
}

impl FromValue for bool {
    fn from_value(value: Value, ctx: &mut Ctx) -> Result<Self, ()> {
        match value {
            Value::Bool((b, _)) => Ok(b),
            Value::Error((err, span)) => Err(ConvertError::EvalErr { span, err }),
            other => Err(ConvertError::TypeErr {
                span: other.span(),
                expected: Type::String,
                found: other.get_type(),
            }),
        }
        .map_err(|err| {
            ctx.emit(err);
        })
    }
}

impl<T: FromValue> FromValue for Vec<T> {
    fn from_value(value: Value, ctx: &mut Ctx) -> Result<Self, ()> {
        match value {
            Value::List((vec, _)) => vec.into_iter().map(|value| T::from_value(value, ctx)).fold(
                Ok(Vec::new()),
                |state, v| match (state, v) {
                    (Ok(mut list), Ok(v)) => {
                        list.push(v);
                        Ok(list)
                    }
                    _ => Err(()),
                },
            ),
            Value::Error((err, span)) => {
                ctx.emit(ConvertError::EvalErr { span, err });
                Err(())
            }
            other => {
                ctx.emit(ConvertError::TypeErr {
                    span: other.span(),
                    expected: Type::String,
                    found: other.get_type(),
                });
                Err(())
            }
        }
    }
}

impl FromValue for RelPath {
    fn from_value(value: Value, ctx: &mut Ctx) -> Result<Self, ()> {
        match value {
            Value::String((string, span)) => {
                RelPath::new(string.into(), ctx.home_dir()).map_err(|err| ConvertError::ValueErr {
                    span,
                    err: Box::new(err),
                })
            }
            Value::Error((err, span)) => Err(ConvertError::EvalErr { span, err }),
            other => Err(ConvertError::TypeErr {
                span: other.span(),
                expected: Type::String,
                found: other.get_type(),
            }),
        }
        .map_err(|err| {
            ctx.emit(err);
        })
    }
}

pub struct ParseStr<T>(pub T);

impl<T: std::str::FromStr> FromValue for ParseStr<T>
where
    T::Err: std::error::Error + 'static,
{
    fn from_value(value: Value, ctx: &mut Ctx) -> Result<Self, ()> {
        match value {
            Value::String((s, span)) => {
                T::from_str(&s)
                    .map(ParseStr)
                    .map_err(|err| ConvertError::ValueErr {
                        span,
                        err: Box::new(err),
                    })
            }
            Value::Error((err, span)) => Err(ConvertError::EvalErr { span, err }),
            other => Err(ConvertError::TypeErr {
                span: other.span(),
                expected: Type::String,
                found: other.get_type(),
            }),
        }
        .map_err(|err| {
            ctx.emit(err);
        })
    }
}

impl<T: FromValue> FromValue for (T, Span) {
    fn from_value(value: Value, ctx: &mut Ctx) -> Result<Self, ()> {
        let val_span = value.span();
        T::from_value(value, ctx).map(|t| (t, val_span))
    }
}

pub(super) trait ParseParam: Sized {
    fn parse(
        name: &'static str,
        value: Option<Arg>,
        span: &Span,
        ctx: &mut Ctx,
    ) -> Result<Self, ()>;
}

impl<T: FromValue> ParseParam for T {
    fn parse(name: &'static str, arg: Option<Arg>, span: &Span, ctx: &mut Ctx) -> Result<Self, ()> {
        match arg {
            Some(arg) => T::from_value(arg.value, ctx),
            None => {
                ctx.emit(ParamError::NotFound {
                    span: span.clone(),
                    name,
                });
                Err(())
            }
        }
    }
}

impl<T: FromValue> ParseParam for Option<T> {
    fn parse(
        name: &'static str,
        arg: Option<Arg>,
        span: &Span,
        context: &mut Ctx,
    ) -> Result<Self, ()> {
        match arg {
            Some(arg) => T::from_value(arg.value, context).map(Some),
            None => Ok(None),
        }
    }
}

impl<T: ParseParam> ParseParam for Result<T, ()> {
    fn parse(
        name: &'static str,
        value: Option<Arg>,
        span: &Span,
        context: &mut Ctx,
    ) -> Result<Self, ()> {
        Ok(T::parse(name, value, span, context))
    }
}

pub(super) trait FromArgs: Sized {
    fn from_args(args: super::Args, ctx: &mut Ctx) -> Result<Self, ()>;
}

macro_rules! params {
    (count $first:ident , $($rest:ident,)*) => {
        1usize + params!(count $($rest,)*)
    };

    (count) => {0usize};

    (parse $span:ident, $names:ident, $params:ident, $context:ident, $field:ident : $kind:ty, $($field_rest:ident : $type_rest:ty,)*) => {
        let $field = <$kind as $crate::structures::ParseParam>::parse(
            $names.next().expect("Unexpected end of iterator"),
            $params.next().expect("Unexpected end of iterator"),
            &$span,
            $context,
        );
        params!{parse $span, $names, $params, $context, $($field_rest : $type_rest,)*}
    };

    (parse $span:ident, $names:ident, $params:ident, $context:ident,) => {};

    ($where:vis $what:ident $name:ident { $($field:ident : $kind:ty),* $(,)? }) => {
        $where $what $name { $($field : $kind),* }

        impl $crate::structures::FromArgs for $name {
            fn from_args(args: $crate::Args, context: &mut crate::Ctx)
                    -> std::result::Result<Self, ()> {
                #![allow(unused)]
                let names = [$(stringify!($field)),*];
                let span = args.span;
                let mut matchmaker = $crate::structures::Matchmaker::new(&names, args.args);
                let mut params = vec![None; params!(count $($field,)*)];
                let mut is_err = false;
                while let Some(res) = matchmaker.next() {
                    match res {
                        Ok((param, arg)) => params[param.0] = Some(arg),
                        Err(err) => match err {
                            $crate::structures::MatchError::Unused(arg) => {
                                context.emit(
                                    $crate::structures::ParamError::Unused {
                                        span: arg.name.1,
                                        name: arg.name.0,
                                    }
                                );
                                is_err = true;
                            },
                            // If this really is an error, it will be reported in the parsing stage
                            $crate::structures::MatchError::NotFound(_) => (),
                        }
                    }
                }
                let mut names = names.into_iter();
                let mut params = params.into_iter();
                params!{parse span, names, params, context, $($field : $kind,)*}

                if is_err {
                    return Err(());
                }

                Ok(Self {
                    $($field: $field?,)*
                })
            }
        }
    };
}

pub(super) enum MatchError {
    Unused(Arg),
    NotFound(ParamId),
}

#[derive(Debug, Clone, Copy)]
pub(super) struct ParamId(pub usize);

pub struct Matchmaker {
    args: std::vec::IntoIter<Arg>,
    params: std::vec::IntoIter<(ParamId, &'static str)>,
}

/// Not an Iterator so it doesn't leak private type
impl Matchmaker {
    pub(super) fn new(params: &[&'static str], mut args: Vec<Arg>) -> Self {
        args.sort_unstable_by(|a, b| a.name.0.cmp(&b.name.0));

        let mut params = params
            .iter()
            .copied()
            .enumerate()
            .map(|(i, p)| (ParamId(i), p))
            .collect::<Vec<_>>();
        params.sort_unstable_by(|a, b| a.1.cmp(&b.1));

        Self {
            args: args.into_iter(),
            params: params.into_iter(),
        }
    }
}

impl Matchmaker {
    pub(super) fn next(&mut self) -> Option<Result<(ParamId, Arg), MatchError>> {
        match (self.args.as_slice().get(0), self.params.as_slice().get(0)) {
            (Some(arg), Some(param)) => match arg.name.0.as_str().cmp(param.1) {
                std::cmp::Ordering::Less => {
                    let arg = self.args.next().unwrap();
                    Some(Err(MatchError::Unused(arg)))
                }
                std::cmp::Ordering::Equal => {
                    let arg = self.args.next().unwrap();
                    let param = self.params.next().unwrap();
                    Some(Ok((param.0, arg)))
                }
                std::cmp::Ordering::Greater => {
                    Some(Err(MatchError::NotFound(self.params.next().unwrap().0)))
                }
            },
            (Some(_), None) => {
                let arg = self.args.next().unwrap();
                Some(Err(MatchError::Unused(arg)))
            }
            (None, Some(_)) => Some(Err(MatchError::NotFound(self.params.next().unwrap().0))),
            (None, None) => None,
        }
    }
}
