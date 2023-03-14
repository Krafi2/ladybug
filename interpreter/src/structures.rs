use super::{Arg, Span, Type, Value};
use crate::eval::Ctx;
use ariadne::{Color, Fmt, ReportKind};
use common::rel_path::RelPath;
use parser::{Ident, Spanned};
use std::path::PathBuf;

#[derive(Debug)]
pub(super) enum ParamError {
    Unused {
        span: Span,
        name: Ident,
    },
    NotFound {
        span: Span,
        name: &'static str,
        /// `true` if there was an argument block to parse
        has_args: bool,
    },
}

report! {
    ParamError {
        ParamError::Unused { span, name } => {
            report(ReportKind::Error, span.start);
            message("Unknown argument '{name}'");
            label(span, Color::Red, "Unknown argument");
        }
        ParamError::NotFound { span, name, has_args } => {
            report(ReportKind::Error, span.start);
            message("Argument '{name}' not found");
            label(span, Color::Red, "Expected argument '{}'", name.fg(Color::Red));
            @if !has_args;
            help("Consider adding arguments: ({name}=...)");
        }
    }
}

#[derive(Debug)]
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
        err: super::ValueError,
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

pub struct Partial<T> {
    pub value: T,
    pub degraded: bool,
}

impl<T> Partial<T> {
    pub fn new(value: T, degraded: bool) -> Self {
        Self { value, degraded }
    }
    pub fn complete(value: T) -> Self {
        Self {
            value,
            degraded: false,
        }
    }

    pub fn degraded(value: T) -> Self {
        Self {
            value,
            degraded: true,
        }
    }

    pub fn map<O, F: FnOnce(T) -> Partial<O>>(self, f: F) -> Partial<O> {
        let new = f(self.value);
        Partial {
            value: new.value,
            degraded: self.degraded | new.degraded,
        }
    }

    pub fn is_degraded(&self) -> bool {
        self.degraded
    }

    pub fn to_value(self) -> T {
        self.value
    }
}

impl<T> Partial<Option<T>> {
    pub fn transpose(self) -> Option<Partial<T>> {
        match self.value {
            Some(value) => Some(Partial {
                value,
                degraded: self.degraded,
            }),
            None => None,
        }
    }
}

impl<T> std::ops::Deref for Partial<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

pub(super) trait FromValue: Sized {
    fn from_value(value: Spanned<Value>, ctx: &mut Ctx) -> Option<Partial<Self>>;
}

impl FromValue for String {
    fn from_value(value: Spanned<Value>, ctx: &mut Ctx) -> Option<Partial<Self>> {
        match value.inner {
            Value::String(s) => Ok(Partial::complete(s)),
            Value::Error(err) => Err(ConvertError::EvalErr {
                span: value.span,
                err,
            }),
            other => Err(ConvertError::TypeErr {
                span: value.span,
                expected: Type::String,
                found: other.get_type(),
            }),
        }
        .map_err(|err| {
            ctx.emit(err);
        })
        .ok()
    }
}

impl FromValue for PathBuf {
    fn from_value(value: Spanned<Value>, ctx: &mut Ctx) -> Option<Partial<Self>> {
        <String as FromValue>::from_value(value, ctx)
            .map(|s| s.map(|s| Partial::complete(PathBuf::from(s))))
    }
}

impl FromValue for bool {
    fn from_value(value: Spanned<Value>, ctx: &mut Ctx) -> Option<Partial<Self>> {
        match value.inner {
            Value::Bool(b) => Ok(Partial::complete(b)),
            Value::Error(err) => Err(ConvertError::EvalErr {
                span: value.span,
                err,
            }),
            other => Err(ConvertError::TypeErr {
                span: value.span,
                expected: Type::Bool,
                found: other.get_type(),
            }),
        }
        .map_err(|err| {
            ctx.emit(err);
        })
        .ok()
    }
}

impl<T: FromValue> FromValue for Vec<T> {
    fn from_value(value: Spanned<Value>, ctx: &mut Ctx) -> Option<Partial<Self>> {
        match value.inner {
            Value::List(vec) => Some(vec.into_iter().map(|value| T::from_value(value, ctx)).fold(
                Partial::complete(Vec::new()),
                |list, val| {
                    list.map(|mut list| match val {
                        Some(val) => val.map(|val| {
                            list.push(val);
                            Partial::complete(list)
                        }),
                        None => Partial::degraded(list),
                    })
                },
            )),
            Value::Error(err) => {
                ctx.emit(ConvertError::EvalErr {
                    span: value.span,
                    err,
                });
                None
            }
            other => {
                ctx.emit(ConvertError::TypeErr {
                    span: value.span,
                    expected: Type::String,
                    found: other.get_type(),
                });
                None
            }
        }
    }
}

impl FromValue for RelPath {
    fn from_value(value: Spanned<Value>, ctx: &mut Ctx) -> Option<Partial<Self>> {
        let span = value.span;
        PathBuf::from_value(value, ctx).and_then(|path| {
            path.map(|path| match RelPath::new(path, ctx.home_dir()) {
                Ok(relpath) => Partial::complete(Some(relpath)),
                Err(err) => {
                    ctx.emit(ConvertError::ValueErr {
                        span,
                        err: Box::new(err),
                    });
                    Partial::degraded(None)
                }
            })
            .transpose()
        })
    }
}

pub struct ParseStr<T>(pub T);

impl<T: std::str::FromStr> FromValue for ParseStr<T>
where
    T::Err: std::error::Error + 'static,
{
    fn from_value(value: Spanned<Value>, ctx: &mut Ctx) -> Option<Partial<Self>> {
        let span = value.span;
        String::from_value(value, ctx).and_then(|string| {
            string
                .map(|string| match T::from_str(&string) {
                    Ok(t) => Partial::complete(Some(ParseStr(t))),
                    Err(err) => {
                        ctx.emit(ConvertError::ValueErr {
                            span,
                            err: Box::new(err),
                        });
                        Partial::degraded(None)
                    }
                })
                .transpose()
        })
    }
}

impl<T: FromValue> FromValue for Spanned<T> {
    fn from_value(value: Spanned<Value>, ctx: &mut Ctx) -> Option<Partial<Self>> {
        let val_span = value.span;
        T::from_value(value, ctx).map(|t| t.map(|t| Partial::complete(Spanned::new(t, val_span))))
    }
}

#[derive(Debug, thiserror::Error)]
enum CommandError {
    #[error("Expected at least one element")]
    Empty,
}

impl FromValue for common::command::Command {
    fn from_value(value: Spanned<Value>, ctx: &mut Ctx) -> Option<Partial<Self>> {
        let span = value.span;
        Vec::<String>::from_value(value, ctx).and_then(|vec| {
            let is_degraded = vec.is_degraded();
            vec.map(|mut vec| {
                if vec.is_empty() {
                    // Only emit an error when it's actually empty
                    if !is_degraded {
                        ctx.emit(ConvertError::ValueErr {
                            span,
                            err: Box::new(CommandError::Empty),
                        });
                    }
                    Partial::degraded(None)
                } else {
                    let cmd = vec.remove(0);
                    Partial::complete(Some(common::command::Command::new(cmd, vec)))
                }
            })
            .transpose()
        })
    }
}

pub(crate) enum ParseResult<T> {
    Ok(T),
    Degraded(T),
    ExpectedValue,
    Failed,
}

impl<T> From<Partial<T>> for ParseResult<T> {
    fn from(partial: Partial<T>) -> Self {
        if partial.is_degraded() {
            Self::Degraded(partial.value)
        } else {
            Self::Ok(partial.value)
        }
    }
}

// pub(crate) struct ParamCtx<'a> {
//     ctx: Ctx<'a>,
//     has_args: bool,
// }

// impl<'a> ParamCtx<'a> {
//     pub(crate) fn parse_val<T: FromValue>(&mut self, val: Value) -> Option<T> {
//         T::from_value(val, &mut self.ctx)
//     }

//     pub (crate) fn not_found(&mut self, )
// }

pub(super) trait ParseParam: Sized {
    fn parse(value: Option<Spanned<Value>>, ctx: &mut Ctx) -> ParseResult<Self>;
}

impl<T: FromValue> ParseParam for T {
    fn parse(value: Option<Spanned<Value>>, ctx: &mut Ctx) -> ParseResult<Self> {
        match value {
            Some(value) => match T::from_value(value, ctx) {
                Some(t) => t.into(),
                None => ParseResult::Failed,
            },
            None => ParseResult::ExpectedValue,
        }
    }
}

impl<T: FromValue> ParseParam for Option<T> {
    fn parse(value: Option<Spanned<Value>>, ctx: &mut Ctx) -> ParseResult<Self> {
        match value {
            Some(value) => match T::from_value(value, ctx) {
                Some(t) => t.map(|t| Partial::complete(Some(t))).into(),
                None => ParseResult::Failed,
            },
            None => ParseResult::Ok(None),
        }
    }
}

pub(crate) trait FromArgs: Sized {
    fn from_args(args: super::Args, ctx: &mut Ctx) -> Option<Partial<Self>>;
}

pub(crate) trait RecoverFromArgs: Sized {
    fn recover_default(args: super::Args, ctx: &mut Ctx) -> Partial<Self>;
}

macro_rules! params {
    (__parse__ $span:ident, $name:expr, $value:expr, $context:expr, $field:ident, $kind:ty) => {
        let $field = <$kind as $crate::structures::ParseParam>::parse(
            $value,
            $context,
        );
    };
    ($where:vis $what:ident $name:ident { $($fvis:vis $field:ident : $kind:ty),* $(,)? }) => {
        $where $what $name { $($fvis $field : $kind),* }

        impl $name {
            fn parse_raw_params(args: $crate::Args, ctx: &mut $crate::Ctx) -> $crate::structures::Partial<($(Option<$kind>,)*)> {
                let names = [$(stringify!($field)),*];
                let mut matchmaker = $crate::structures::Matchmaker::new(&names, args.args);
                let mut values = vec![None; names.len()];
                let mut degraded = false;
                while let Some(res) = matchmaker.next() {
                    match res {
                        Ok((param, arg)) => values[param.0] = Some(arg.value),
                        Err(err) => match err {
                            $crate::structures::MatchError::Unused(arg) => {
                                ctx.emit(
                                    $crate::structures::ParamError::Unused {
                                        span: arg.name.span,
                                        name: arg.name.inner,
                                    }
                                );
                                degraded = true;
                            },
                            // If this really is an error, it will be reported in the parsing stage
                            $crate::structures::MatchError::NotFound(_) => (),
                        }
                    }
                }

                #[allow(unused_variables, unused_mut)]
                let (mut names, mut values) = (names.into_iter(), values.into_iter());
                $(
                    let $field = {
                        let value = values.next().unwrap();
                        let name = names.next().unwrap();
                        match <$kind as $crate::structures::ParseParam>::parse(
                            value,
                            ctx,
                        ) {
                            $crate::structures::ParseResult::Ok(val) => Some(val),
                            $crate::structures::ParseResult::Degraded(val) => {
                                degraded = true;
                                Some(val)
                            }
                            $crate::structures::ParseResult::ExpectedValue => {
                                ctx.emit($crate::structures::ParamError::NotFound {
                                    span: args.span,
                                    name,
                                    has_args: args.accurate_span,
                                });
                                degraded = true;
                                None
                            }
                            $crate::structures::ParseResult::Failed => {
                                degraded = true;
                                None
                            }
                        }
                    };
                )*
                $crate::structures::Partial::new(
                    ($($field,)*),
                    degraded,
                )
            }
        }

        impl $crate::structures::FromArgs for $name {
            fn from_args(args: $crate::Args, ctx: &mut $crate::Ctx) -> Option<$crate::structures::Partial<Self>> {
                let values = Self::parse_raw_params(args, ctx);
                #[allow(unused_variables, unused_mut)]
                let mut degraded = values.is_degraded();
                let ($($field,)*) = values.to_value();

                let out = Self { $($field: $field?,)* };
                Some($crate::structures::Partial::new(out, degraded))
            }
        }

        impl $crate::structures::RecoverFromArgs for $name
            where $($kind: Default),*
        {
            fn recover_default(args: $crate::Args, ctx: &mut $crate::Ctx) -> $crate::structures::Partial<Self> {
                Self::parse_raw_params(args, ctx).map(|($($field,)*)| {
                    let out = Self { $( $field: $field.unwrap_or_default() ),* };
                    $crate::structures::Partial::complete(out)
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
