use crate::{context::Context, rel_path::RelPath};

use super::{Arg, ArgId, Emitter, Span, Type, Value};

pub(super) enum ParamError {
    ValueErr {
        arg: ArgId,
        span: Span,
        err: Box<dyn std::error::Error>,
    },
    TypeErr {
        arg: ArgId,
        span: Span,
        expected: Type,
        found: Type,
    },
    Unused {
        arg: ArgId,
        span: Span,
    },
    NotFound {
        span: Span,
        name: &'static str,
    },
    EvalErr {
        span: Span,
        err: super::EvalError,
    },
}

pub(super) trait ParseParam: Sized {
    fn parse(
        span: &Span,
        name: &'static str,
        value: Option<(ArgId, Value)>,
        emitter: &mut Emitter,
        context: &Context,
    ) -> Result<Self, ()>;
}

impl ParseParam for String {
    fn parse(
        span: &Span,
        name: &'static str,
        value: Option<(ArgId, Value)>,
        emitter: &mut Emitter,
        _context: &Context,
    ) -> Result<Self, ()> {
        match value {
            Some((arg, value)) => match value {
                Value::String((s, _)) => Ok(s),
                Value::Error((err, span)) => Err(ParamError::EvalErr { span, err }),
                other => Err(ParamError::TypeErr {
                    arg,
                    span: other.span(),
                    expected: Type::String,
                    found: other.get_type(),
                }),
            },
            None => Err(ParamError::NotFound {
                span: span.clone(),
                name,
            }),
        }
        .map_err(|err| {
            emitter.emit(err);
        })
    }
}

impl ParseParam for bool {
    fn parse(
        span: &Span,
        name: &'static str,
        value: Option<(ArgId, Value)>,
        emitter: &mut Emitter,
        _context: &Context,
    ) -> Result<Self, ()> {
        match value {
            Some((arg, value)) => match value {
                Value::Bool((b, _)) => Ok(b),
                Value::Error((err, span)) => Err(ParamError::EvalErr { span, err }),
                other => Err(ParamError::TypeErr {
                    arg,
                    span: other.span(),
                    expected: Type::String,
                    found: other.get_type(),
                }),
            },
            None => Err(ParamError::NotFound {
                span: span.clone(),
                name,
            }),
        }
        .map_err(|err| {
            emitter.emit(err);
        })
    }
}

impl<T: ParseParam> ParseParam for Vec<T> {
    fn parse(
        span: &Span,
        name: &'static str,
        value: Option<(ArgId, Value)>,
        emitter: &mut Emitter,
        context: &Context,
    ) -> Result<Self, ()> {
        match value {
            Some((arg, value)) => match value {
                Value::List((vec, _)) => vec
                    .into_iter()
                    .map(|value| T::parse(span, "", Some((arg, value)), emitter, context))
                    .fold(Ok(Vec::new()), |state, v| match (state, v) {
                        (Ok(mut list), Ok(v)) => {
                            list.push(v);
                            Ok(list)
                        }
                        _ => Err(()),
                    }),
                Value::Error((err, span)) => {
                    emitter.emit(ParamError::EvalErr { span, err });
                    Err(())
                }
                other => {
                    emitter.emit(ParamError::TypeErr {
                        arg,
                        span: other.span(),
                        expected: Type::String,
                        found: other.get_type(),
                    });
                    Err(())
                }
            },
            None => {
                emitter.emit(ParamError::NotFound {
                    span: span.clone(),
                    name,
                });
                Err(())
            }
        }
    }
}

impl<T: ParseParam> ParseParam for Option<T> {
    fn parse(
        span: &Span,
        name: &'static str,
        value: Option<(ArgId, Value)>,
        emitter: &mut Emitter,
        context: &Context,
    ) -> Result<Self, ()> {
        match value {
            Some(value) => T::parse(span, name, Some(value), emitter, context).map(Some),
            None => Ok(None),
        }
    }
}

impl ParseParam for RelPath {
    fn parse(
        span: &Span,
        name: &'static str,
        value: Option<(ArgId, Value)>,
        emitter: &mut Emitter,
        context: &Context,
    ) -> Result<Self, ()> {
        match value {
            Some((arg, value)) => match value {
                Value::String((string, span)) => {
                    RelPath::new(string.into(), context).map_err(|err| ParamError::ValueErr {
                        arg,
                        span,
                        err: Box::new(err),
                    })
                }
                Value::Error((err, span)) => Err(ParamError::EvalErr { span, err }),
                other => Err(ParamError::TypeErr {
                    arg,
                    span: other.span(),
                    expected: Type::String,
                    found: other.get_type(),
                }),
            },
            None => Err(ParamError::NotFound {
                span: span.clone(),
                name,
            }),
        }
        .map_err(|err| {
            emitter.emit(err);
        })
    }
}

pub struct FromString<T>(pub T);

impl<T: std::str::FromStr> ParseParam for FromString<T>
where
    T::Err: std::error::Error + 'static,
{
    fn parse(
        span: &Span,
        name: &'static str,
        value: Option<(ArgId, Value)>,
        emitter: &mut Emitter,
        _context: &Context,
    ) -> Result<Self, ()> {
        match value {
            Some((arg, value)) => match value {
                Value::String((s, span)) => {
                    T::from_str(&s)
                        .map(FromString)
                        .map_err(|err| ParamError::ValueErr {
                            arg,
                            span,
                            err: Box::new(err),
                        })
                }
                Value::Error((err, span)) => Err(ParamError::EvalErr { span, err }),
                other => Err(ParamError::TypeErr {
                    arg,
                    span: other.span(),
                    expected: Type::String,
                    found: other.get_type(),
                }),
            },
            None => Err(ParamError::NotFound {
                span: span.clone(),
                name,
            }),
        }
        .map_err(|err| {
            emitter.emit(err);
        })
    }
}

impl<T: ParseParam> ParseParam for Result<T, ()> {
    fn parse(
        span: &Span,
        name: &'static str,
        value: Option<(ArgId, Value)>,
        emitter: &mut Emitter,
        context: &Context,
    ) -> Result<Self, ()> {
        Ok(T::parse(span, name, value, emitter, context))
    }
}

pub(super) trait FromArgs: Sized {
    fn from_args(
        args: super::Args,
        emitter: &mut super::Emitter,
        context: &Context,
    ) -> Result<Self, ()>;
}

macro_rules! params {
    (count $first:ident , $($rest:ident,)*) => {
        1usize + params!(count $($rest,)*)
    };

    (count) => {0usize};

    (parse $span:ident, $names:ident, $params:ident, $emitter:ident, $context:ident, $field:ident : $kind:ty, $($field_rest:ident : $type_rest:ty,)*) => {
        let $field = <$kind as $crate::unit::interpreter::structures::ParseParam>::parse(
            &$span,
            $names.next().expect("Unexpected end of iterator"),
            $params.next().expect("Unexpected end of iterator"),
            $emitter,
            $context,
        );
        params!{parse $span, $names, $params, $emitter, $context, $($field_rest : $type_rest,)*}
    };

    (parse $span:ident, $names:ident, $params:ident, $errors:ident, $context:ident,) => {};

    ($where:vis $what:ident $name:ident { $($field:ident : $kind:ty),* $(,)? }) => {
        $where $what $name { $($field : $kind),* }

        impl $crate::unit::interpreter::structures::FromArgs for $name {
            fn from_args(args: $crate::unit::interpreter::Args, emitter: &mut $crate::unit::interpreter::Emitter, context: &$crate::context::Context)
                    -> std::result::Result<Self, ()> {
                #![allow(unused)]
                let names = [$(stringify!($field)),*];
                let span = args.span().clone();
                let mut matchmaker = $crate::unit::interpreter::structures::Matchmaker::new(&names, args);
                let mut params = vec![None; params!(count $($field,)*)];
                let mut is_err = false;
                while let Some(res) = matchmaker.next() {
                    match res {
                        Ok((param, arg, value)) => params[param.0] = Some((arg, value)),
                        Err(err) => match err {
                            $crate::unit::interpreter::structures::MatchError::Unused(span, arg) => {
                                emitter.emit($crate::unit::interpreter::structures::ParamError::Unused{ span, arg });
                                is_err = true;
                            },
                            // If this really is an error, it will be reported in the parsing stage
                            $crate::unit::interpreter::structures::MatchError::NotFound(_) => (),
                        }
                    }
                }
                let mut names = names.into_iter();
                let mut params = params.into_iter();
                params!{parse span, names, params, emitter, context, $($field : $kind,)*}

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
    Unused(Span, ArgId),
    NotFound(ParamId),
}

#[derive(Debug, Clone, Copy)]
pub(super) struct ParamId(pub usize);

pub struct Matchmaker {
    args: std::vec::IntoIter<(ArgId, Arg)>,
    params: std::vec::IntoIter<(ParamId, &'static str)>,
}

/// Not an Iterator so it doesn't leak private type
impl Matchmaker {
    pub(super) fn new(params: &[&'static str], args: crate::unit::interpreter::Args) -> Self {
        let mut args = args.collect::<Vec<_>>();
        args.sort_unstable_by(|a, b| a.1.name.0.cmp(&b.1.name.0));

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
    pub(super) fn next(&mut self) -> Option<Result<(ParamId, ArgId, Value), MatchError>> {
        match (self.args.as_slice().get(0), self.params.as_slice().get(0)) {
            (Some(arg), Some(param)) => match arg.1.name.0.as_str().cmp(param.1) {
                std::cmp::Ordering::Less => {
                    let arg = self.args.next().unwrap();
                    Some(Err(MatchError::Unused(arg.1.name.1, arg.0)))
                }
                std::cmp::Ordering::Equal => {
                    let arg = self.args.next().unwrap();
                    let param = self.params.next().unwrap();
                    Some(Ok((param.0, arg.0, arg.1.value)))
                }
                std::cmp::Ordering::Greater => {
                    Some(Err(MatchError::NotFound(self.params.next().unwrap().0)))
                }
            },
            (Some(_), None) => {
                let arg = self.args.next().unwrap();
                Some(Err(MatchError::Unused(arg.1.name.1, arg.0)))
            }
            (None, Some(_)) => Some(Err(MatchError::NotFound(self.params.next().unwrap().0))),
            (None, None) => None,
        }
    }
}
