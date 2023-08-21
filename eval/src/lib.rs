mod error;
pub use error::{Error, IntoReport, IntoReportBoxed};

use ariadne::{Color, Fmt, ReportKind};
use common::rel_path::RelPath;
use parser::{span::AriadneSpan, Ident, Span, Spanned};

use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub enum ValueError {
    UnknownVar(Ident),
    VarError(Ident, Box<Spanned<ValueError>>),
}

report! {
    (ValueError, Span) {
        (ValueError::UnknownVar(var), span) => {
            report(ReportKind::Error, span.start);
            message("Undefined variable ${}", var.fg(Color::Red));
            label(span, Color::Red, "This variable is not defined");
        }
        (ValueError::VarError(var, _), span) => {
            report(ReportKind::Error, span.start);
            message("Encountered an error while evaluating variable ${}", var.fg(Color::Red));
            label(span, Color::Red, "Cannot evaluate this variable");
        }
    }
}

impl IntoReport for Spanned<ValueError> {
    fn into_report(self, filename: &str) -> ariadne::Report<AriadneSpan> {
        (self.inner, self.span).into_report(filename)
    }
}

#[derive(Debug)]
pub enum ConvertError {
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
        err: ValueError,
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

#[derive(Debug)]
pub enum ParamError {
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

pub type Env = std::collections::HashMap<Ident, Spanned<Value>>;

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Bool(bool),
    List(Vec<Spanned<Value>>),
    Map(Vec<Spanned<Arg>>),
    Code(String),
    Item(Spanned<Ident>, Args),
    Error(ValueError),
}

impl Value {
    pub fn from_expr(expr: parser::Expr, env: &Env) -> Self {
        match expr {
            parser::Expr::Variable(var) => match env.get(&var).cloned() {
                Some(v) => match v.inner {
                    Value::Error(e) => Value::Error(ValueError::VarError(
                        var,
                        Box::new(Spanned::new(e.clone(), v.span)),
                    )),
                    v => v,
                },
                None => Value::Error(ValueError::UnknownVar(var)),
            },
            parser::Expr::String(s) => Self::String(s),
            parser::Expr::Bool(b) => Self::Bool(b),
            parser::Expr::List(list) => Self::List(
                list.into_iter()
                    .map(|expr| expr.map(|expr| Self::from_expr(expr, env)))
                    .collect(),
            ),
            parser::Expr::Map(_) => todo!(),
            parser::Expr::Code(_) => todo!(),
            parser::Expr::Item(_, _) => todo!(),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Value::String(_) => Type::String,
            Value::Bool(_) => Type::Bool,
            Value::List(_) => Type::List,
            Value::Map(_) => Type::Map,
            Value::Code(_) => Type::Code,
            Value::Item(_, _) => Type::Item,
            Value::Error(_) => Type::Error,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    String,
    Bool,
    List,
    Map,
    Code,
    Item,
    Error,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::String => "String",
            Type::Bool => "Bool",
            Type::List => "List",
            Type::Map => "Map",
            Type::Code => "Code",
            Type::Item => "Item",
            Type::Error => "Error",
        };
        f.write_str(s)
    }
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: Spanned<Ident>,
    pub value: Spanned<Value>,
    pub span: Span,
}

impl Arg {
    fn from_expr(expr: Spanned<parser::Param>, env: &Env) -> Self {
        Arg {
            name: expr.inner.name,
            value: expr.inner.val.map(|val| Value::from_expr(val, env)),
            span: expr.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Args {
    pub args: Vec<Arg>,
    pub span: Span,
    /// If true, the args were present in the source code and the span is accurate.
    /// Otherwise were omitted and should be assumed empty. The provided span
    /// points to the space where args could be placed.
    pub accurate_span: bool,
}

impl Args {
    pub fn from_exprs(exprs: Spanned<Vec<Spanned<parser::Param>>>, env: &Env) -> Self {
        Self {
            args: exprs
                .inner
                .into_iter()
                .map(|arg| Arg::from_expr(arg, env))
                .collect(),
            span: exprs.span,
            accurate_span: true,
        }
    }
    pub fn from_item(
        ident: &Spanned<Ident>,
        args: Option<Spanned<Vec<Spanned<parser::Param>>>>,
        env: &Env,
    ) -> Self {
        match args {
            Some(expr) => Self::from_exprs(expr, env),
            None => Self {
                args: Vec::new(),
                span: ident.span,
                accurate_span: false,
            },
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

    pub fn map_complete<O, F: FnOnce(T) -> O>(self, f: F) -> Partial<O> {
        let new = f(self.value);
        Partial {
            value: new,
            degraded: self.degraded,
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

impl<T: Default> Default for Partial<T> {
    fn default() -> Self {
        Partial::complete(T::default())
    }
}

impl<T> std::ops::Deref for Partial<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

pub struct Ctx {
    pub home_dir: Result<PathBuf, common::rel_path::HomeError>,
    pub errors: Vec<crate::error::Error>,
}

impl Ctx {
    pub fn home_dir(&self) -> Result<&Path, common::rel_path::HomeError> {
        self.home_dir.as_deref().map_err(Clone::clone)
    }

    /// Emit an error to the output
    pub fn emit<E: Into<error::Error>>(&mut self, error: E) {
        self.errors.push(error.into());
    }
}

pub trait FromValue: Sized {
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

#[derive(Debug)]
enum CommandError {
    Empty,
}

impl std::fmt::Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Expected at least one element")
    }
}

impl std::error::Error for CommandError {}

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

pub enum ParseResult<T> {
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

pub trait ParseParam: Sized {
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

pub trait FromArgs: Sized {
    fn from_args(args: Args, ctx: &mut Ctx) -> Option<Partial<Self>>;
}

pub trait RecoverFromArgs: Sized {
    fn recover_default(args: Args, ctx: &mut Ctx) -> Partial<Self>;
}

#[macro_export]
macro_rules! params {
    (__parse__ $span:ident, $name:expr, $value:expr, $context:expr, $field:ident, $kind:ty) => {
        let $field = <$kind as $crate::ParseParam>::parse(
            $value,
            $context,
        );
    };
    ($where:vis $what:ident $name:ident { $($fvis:vis $field:ident : $kind:ty),* $(,)? }) => {
        $where $what $name { $($fvis $field : $kind),* }

        impl $name {
            fn parse_raw_params(args: $crate::Args, ctx: &mut $crate::Ctx) -> $crate::Partial<($(Option<$kind>,)*)> {
                let names = [$(stringify!($field)),*];
                let mut matchmaker = $crate::Matchmaker::new(&names, args.args);
                let mut values = vec![None; names.len()];
                let mut degraded = false;
                while let Some(res) = matchmaker.next() {
                    match res {
                        Ok((param, arg)) => values[param.0] = Some(arg.value),
                        Err(err) => match err {
                            $crate::MatchError::Unused(arg) => {
                                ctx.emit(
                                    $crate::ParamError::Unused {
                                        span: arg.name.span,
                                        name: arg.name.inner,
                                    }
                                );
                                degraded = true;
                            },
                            // If this really is an error, it will be reported in the parsing stage
                            $crate::MatchError::NotFound(_) => (),
                        }
                    }
                }

                #[allow(unused_variables, unused_mut)]
                let (mut names, mut values) = (names.into_iter(), values.into_iter());
                $(
                    let $field = {
                        let value = values.next().unwrap();
                        let name = names.next().unwrap();
                        match <$kind as $crate::ParseParam>::parse(
                            value,
                            ctx,
                        ) {
                            $crate::ParseResult::Ok(val) => Some(val),
                            $crate::ParseResult::Degraded(val) => {
                                degraded = true;
                                Some(val)
                            }
                            $crate::ParseResult::ExpectedValue => {
                                ctx.emit($crate::ParamError::NotFound {
                                    span: args.span,
                                    name,
                                    has_args: args.accurate_span,
                                });
                                degraded = true;
                                None
                            }
                            $crate::ParseResult::Failed => {
                                degraded = true;
                                None
                            }
                        }
                    };
                )*
                $crate::Partial::new(
                    ($($field,)*),
                    degraded,
                )
            }
        }

        impl $crate::FromArgs for $name {
            fn from_args(args: $crate::Args, ctx: &mut $crate::Ctx) -> Option<$crate::Partial<Self>> {
                let values = Self::parse_raw_params(args, ctx);
                #[allow(unused_variables, unused_mut)]
                let mut degraded = values.is_degraded();
                let ($($field,)*) = values.to_value();

                let out = Self { $($field: $field?,)* };
                Some($crate::Partial::new(out, degraded))
            }
        }

        impl $crate::RecoverFromArgs for $name
            where $($kind: Default),*
        {
            fn recover_default(args: $crate::Args, ctx: &mut $crate::Ctx) -> $crate::Partial<Self> {
                Self::parse_raw_params(args, ctx).map(|($($field,)*)| {
                    let out = Self { $( $field: $field.unwrap_or_default() ),* };
                    $crate::Partial::complete(out)
                })
            }
        }
    };
}

pub enum MatchError {
    Unused(Arg),
    NotFound(ParamId),
}

#[derive(Debug, Clone, Copy)]
pub struct ParamId(pub usize);

pub struct Matchmaker {
    args: std::vec::IntoIter<Arg>,
    params: std::vec::IntoIter<(ParamId, &'static str)>,
}

impl Matchmaker {
    pub fn new(params: &[&'static str], mut args: Vec<Arg>) -> Self {
        args.sort_unstable_by(|a, b| a.name.0.cmp(&b.name.0));

        let mut params = params
            .iter()
            .copied()
            .enumerate()
            .map(|(i, p)| (ParamId(i), p))
            .collect::<Vec<_>>();
        params.sort_unstable_by(|a, b| a.1.cmp(b.1));

        Self {
            args: args.into_iter(),
            params: params.into_iter(),
        }
    }
}

impl Iterator for Matchmaker {
    type Item = Result<(ParamId, Arg), MatchError>;
    fn next(&mut self) -> Option<Self::Item> {
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
