#[macro_use]
pub mod error;
#[macro_use]
pub mod structures;
pub mod provider;

use ariadne::{Color, Fmt, ReportKind};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);
pub type Env = HashMap<String, Value>;

#[derive(Debug, Clone)]
pub enum Value {
    List(Spanned<Vec<Value>>),
    String(Spanned<String>),
    Bool(Spanned<bool>),
    Error(Spanned<EvalError>),
}

#[derive(Debug, Clone)]
pub enum EvalError {
    UnknownVar(String),
    VarError(String, Box<Spanned<EvalError>>),
}

report! {
    (EvalError, Span) {
        (EvalError::UnknownVar(var), span) => {
            report(ReportKind::Error, span.start);
            message("Undefined variable ${}", var.fg(Color::Red));
            label(span, Color::Red, "This variable is not defined");

        }
        (EvalError::VarError(var, _), span) => {
            report(ReportKind::Error, span.start);
            message("Encountered an error while evaluating variable ${}", var.fg(Color::Red));
            label(span, Color::Red, "Cannot evaluate this variable");
        }
    }
}

impl Value {
    fn from_expr(expr: parser::Expr, env: &Env) -> Self {
        match expr {
            parser::Expr::Variable((var, span)) => match env.get(&var) {
                Some(v) => match v {
                    Value::Error(e) => {
                        Value::Error((EvalError::VarError(var, Box::new(e.clone())), span))
                    }
                    v => {
                        let mut val = v.clone();
                        val.set_span(span);
                        val
                    }
                },
                None => Value::Error((EvalError::UnknownVar(var), span)),
            },
            parser::Expr::String(s) => Self::String(s),
            parser::Expr::Bool(b) => Self::Bool(b),
            parser::Expr::List((list, span)) => Self::List((
                list.into_iter().map(|e| Self::from_expr(e, env)).collect(),
                span,
            )),
        }
    }

    fn get_type(&self) -> Type {
        match self {
            Value::List(_) => Type::List,
            Value::String(_) => Type::String,
            Value::Bool(_) => Type::Bool,
            Value::Error(_) => Type::Error,
        }
    }
    fn span(&self) -> Span {
        match self {
            Value::List(l) => &l.1,
            Value::String(s) => &s.1,
            Value::Bool(b) => &b.1,
            Value::Error(e) => &e.1,
        }
        .clone()
    }
    fn set_span(&mut self, span: Span) {
        *match self {
            Value::List(l) => &mut l.1,
            Value::String(s) => &mut s.1,
            Value::Bool(b) => &mut b.1,
            Value::Error(e) => &mut e.1,
        } = span;
    }
}
#[derive(Debug, Clone, Copy)]
enum Type {
    List,
    String,
    Bool,
    Error,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::List => "List",
            Type::String => "String",
            Type::Bool => "Bool",
            Type::Error => "Error",
        };
        f.write_str(s)
    }
}

#[derive(Debug, Clone)]
struct Package {
    name: Spanned<String>,
    args: Args,
}

#[derive(Debug, Clone)]
struct Packages {
    pub packages: Vec<Package>,
    pub span: Span,
}

impl Packages {
    pub fn new(packages: Vec<Package>, span: Span) -> Self {
        Self { packages, span }
    }
}

#[derive(Debug, Clone)]
struct Arg {
    name: Spanned<String>,
    value: Value,
}

impl Arg {
    fn from_expr(expr: parser::Param, env: &Env) -> Self {
        Arg {
            name: expr.name,
            value: Value::from_expr(expr.val, &env),
        }
    }
}

#[derive(Debug, Clone)]
struct Args {
    pub args: Vec<Arg>,
    pub span: Span,
}

impl Args {
    pub fn new(args: Vec<Arg>, span: Span) -> Self {
        Self { args, span }
    }

    pub fn from_exprs(exprs: Spanned<Vec<Spanned<parser::Param>>>, env: &Env) -> Self {
        Self::new(
            exprs
                .0
                .into_iter()
                .map(|(arg, _)| Arg {
                    name: arg.name,
                    value: Value::from_expr(arg.val, env),
                })
                .collect(),
            exprs.1,
        )
    }
}

#[derive(Debug, Clone)]
pub struct MemberPath(pub PathBuf);

#[derive(Debug, thiserror::Error)]
pub enum PathError {
    #[error("The path must be relative to the current directory")]
    NotRelative,
    #[error("The path cannot contain multiple components")]
    TooDeep,
    #[error("The path cannot be empty")]
    Empty,
}

impl std::str::FromStr for MemberPath {
    type Err = PathError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let path = Path::new(s);
        if path.is_absolute() || path.starts_with("~") {
            Err(PathError::NotRelative)
        } else {
            match path.components().count() {
                0 => Err(PathError::Empty),
                1 => Ok(Self(path.to_owned())),
                _ => Err(PathError::TooDeep),
            }
        }
    }
}

use eval::Ctx;
pub use eval::{Interpreter, RoutineFigment, UnitFigment};
mod eval {
    use std::path::Path;

    use super::{error::Error, Arg, Args, Env, MemberPath, Package, Packages, Spanned, Value};
    use crate::{provider::Transaction, structures};
    use common::{command::Command, rel_path::RelPath};

    params! { struct NoParams {} }

    params! {
        struct RoutineParams {
            shell: Result<Option<Command>, ()>,
            stdin: Result<Option<bool>,()>,
            stdout: Result<Option<bool>, ()>,
        }
    }

    params! {
        struct UnitHeader {
            name: Result<String, ()>,
            desc: Result<String, ()>,
            topic: Result<Option<String>, ()>,
            shell: Result<Option<Command>, ()>,
            members: Result<Option<Vec<structures::ParseStr<MemberPath>>>, ()>,
        }
    }

    #[derive(Debug, Default)]
    pub struct RoutineFigment {
        pub shell: Option<Command>,
        pub stdin: Option<bool>,
        pub stdout: Option<bool>,
        pub body: String,
    }

    #[derive(Debug, Default)]
    pub struct UnitFigment {
        pub name: Option<String>,
        pub desc: Option<String>,
        pub topic: Option<String>,
        pub shell: Option<Command>,

        pub transactions: Vec<Transaction>,
        pub deploy: Option<RoutineFigment>,
        pub remove: Option<RoutineFigment>,
        pub capture: Option<RoutineFigment>,
    }

    pub struct UnitData {
        pub figment: UnitFigment,
        pub env: Env,
        pub members: Option<Vec<MemberPath>>,
        pub errors: Vec<Error>,
    }

    pub(super) struct Ctx<'a> {
        members: Vec<MemberPath>,
        dir: RelPath,
        home_dir: Result<&'a Path, common::rel_path::HomeError>,
        root: bool,
        errors: Vec<crate::error::Error>,
    }

    impl<'a> Ctx<'a> {
        pub fn members(&self) -> &[MemberPath] {
            &self.members
        }

        pub fn unit_dir(&self) -> &RelPath {
            &self.dir
        }

        pub fn home_dir(&self) -> Result<&Path, common::rel_path::HomeError> {
            self.home_dir
        }

        pub fn emit<E: Into<crate::error::Error>>(&mut self, error: E) {
            self.errors.push(error.into());
        }

        pub fn has_root(&self) -> bool {
            self.root
        }
    }

    pub struct Interpreter {
        parser: parser::Parser,
    }

    impl Interpreter {
        pub fn new() -> Self {
            Self {
                parser: parser::Parser::new(),
            }
        }

        pub fn eval(
            &self,
            src: &str,
            path: &RelPath,
            manager: &mut super::provider::Manager,
            mut env: Env,
            home_dir: Result<&Path, common::rel_path::HomeError>,
            root: bool,
        ) -> UnitData {
            let mut context = Ctx {
                members: Vec::new(),
                dir: {
                    let mut path = path.clone();
                    path.pop();
                    path
                },
                home_dir,
                root,
                errors: Vec::new(),
            };

            let (blocks, errors) = self.parser.parse(src);
            for err in errors {
                context.emit(err);
            }

            let mut unit_name = None;
            let mut desc = None;
            let mut topic = None;
            let mut shell = None;
            let mut members = None;

            let mut transactions = Vec::new();
            let mut deploy = None;
            let mut remove = None;
            let mut capture = None;

            for block in blocks {
                match block {
                    parser::Block::Map { name, params, body } => match name {
                        parser::Name::Unit => {
                            let _ = parse_args::<NoParams>(params, &env, &mut context);
                            let (header, _) = parse_args::<UnitHeader>(body, &env, &mut context)
                                .expect("Param parsing should be infallible");
                            unit_name = header.name.ok();
                            desc = header.desc.ok();
                            topic = header.topic.unwrap_or_default();
                            shell = header.shell.unwrap_or_default();
                            members = header.members.unwrap_or_default().map(|members| {
                                members.into_iter().map(|path| path.0).collect::<Vec<_>>()
                            });
                            if let Some(members) = members.as_ref() {
                                context.members = members.clone();
                            }
                        }
                        parser::Name::Env => {
                            let _ = parse_args::<NoParams>(params, &env, &mut context);
                            for (param, _span) in body.0 {
                                let name = param.name.0;
                                let val = Value::from_expr(param.val, &env);

                                if let Value::Error(err) = &val {
                                    context.emit(err.clone());
                                }
                                env.insert(name, val);
                            }
                        }
                        _ => panic!("Map block has invalid name"),
                    },
                    parser::Block::Items {
                        name,
                        params,
                        items,
                    } => {
                        let packages = Packages::new(
                            items
                                .0
                                .into_iter()
                                .map(|item| Package {
                                    name: item.name,
                                    args: Args::from_exprs(item.args, &env),
                                })
                                .collect(),
                            items.1,
                        );

                        let args = (
                            params
                                .0
                                .into_iter()
                                .map(|(arg, _)| Arg::from_expr(arg, &env))
                                .collect(),
                            params.1,
                        );

                        let res = match name {
                            parser::Name::Files => manager.new_files(args, packages, &mut context),
                            parser::Name::Packages => {
                                manager.new_transaction(args, packages, &mut context)
                            }
                            _ => panic!("Item block has invalid name"),
                        };
                        if let Ok(t) = res {
                            transactions.push(t)
                        }
                    }
                    parser::Block::Routine {
                        name,
                        params,
                        content,
                    } => {
                        let (params, _) = parse_args::<RoutineParams>(params, &env, &mut context)
                            .expect("Param parsing should be infallible");
                        let routine = RoutineFigment {
                            shell: params.shell.unwrap_or_default(),
                            stdin: params.stdin.unwrap_or_default(),
                            stdout: params.stdout.unwrap_or_default(),
                            body: content,
                        };
                        match name {
                            // TODO: make it an error to have multiple routine blocks of the same name
                            parser::Name::Deploy => deploy = Some(routine),
                            parser::Name::Remove => remove = Some(routine),
                            parser::Name::Capture => capture = Some(routine),
                            _ => panic!("Routine block has invalid name"),
                        }
                    }
                }
            }

            UnitData {
                figment: UnitFigment {
                    name: unit_name,
                    desc,
                    topic,
                    shell,
                    transactions,
                    deploy,
                    remove,
                    capture,
                },
                env,
                members,
                errors: context.errors,
            }
        }
    }

    fn parse_args<T: structures::FromArgs>(
        args: Spanned<Vec<Spanned<parser::Param>>>,
        env: &Env,
        ctx: &mut Ctx,
    ) -> Result<(T, bool), ()> {
        let args = Args::from_exprs(args, env);
        T::from_args(args, ctx)
    }
}
