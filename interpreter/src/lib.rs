#[macro_use]
pub mod structures;
#[macro_use]
pub mod error;
pub mod provider;

use crate::{context::Context, rel_path::RelPath, shell::Shell};
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
                    v => v.clone(),
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
            Value::List(l) => l.1.clone(),
            Value::String(s) => s.1.clone(),
            Value::Bool(b) => b.1.clone(),
            Value::Error(e) => e.1.clone(),
        }
    }
}
#[derive(Debug, Clone, Copy)]
enum Type {
    List,
    String,
    Bool,
    Error,
}

#[derive(Debug, Clone)]
struct Package {
    name: Spanned<String>,
    args: Args,
}

#[derive(Debug, Clone, Copy)]
struct PackageId(pub usize);

#[derive(Debug, Clone)]
struct Packages {
    iter: std::vec::IntoIter<Package>,
    n: usize,
    span: Span,
}

impl Packages {
    pub fn new(packages: Vec<Package>, span: Span) -> Self {
        Self {
            iter: packages.into_iter(),
            n: 0,
            span,
        }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl Iterator for Packages {
    type Item = (PackageId, Package);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|package| {
            let id = self.n;
            self.n += 1;
            (PackageId(id), package)
        })
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

#[derive(Debug, Clone, Copy)]
struct ArgId(pub usize);

#[derive(Debug, Clone)]
struct Args {
    iter: std::vec::IntoIter<Arg>,
    n: usize,
    span: Span,
}

impl Args {
    pub fn new(args: Vec<Arg>, span: Span) -> Self {
        Self {
            iter: args.into_iter(),
            n: 0,
            span,
        }
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

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl Iterator for Args {
    type Item = (ArgId, Arg);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|package| {
            let id = self.n;
            self.n += 1;
            (ArgId(id), package)
        })
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

use eval::EvalCtx;
pub use eval::{Interpreter, UnitFigment};
mod eval {
    use super::{
        error::{Emitter, Error},
        provider, Arg, Args, Env, MemberPath, Package, Packages, Spanned, Value,
    };
    use crate::{
        context::Context, provider::Transaction, rel_path::RelPath, shell::Shell, structures,
        Routine, Unit,
    };

    params! { struct NoParams {} }

    params! {
        struct RoutineParams {
            shell: Option<Vec<String>>,
            stdin: Option<bool>,
            stdout: Option<bool>,
        }
    }

    params! {
        struct UnitHeader {
            name: Result<String, ()>,
            desc: Result<String, ()>,
            topic: Option<String>,
            shell: Option<Vec<String>>,
            members: Option<Vec<structures::ParseStr<MemberPath>>>,
        }
    }

    #[derive(Debug, Default)]
    pub struct UnitFigment {
        pub name: Option<String>,
        pub desc: Option<String>,
        pub topic: Option<String>,
        pub shell: Option<Shell>,

        pub transactions: Vec<Transaction>,
        pub deploy: Option<Routine>,
        pub remove: Option<Routine>,
        pub capture: Option<Routine>,
    }

    pub struct UnitData {
        pub figment: UnitFigment,
        pub env: Env,
        pub members: Option<Vec<MemberPath>>,
        pub errors: Vec<Error>,
    }

    pub(super) struct EvalCtx {
        members: Vec<MemberPath>,
        dir: RelPath,
    }

    impl EvalCtx {
        pub fn members(&self) -> &[MemberPath] {
            &self.members
        }

        pub fn dir(&self) -> &RelPath {
            &self.dir
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
            context: &crate::context::Context,
        ) -> UnitData {
            let mut emitter = Emitter::new();

            let (blocks, errors) = self.parser.parse(src);
            for err in errors {
                emitter.emit(err);
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
            let mut eval_ctx = EvalCtx {
                members: vec![],
                dir: {
                    let mut path = path.clone();
                    path.pop();
                    path
                },
            };

            for block in blocks {
                match block {
                    parser::Block::Map { name, params, body } => match name {
                        parser::Name::Unit => {
                            let _ = parse_args::<NoParams>(params, &env, &mut emitter, context);
                            if let Ok(header) =
                                parse_args::<UnitHeader>(body, &env, &mut emitter, context)
                            {
                                unit_name = header.name.ok();
                                desc = header.desc.ok();
                                topic = header.topic;
                                shell = header.shell;
                                members = header.members.map(|members| {
                                    members.into_iter().map(|path| path.0).collect::<Vec<_>>()
                                });
                                if let Some(members) = members.as_ref() {
                                    eval_ctx.members = members.clone();
                                }
                            }
                        }
                        parser::Name::Env => {
                            let _ = parse_args::<NoParams>(params, &env, &mut emitter, context);
                            for (param, _span) in body.0 {
                                let name = param.name.0;
                                let val = Value::from_expr(param.val, &env);

                                if let Value::Error(err) = &val {
                                    emitter.emit(err.clone());
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
                            parser::Name::Files => {
                                manager.new_files(args, packages, &mut emitter, &eval_ctx, context)
                            }
                            parser::Name::Packages => manager.new_transaction(
                                args,
                                packages,
                                &mut emitter,
                                &eval_ctx,
                                context,
                            ),
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
                        if let Ok(params) =
                            parse_args::<RoutineParams>(params, &env, &mut emitter, context)
                        {
                            let routine = super::Routine {
                                shell: params.shell.map(crate::shell::Shell::from_vec),
                                stdin: params.stdin.unwrap_or(true),
                                stdout: params.stdout.unwrap_or(true),
                                code: content,
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
            }

            UnitData {
                figment: UnitFigment {
                    name: unit_name,
                    desc,
                    topic,
                    shell: shell.map(Shell::from_vec),
                    transactions,
                    deploy,
                    remove,
                    capture,
                },
                env,
                members,
                errors: emitter.into_errors(),
            }
        }
    }

    fn collect_errors<I: IntoIterator<Item = Result<A, B>>, A, B>(
        iter: I,
    ) -> Result<Vec<A>, Vec<B>> {
        iter.into_iter()
            .fold(Ok(Vec::new()), |state, next| match (state, next) {
                (Ok(mut list), Ok(next)) => {
                    list.push(next);
                    Ok(list)
                }
                (Ok(_), Err(err)) => Err(vec![err]),
                (Err(errors), Ok(_)) => Err(errors),
                (Err(mut errors), Err(err)) => {
                    errors.push(err);
                    Err(errors)
                }
            })
    }

    fn parse_args<T: structures::FromArgs>(
        args: Spanned<Vec<Spanned<parser::Param>>>,
        env: &Env,
        emitter: &mut Emitter,
        context: &Context,
    ) -> Result<T, ()> {
        let args = Args::from_exprs(args, env);
        T::from_args(args, emitter, context)
    }
}
