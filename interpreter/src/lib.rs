#[macro_use]
pub mod error;
#[macro_use]
pub mod structures;
pub mod provider;

use ariadne::{Color, Fmt, ReportKind};
use common::rel_path::RelPath;
use std::{
    collections::HashMap,
    fmt::Display,
    path::{Path, PathBuf},
};
use structures::FromValue;

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);
pub type Env = HashMap<String, Value>;

#[derive(Debug)]
enum EvalError {
    NotRelative(Span, LocalPath),
    TooDeep(Span, LocalPath),
    Empty(Span),
    NotFound(Span, UnitPath),
}

report! {
    EvalError {
        EvalError::NotRelative(span, path) => {
            report(ReportKind::Error, span.start);
            message("Member '{}' must be relative", path.fg(Color::Red));
            label(span, Color::Red, "The path must be relative to the current directory");
        }
        EvalError::TooDeep(span, path) => {
            report(ReportKind::Error, span.start);
            message("Member '{}' is too deep", path.fg(Color::Red));
            label(span, Color::Red, "This path must only have a single component");
        }
        EvalError::Empty(span) => {
            report(ReportKind::Error, span.start);
            message("Empty member path");
            label(span, Color::Red, "This path must not be empty");
        }
        EvalError::NotFound(span, path) => {
            report(ReportKind::Error, span.start);
            message("Member '{}' doesn't exist", (&path).fg(Color::Red));
            label(span, Color::Red, "Cannot find file '{}'", path.unit_file().fg(Color::Red));
        }

    }
}

#[derive(Debug, Clone)]
pub enum Value {
    List(Spanned<Vec<Value>>),
    String(Spanned<String>),
    Bool(Spanned<bool>),
    Error(Spanned<ValueError>),
}

#[derive(Debug, Clone)]
pub enum ValueError {
    UnknownVar(String),
    VarError(String, Box<Spanned<ValueError>>),
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

impl Value {
    fn from_expr(expr: parser::Expr, env: &Env) -> Self {
        match expr {
            parser::Expr::Variable((var, span)) => match env.get(&var) {
                Some(v) => match v {
                    Value::Error(e) => {
                        Value::Error((ValueError::VarError(var, Box::new(e.clone())), span))
                    }
                    v => {
                        let mut val = v.clone();
                        val.set_span(span);
                        val
                    }
                },
                None => Value::Error((ValueError::UnknownVar(var), span)),
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
pub struct LocalPath(PathBuf);

impl std::fmt::Display for LocalPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display().fmt(f)
    }
}

impl LocalPath {
    pub fn bind(&self, mut location: RelPath) -> RelPath {
        location.push(&self.0);
        location
    }

    pub fn join<P: AsRef<Path>>(&self, path: P) -> Self {
        Self(self.0.join(path.as_ref()))
    }

    pub fn push<P: AsRef<Path>>(&mut self, path: P) {
        self.0.push(path.as_ref())
    }
}

impl From<UnitPath> for LocalPath {
    fn from(path: UnitPath) -> Self {
        match path.0 {
            UnitPathInner::Root => LocalPath("".into()),
            UnitPathInner::Path(path) => path,
        }
    }
}

#[derive(Debug, Clone)]
enum UnitPathInner {
    Root,
    Path(LocalPath),
}

#[derive(Debug, Clone)]
pub struct UnitPath(UnitPathInner);

impl UnitPath {
    pub fn name(&self) -> String {
        match &self.0 {
            UnitPathInner::Root => "main".into(),
            UnitPathInner::Path(path) => path
                .0
                .components()
                .last()
                .unwrap()
                .as_os_str()
                .to_string_lossy()
                .into_owned(),
        }
    }

    pub fn unit_file(self) -> LocalPath {
        let file = self.name() + ".unit";
        LocalPath::from(self).join(file)
    }

    pub fn root() -> Self {
        Self(UnitPathInner::Root)
    }

    pub fn join<P: AsRef<Path>>(&self, path: P) -> LocalPath {
        let mut new = LocalPath::from(self.clone());
        new.push(path.as_ref());
        new
    }

    pub fn bind(&self, mut location: RelPath) -> RelPath {
        location.push(LocalPath::from(self.clone()).0);
        location
    }
}

impl FromValue for UnitPath {
    fn from_value(value: Value, ctx: &mut Ctx) -> Result<Self, ()> {
        <(PathBuf, Span)>::from_value(value, ctx).and_then(|(path, span)| {
            if path.is_absolute() || path.starts_with("~") {
                Err(EvalError::NotRelative(span, LocalPath(path)))
            } else {
                match path.components().count() {
                    0 => Err(EvalError::Empty(span)),
                    1 => {
                        let path = UnitPath(UnitPathInner::Path(
                            LocalPath::from(ctx.unit_dir().clone()).join(path),
                        ));
                        if path
                            .clone()
                            .unit_file()
                            .bind(ctx.dotfile_dir().clone())
                            .exists()
                        {
                            Ok(path)
                        } else {
                            Err(EvalError::NotFound(span, path))
                        }
                    }
                    _ => Err(EvalError::TooDeep(span, LocalPath(path))),
                }
            }
            .map_err(|err| ctx.emit(err))
        })
    }
}

impl Display for UnitPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            UnitPathInner::Root => f.write_str("main"),
            UnitPathInner::Path(path) => path.fmt(f),
        }
    }
}

use eval::Ctx;
pub use eval::{Interpreter, RoutineFigment, UnitFigment};
mod eval {
    use std::path::Path;

    use super::{error::Error, Arg, Args, Env, Package, Packages, Spanned, UnitPath, Value};
    use crate::{provider::Transaction, structures};
    use common::{command::Command, rel_path::RelPath};

    params! { struct NoParams {} }

    params! {
        struct RoutineParams {
            shell: Result<Option<Command>, ()>,
            stdout: Result<Option<bool>, ()>,
            workdir: Result<Option<RelPath>, ()>,
        }
    }

    params! {
        struct UnitHeader {
            name: Result<String, ()>,
            desc: Result<String, ()>,
            topic: Result<Option<String>, ()>,
            shell: Result<Option<Command>, ()>,
            members: Result<Option<structures::CollectOk<UnitPath>>, ()>,
        }
    }

    #[derive(Debug, Default)]
    pub struct RoutineFigment {
        pub shell: Option<Command>,
        pub stdout: Option<bool>,
        pub workdir: Option<RelPath>,
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
        pub members: Option<Vec<UnitPath>>,
        pub errors: Vec<Error>,
    }

    pub(super) struct Ctx<'a> {
        members: Vec<UnitPath>,
        dir: UnitPath,
        home_dir: Result<&'a Path, common::rel_path::HomeError>,
        dotfile_dir: &'a RelPath,
        root: bool,
        errors: Vec<crate::error::Error>,
    }

    impl<'a> Ctx<'a> {
        pub fn members(&self) -> &[UnitPath] {
            &self.members
        }

        pub fn unit_dir(&self) -> &UnitPath {
            &self.dir
        }

        pub fn home_dir(&self) -> Result<&Path, common::rel_path::HomeError> {
            self.home_dir
        }

        pub fn dotfile_dir(&self) -> &RelPath {
            self.dotfile_dir
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
            path: UnitPath,
            manager: &mut super::provider::Manager,
            mut env: Env,
            home_dir: Result<&Path, common::rel_path::HomeError>,
            dotfile_dir: &RelPath,
            root: bool,
        ) -> UnitData {
            let mut context = Ctx {
                members: Vec::new(),
                dir: path,
                home_dir,
                dotfile_dir,
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
                            members = header.members.unwrap_or_default().map(|members| members.0);
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
                            stdout: params.stdout.unwrap_or_default(),
                            workdir: params.workdir.unwrap_or_default(),
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
