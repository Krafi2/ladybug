pub mod error;
pub mod provider;

use std::{
    collections::HashMap,
    fmt::Display,
    path::{Path, PathBuf},
};

use ariadne::{Color, Fmt, ReportKind};

use common::rel_path::RelPath;
use error::IntoReport;
use parser::{span::AriadneSpan, Ident, Span, Spanned};
use structures::Partial;

use self::structures::FromValue;

#[derive(Debug)]
enum BlockType {
    Map,
    List,
    Code,
}

impl From<parser::Body> for BlockType {
    fn from(value: parser::Body) -> Self {
        match value {
            parser::Body::Map(_) => Self::Map,
            parser::Body::List(_) => Self::List,
            parser::Body::Code(_) => Self::Code,
        }
    }
}

impl std::fmt::Display for BlockType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BlockType::Map => "Map",
            BlockType::List => "List",
            BlockType::Code => "Code",
        };
        f.write_str(s)
    }
}

#[derive(Debug)]
enum StructureError {
    ExpectedUnit(Span, Ident),
    MisplacedUnit(Span),
    MisplacedEnv(Span),
    UnexpectedBlock {
        span: Span,
        found: Ident,
        expected: Vec<&'static str>,
    },
    WrongType {
        span: Span,
        found: BlockType,
        expected: BlockType,
    },
}

report! {
    StructureError {
        StructureError::ExpectedUnit(span, name) => {
            report(ReportKind::Error, span.start);
            message("The file should start with the unit declaration");
            label(span, Color::Red, "Expected 'unit', found '{}'", name.fg(Color::Red));
        }
        StructureError::MisplacedUnit(span) => {
            report(ReportKind::Error, span.start);
            message("The unit declaration should be located at the start of the file");
            label(span, Color::Red, "'unit' expected at the top of file");
        }
        StructureError::MisplacedEnv(span) => {
            report(ReportKind::Error, span.start);
            message("The env block should be placed directly after the unit declaration");
            label(span, Color::Red, "Unexpected 'env'");
        }
        StructureError::UnexpectedBlock { span, found, expected } => {
            report(ReportKind::Error, span.start);
            message("Unknown block '{}'", found);
            label(
                span,
                Color::Red,
                "Expected one of {}",
                expected.into_iter().map(|s| format!("'{s}'")).collect::<Vec<_>>().join(", ")
            );
        }
        StructureError::WrongType { span, found, expected } => {
            report(ReportKind::Error, span.start);
            message("Expected {} block", expected);
            label(
                span,
                Color::Red,
                "Found {}",
                found.fg(Color::Red),
            );

        }
    }
}

#[derive(Debug)]
enum PathError {
    NotRelative(Span, LocalPath),
    TooDeep(Span, LocalPath),
    Empty(Span),
    NotFound(Span, UnitPath),
}

report! {
    PathError {
        PathError::NotRelative(span, path) => {
            report(ReportKind::Error, span.start);
            message("Member '{}' must be relative", path.fg(Color::Red));
            label(span, Color::Red, "The path must be relative to the current directory");
        }
        PathError::TooDeep(span, path) => {
            report(ReportKind::Error, span.start);
            message("Member '{}' is too deep", path.fg(Color::Red));
            label(span, Color::Red, "This path must only have a single component");
        }
        PathError::Empty(span) => {
            report(ReportKind::Error, span.start);
            message("Empty member path");
            label(span, Color::Red, "This path must not be empty");
        }
        PathError::NotFound(span, path) => {
            report(ReportKind::Error, span.start);
            message("Member '{}' doesn't exist", (&path).fg(Color::Red));
            label(span, Color::Red, "Cannot find file '{}'", path.unit_file().fg(Color::Red));
        }

    }
}

// A path relative to the dotfile directory
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
        // The path is flattened into a single directory separated by dots.
        // Fox example "foo/bar/baz/" becomes "foo.bar.baz/"
        match path.0 {
            UnitPathInner::Root => LocalPath("main".into()),
            UnitPathInner::Path(path) => {
                let dir = path
                    .0
                    .components()
                    .map(|c| c.as_os_str())
                    .collect::<Vec<_>>()
                    .join(".".as_ref());
                LocalPath(dir.into())
            }
        }
    }
}

#[derive(Debug, Clone)]
enum UnitPathInner {
    Root,
    Path(LocalPath),
}

// A `LocalPath` that points to a unit
#[derive(Debug, Clone)]
pub struct UnitPath(UnitPathInner);

impl UnitPath {
    pub fn name(&self) -> String {
        match &self.0 {
            // TODO: Think of a better name than "main"
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
        let file = self.name() + ".ldbg";
        LocalPath::from(self).join(file)
    }

    pub fn root() -> Self {
        Self(UnitPathInner::Root)
    }

    pub fn join<P: AsRef<Path> + ToOwned<Owned = PathBuf>>(&self, path: P) -> LocalPath {
        match self.0 {
            // Don't add the root prefix to paths
            UnitPathInner::Root => LocalPath(path.to_owned()),
            // Join non-root paths
            UnitPathInner::Path(_) => {
                let mut new = LocalPath::from(self.clone());
                new.push(path.as_ref());
                new
            }
        }
    }

    pub fn bind(&self, mut location: RelPath) -> RelPath {
        location.push(LocalPath::from(self.clone()).0);
        location
    }
}

impl FromValue for UnitPath {
    fn from_value(value: Spanned<Value>, ctx: &mut Ctx) -> Option<Partial<Self>> {
        <Spanned<PathBuf>>::from_value(value, ctx).and_then(|path| {
            path.map(|path| {
                let span = path.span;
                let path = path.inner;

                let res = if path.is_absolute() || path.starts_with("~") {
                    Err(PathError::NotRelative(span, LocalPath(path)))
                } else {
                    match path.components().count() {
                        0 => Err(PathError::Empty(span)),
                        1 => {
                            let path =
                                UnitPath(UnitPathInner::Path(ctx.unit_dir().clone().join(path)));
                            if path
                                .clone()
                                .unit_file()
                                .bind(ctx.dotfile_dir().clone())
                                .exists()
                            {
                                Ok(path)
                            } else {
                                Err(PathError::NotFound(span, path))
                            }
                        }
                        _ => Err(PathError::TooDeep(span, LocalPath(path))),
                    }
                };
                match res {
                    Ok(path) => Partial::complete(Some(path)),
                    Err(err) => {
                        ctx.emit(err);
                        Partial::degraded(None)
                    }
                }
            })
            .transpose()
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

pub use eval::Ctx;
pub use eval::{Interpreter, RoutineFigment, UnitFigment};
mod eval {
    use std::{collections::HashMap, path::Path};

    use super::{error::Error, Args, Env, Packages, Spanned, UnitPath, Value};
    use crate::{provider::Transaction, structures::RecoverFromArgs, BlockType, StructureError};
    use common::{command::Command, rel_path::RelPath};
    use parser::{Body, Ident};

    #[derive(Clone, Copy)]
    enum Name {
        Unit,
        Env,
        Files,
        Packages,
        Deploy,
        Remove,
        Capture,
    }

    impl Name {
        fn from_ident(ident: &Ident) -> Option<Name> {
            match ident.0.as_str() {
                "unit" => Some(Self::Unit),
                "env" => Some(Self::Env),
                "files" => Some(Self::Files),
                "packages" => Some(Self::Packages),
                "deploy" => Some(Self::Deploy),
                "remove" => Some(Self::Remove),
                "capture" => Some(Self::Capture),
                _ => None,
            }
        }
    }

    params! { struct NoParams {} }

    params! {
        struct RoutineParams {
            shell: Option<Command>,
            stdout: Option<bool>,
            workdir: Option<RelPath>,
        }
    }

    params! {
        struct UnitHeader {
            name: Option<String>,
            desc: Option<String>,
            topic: Option<String>,
            shell: Option<Command>,
            members: Option<Vec<UnitPath>>,
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
        pub deploy: Vec<RoutineFigment>,
        pub remove: Vec<RoutineFigment>,
        pub capture: Vec<RoutineFigment>,
    }

    pub struct UnitData {
        pub figment: UnitFigment,
        pub env: Env,
        pub members: Option<Vec<UnitPath>>,
        pub errors: Vec<Error>,
    }

    params! {
        pub struct Config {
            pub dotfiles: Option<RelPath>,
            pub shell: Option<Command>,
        }
    }

    pub struct Ctx<'a> {
        members: Vec<UnitPath>,
        dir: Option<UnitPath>,
        home_dir: Result<&'a Path, common::rel_path::HomeError>,
        dotfile_dir: Option<&'a RelPath>,
        root: bool,
        errors: Vec<crate::error::Error>,
    }

    impl<'a> Ctx<'a> {
        pub fn members(&self) -> &[UnitPath] {
            &self.members
        }

        pub fn unit_dir(&self) -> &UnitPath {
            self.dir.as_ref().expect("Not in unit context")
        }

        pub fn home_dir(&self) -> Result<&Path, common::rel_path::HomeError> {
            self.home_dir
        }

        pub fn dotfile_dir(&self) -> &RelPath {
            self.dotfile_dir.expect("Not in unit context")
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

        pub fn eval_config(
            &self,
            src: &str,
            home_dir: Result<&Path, common::rel_path::HomeError>,
        ) -> (Config, Vec<Error>) {
            let mut ctx = Ctx {
                members: Vec::new(),
                dir: None,
                home_dir,
                dotfile_dir: None,
                root: false,
                errors: Vec::new(),
            };
            let env = &HashMap::new();

            let (blocks, errors) = self.parser.parse(src);
            for err in errors {
                ctx.emit(err);
            }

            let mut dotfiles = None;
            let mut shell = None;

            for block in blocks {
                let block = block.inner;
                let ident = block.ident;
                let args = Args::from_item(&ident, block.params, &env);
                match ident.inner.0.as_str() {
                    "config" => {
                        let _ = NoParams::recover_default(args, &mut ctx);
                        match block.body.inner {
                            Body::Map(body) => {
                                let body =
                                    Args::from_exprs(Spanned::new(body, block.body.span), &env);
                                let config = Config::recover_default(body, &mut ctx);

                                dotfiles = config.value.dotfiles;
                                shell = config.value.shell;
                            }
                            other => ctx.emit(StructureError::WrongType {
                                span: block.body.span,
                                found: BlockType::from(other),
                                expected: BlockType::Map,
                            }),
                        }
                    }
                    _ => ctx.emit(StructureError::UnexpectedBlock {
                        span: ident.span,
                        found: ident.inner,
                        expected: vec!["config"],
                    }),
                }
            }

            (Config { dotfiles, shell }, ctx.errors)
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
            let mut ctx = Ctx {
                members: Vec::new(),
                dir: Some(path),
                home_dir,
                dotfile_dir: Some(dotfile_dir),
                root,
                errors: Vec::new(),
            };

            let (blocks, errors) = self.parser.parse(src);
            for err in errors {
                ctx.emit(err);
            }

            let mut unit_name = None;
            let mut desc = None;
            let mut topic = None;
            let mut shell = None;
            let mut members = None;

            let mut transactions = Vec::new();
            let mut deploy = Vec::new();
            let mut remove = Vec::new();
            let mut capture = Vec::new();

            for (i, block) in blocks.into_iter().enumerate() {
                let block = block.inner;
                let ident = block.ident;
                let args = Args::from_item(&ident, block.params, &env);
                let name = match Name::from_ident(&ident.inner) {
                    Some(name) => name,
                    None => {
                        ctx.emit(StructureError::UnexpectedBlock {
                            span: ident.span,
                            found: ident.inner,
                            expected: vec![
                                "unit", "env", "packages", "files", "deploy", "remove", "capture",
                            ],
                        });

                        continue;
                    }
                };

                match name {
                    Name::Unit if i == 0 => (),
                    Name::Unit => ctx.emit(StructureError::MisplacedUnit(ident.span)),
                    Name::Env if i > 1 => ctx.emit(StructureError::MisplacedEnv(ident.span)),
                    _ if i == 0 => ctx.emit(StructureError::ExpectedUnit(ident.span, ident.inner)),
                    _ => (),
                }

                match (name, block.body.inner) {
                    (Name::Unit, Body::Map(body)) => {
                        let _ = NoParams::recover_default(args, &mut ctx);
                        let body = Args::from_exprs(Spanned::new(body, block.body.span), &env);
                        let header = UnitHeader::recover_default(body, &mut ctx).value;
                        unit_name = header.name;
                        desc = header.desc;
                        topic = header.topic;
                        shell = header.shell;
                        members = header.members;
                        if let Some(members) = members.as_ref() {
                            ctx.members = members.clone();
                        }
                    }
                    (Name::Env, Body::Map(body)) => {
                        let _ = NoParams::recover_default(args, &mut ctx);
                        for arg in body {
                            let name = arg.inner.name.inner;
                            let val = arg.inner.val.map(|val| Value::from_expr(val, &env));

                            if let Value::Error(err) = &val.inner {
                                ctx.emit(Spanned::new(err.clone(), val.span));
                            }
                            env.insert(name, val);
                        }
                    }
                    (Name::Files | Name::Packages, Body::List(items)) => {
                        let packages = Packages::from_exprs(
                            Spanned::new(items, block.body.span),
                            &env,
                            &mut ctx,
                        );
                        let res = match name {
                            Name::Files => manager.new_files(ident.span, args, packages, &mut ctx),
                            Name::Packages => manager.new_transaction(args, packages, &mut ctx),
                            _ => unreachable!(),
                        };
                        if let Ok(trans) = res {
                            transactions.push(trans)
                        }
                    }
                    (Name::Deploy | Name::Remove | Name::Capture, Body::Code(code)) => {
                        let params = RoutineParams::recover_default(args, &mut ctx).value;
                        let routine = RoutineFigment {
                            shell: params.shell,
                            stdout: params.stdout,
                            workdir: params.workdir,
                            body: code,
                        };
                        match name {
                            Name::Deploy => deploy.push(routine),
                            Name::Remove => remove.push(routine),
                            Name::Capture => capture.push(routine),
                            _ => unreachable!(),
                        }
                    }
                    (name, other) => ctx.emit(StructureError::WrongType {
                        span: block.body.span,
                        found: BlockType::from(other),
                        expected: match name {
                            Name::Unit => BlockType::Map,
                            Name::Env => BlockType::Map,
                            Name::Files => BlockType::List,
                            Name::Packages => BlockType::List,
                            Name::Deploy => BlockType::Code,
                            Name::Remove => BlockType::Code,
                            Name::Capture => BlockType::Code,
                        },
                    }),
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
                errors: ctx.errors,
            }
        }
    }
}

fn new_packages(
    items: Spanned<Vec<Spanned<parser::Expr>>>,
    env: &eval::Env,
    ctx: &mut eval::Ctx,
) -> provider::Packages {
    provider::Packages {
        packages: items
            .inner
            .into_iter()
            .filter_map(|item| match Value::from_expr(item.inner, env) {
                Value::String(name) => Some(Package {
                    name: Spanned::new(name.clone(), item.span),
                    args: Args::from_item(&Spanned::new(parser::Ident(name), item.span), None, env),
                    span: item.span,
                }),
                Value::Item(ident, args) => Some(Package {
                    name: ident.map(|ident| ident.0),
                    args,
                    span: item.span,
                }),
                other => {
                    ctx.emit(eval::ConvertError::TypeErr {
                        span: item.span,
                        // TODO: Better handle expected types
                        // TODO: Improve errors when the type is a value of a variable
                        expected: eval::Type::String,
                        found: other.get_type(),
                    });
                    None
                }
            })
            .collect(),

        span: items.span,
    }
}
