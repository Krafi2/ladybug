#[macro_use]
pub mod structures;
mod provider;

pub use provider::Transaction;
use std::{collections::HashMap, path::Path};

use crate::context::Context;

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
    VarError(Box<Spanned<EvalError>>),
}

impl Value {
    fn from_expr(expr: parser::Expr, env: &Env) -> Self {
        match expr {
            parser::Expr::Variable((v, span)) => match env.get(&v) {
                Some(v) => match v {
                    Value::Error(e) => {
                        Value::Error((EvalError::VarError(Box::new(e.clone())), span))
                    }
                    v => v.clone(),
                },
                None => Value::Error((EvalError::UnknownVar(v), span)),
            },
            parser::Expr::String(s) => Self::String(s),
            parser::Expr::Bool(b) => Self::Bool(b),
            parser::Expr::List((list, span)) => Self::List((
                list.into_iter().map(|e| Self::from_expr(e, env)).collect(),
                span,
            )),
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

impl Value {
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
    value: crate::topic::interpreter::Value,
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

enum Error {
    Eval(Spanned<EvalError>),
    Parse(parser::Error),
    Param(structures::ParamError),
    Provider(provider::ProviderError),
    Transaction(provider::TransactionError),
}

impl From<Spanned<EvalError>> for Error {
    fn from(err: Spanned<EvalError>) -> Self {
        Self::Eval(err)
    }
}
impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Self {
        Self::Parse(err)
    }
}
impl From<structures::ParamError> for Error {
    fn from(err: structures::ParamError) -> Self {
        Self::Param(err)
    }
}
impl From<provider::ProviderError> for Error {
    fn from(err: provider::ProviderError) -> Self {
        Self::Provider(err)
    }
}
impl From<provider::TransactionError> for Error {
    fn from(err: provider::TransactionError) -> Self {
        Self::Transaction(err)
    }
}

impl Error {
    fn into_report(self, file: &Path) -> ariadne::Report {
        todo!()
    }
}

struct ErrorId(usize);

struct Emitter {
    errors: Vec<Error>,
}

impl Emitter {
    pub fn emit<E: Into<Error>>(&mut self, error: E) -> ErrorId {
        let id = self.errors.len();
        self.errors.push(error.into());
        ErrorId(id)
    }
}

params! { struct NoParams {} }

params! {
    struct TopicData {
        name: String,
        desc: String,
        target: crate::rel_path::RelPath,
        shell: Vec<String>,
        children: Vec<String>,
    }
}

params! {
    struct RoutineParams {
        shell: Option<Vec<String>>,
        stdin: Option<bool>,
        stdout: Option<bool>,
    }
}

fn eval(
    src: &str,
    manager: &mut provider::Manager,
    mut env: Env,
    eval_ctx: &mut super::EvalContext,
    context: &crate::context::Context,
) -> Result<super::Topic, Vec<Error>> {
    let mut emitter = Emitter { errors: Vec::new() };

    let (blocks, errors) = parser::parse(src);
    let mut topic_name = None;
    let mut desc = None;
    let mut target = None;
    let mut shell = None;
    let mut children = None;
    let mut transactions = Vec::new();
    let mut deploy = None;
    let mut remove = None;
    let mut capture = None;

    for block in blocks {
        match block {
            parser::Block::Map { name, params, body } => match name {
                parser::Name::Topic => {
                    let _ = parse_args::<NoParams>(params, &env, &mut emitter, context);
                    if let Ok(topic_data) =
                        parse_args::<TopicData>(body, &env, &mut emitter, context)
                    {
                        topic_name = Some(topic_data.name);
                        desc = Some(topic_data.desc);
                        target = Some(topic_data.target);
                        shell = Some(topic_data.shell);
                        children = Some(topic_data.children);
                    }
                }
                parser::Name::Env => {
                    let _ = parse_args::<NoParams>(params, &env, &mut emitter, context);
                    for (param, span) in body.0 {
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
                    parser::Name::Files => manager.new_files(args, packages, &mut emitter, context),
                    parser::Name::Packages => {
                        manager.new_transaction(args, packages, &mut emitter, context)
                    }
                    _ => panic!("Item block has invalid name"),
                };

                match res {
                    Ok(trans) => transactions.push(trans),
                    Err(err) => match err {
                        provider::Error::Provider(_) => todo!(),
                        provider::Error::Param(_) => todo!(),
                        provider::Error::Emitted => (),
                    },
                }
            }
            parser::Block::Routine {
                name,
                params,
                content,
            } => {
                if let Ok(params) = parse_args::<RoutineParams>(params, &env, &mut emitter, context)
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

    // let target = crate::rel_path::RelPath::new(target.unwrap().0, context) {
    //     Ok(target) => target,
    //     Err(_) => todo!(),
    // }

    if emitter.errors.is_empty() {
        Ok(super::Topic {
            name: topic_name.unwrap(),
            desc: desc.unwrap(),
            target: target.unwrap(),
            shell: shell.map(crate::shell::Shell::from_vec),
            children: children
                .unwrap()
                .into_iter()
                .map(|topic| eval_ctx.register_topic(topic))
                .collect(),
            env,
            transactions,
            deploy,
            remove,
            capture,
        })
    } else {
        Err(emitter.errors)
    }
}

fn collect_errors<I: IntoIterator<Item = Result<A, B>>, A, B>(iter: I) -> Result<Vec<A>, Vec<B>> {
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
