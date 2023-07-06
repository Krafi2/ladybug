use eval::ValueError;
use parser::{span::AriadneSpan, span::Spanned};

#[derive(Debug)]
enum Inner {
    Structure(crate::StructureError),
    Eval(crate::PathError),
    Parse(parser::Error),
    Transaction(provider::TransactionError),
    Other(color_eyre::Report),
}

impl From<crate::StructureError> for Inner {
    fn from(err: crate::StructureError) -> Self {
        Self::Structure(err)
    }
}

impl From<crate::PathError> for Inner {
    fn from(err: crate::PathError) -> Self {
        Self::Eval(err)
    }
}
impl From<Spanned<super::ValueError>> for Inner {
    fn from(err: Spanned<super::ValueError>) -> Self {
        Self::Value(err)
    }
}
impl From<parser::Error> for Inner {
    fn from(err: parser::Error) -> Self {
        Self::Parse(err)
    }
}
impl From<structures::ParamError> for Inner {
    fn from(err: structures::ParamError) -> Self {
        Self::Param(err)
    }
}
impl From<structures::ConvertError> for Inner {
    fn from(err: structures::ConvertError) -> Self {
        Self::Convert(err)
    }
}
impl From<provider::TransactionError> for Inner {
    fn from(err: provider::TransactionError) -> Self {
        Self::Transaction(err)
    }
}

#[derive(Debug)]
pub struct Error(Inner);

impl<T: Into<Inner>> From<T> for Error {
    fn from(err: T) -> Self {
        Error(err.into())
    }
}

pub(super) trait IntoReport {
    fn into_report(self, filename: &str) -> ariadne::Report<AriadneSpan>;
}

impl IntoReport for parser::Error {
    fn into_report(self, filename: &str) -> ariadne::Report<AriadneSpan> {
        // This is a method, not a trait
        self.into_report(filename)
    }
}

impl Error {
    pub fn into_report<'a>(self, filename: &'a str) -> ariadne::Report<AriadneSpan> {
        match self.0 {
            Inner::Structure(err) => err.into_report(filename),
            Inner::Eval(err) => err.into_report(filename),
            Inner::Parse(err) => err.into_report(filename),
            Inner::Convert(err) => err.into_report(filename),
            Inner::Value(err) => err.into_report(filename),
            Inner::Param(err) => err.into_report(filename),
            Inner::Transaction(err) => err.into_report(filename),
            Inner::Other(_err) => todo!(),
        }
    }

    pub fn custom(err: color_eyre::Report) -> Self {
        Error(Inner::Other(err))
    }
}
