use super::{provider, structures, Span, Spanned};

#[derive(Debug)]
enum Inner {
    Eval(Spanned<super::EvalError>),
    Parse(parser::Error),
    Convert(structures::ConvertError),
    Param(structures::ParamError),
    Transaction(provider::TransactionError),
    Other(color_eyre::Report),
}

impl From<Spanned<super::EvalError>> for Inner {
    fn from(err: Spanned<super::EvalError>) -> Self {
        Self::Eval(err)
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
    fn into_report(self, filename: &str) -> ariadne::Report<(&str, Span)>;
}

impl IntoReport for parser::Error {
    fn into_report(self, filename: &str) -> ariadne::Report<(&str, Span)> {
        // This is a function, not a trait
        self.into_report(filename)
    }
}

impl Error {
    pub fn into_report<'a>(self, filename: &'a str) -> ariadne::Report<(&'a str, Span)> {
        match self.0 {
            Inner::Parse(err) => err.into_report(filename),
            Inner::Convert(err) => err.into_report(filename),
            Inner::Eval(err) => err.into_report(filename),
            Inner::Param(err) => err.into_report(filename),
            Inner::Transaction(err) => err.into_report(filename),
            Inner::Other(_err) => todo!(),
        }
    }

    pub fn custom(err: color_eyre::Report) -> Self {
        Error(Inner::Other(err))
    }
}

macro_rules! report {
    ( $filename:ident, delegate ( $error:expr ) ) => {
         $crate::error::IntoReport::into_report($error, $filename)
    };
    ( $filename:ident, report ( $kind:expr , $start:expr ) $($rest:tt)* ) => {
         report!( $filename, ::ariadne::Report::build($kind, $filename, $start) , $($rest)* )
    };
    ( $filename:ident, $prev:expr, label ( $span:expr , $color:expr , $($message:expr),* ) $($rest:tt)* ) => {
        report!(
            $filename,
            $prev
            .with_label(
                ::ariadne::Label::new(($filename, $span))
                    .with_color($color)
                    .with_message(format!( $($message),* ))
            ),
            $($rest)*
        )
    };
    ( $filename:ident, $prev:expr, message ( $($arg:expr),* ) $($rest:tt)* ) => {
        report!(
            $filename,
            $prev
            .with_message(format!( $($arg),* )),
            $($rest)*
        )
    };
    ( $filename:ident, $prev:expr, ) => { $prev.finish() };
    (  $type:ty { $( $pattern:pat => { $($atr:ident $args:tt;)* } )* } ) => {
        impl $crate::error::IntoReport for $type {
            fn into_report(self, filename: &str) -> ::ariadne::Report<(&str, Span)> {
                match self {
                    $($pattern => report!( filename, $($atr $args)* ) ,)*
                }
            }
        }
    };
}
