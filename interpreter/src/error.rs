use parser::span::AriadneSpan;

use super::{provider, structures, Spanned};

#[derive(Debug)]
enum Inner {
    Structure(crate::StructureError),
    Eval(crate::PathError),
    Value(Spanned<super::ValueError>),
    Parse(parser::Error),
    Convert(structures::ConvertError),
    Param(structures::ParamError),
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
        // This is a function, not a trait
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

macro_rules! report {
    ( __init__ $filename:ident, delegate ( $error:expr $(,)? ) ) => {
         return $crate::error::IntoReport::into_report($error, $filename)
    };
    ( __init__ $filename:ident, report ( $kind:expr , $start:expr $(,)? ) ) => {
         ::ariadne::Report::build($kind, $filename, $start)
    };
    ( __item__ $report:ident, $filename:ident, label ( $span:expr , $color:expr , $($message:expr),* $(,)? )  ) => {
        $report.add_label(
            ::ariadne::Label::new(::parser::span::AriadneSpan::new($filename, $span))
                .with_color($color)
                .with_message(format!( $($message),* ))
        );
    };
    ( __item__ $report:ident, $filename:ident, help ( $($arg:expr),* $(,)? ) ) => {
        $report.set_help(format!( $($arg),* ));
    };
    ( __item__ $report:ident, $filename:ident, message ( $($arg:expr),* $(,)? ) ) => {
        $report.set_message(format!( $($arg),* ));
    };
    ( __item__ $report:ident, $filename:ident, note( $($arg:expr),* $(,)? ) ) => {
        $report.set_note(format!( $($arg),* ));
    };
    ( __cond__ @$cond:expr; $($rest:tt)* ) => {
        if $cond {
            report!{ $($rest)* }
        }
    };
    ( __cond__ $($rest:tt)* ) => {
        report!{ $($rest)* }
    };
    (  $type:ty { $( $pattern:pat => { $init:ident $init_args:tt; $( $( @if $cond:expr; )? $item:ident $args:tt; )* } )* } ) => {
        impl $crate::error::IntoReport for $type {
            fn into_report(self, filename: &str) -> ::ariadne::Report<::parser::span::AriadneSpan> {
                match self {
                    $(
                        $pattern => {
                            #[allow(unused_variables)]
                            let mut report: ::ariadne::ReportBuilder<_> = report!( __init__ filename, $init $init_args );
                            $( report!{ __cond__ $(@$cond;)? __item__ report, filename, $item $args } )*
                            #[allow(unreachable_code)]
                            report.finish()
                        }
                    ,)*
                }
            }
        }
    };
}
