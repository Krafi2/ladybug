use parser::span::AriadneSpan;

use super::Spanned;

enum Inner {
    Value(Spanned<crate::ValueError>),
    Parse(parser::Error),
    Convert(crate::ConvertError),
    Param(crate::ParamError),
    Other(Box<dyn IntoReportBoxed + 'static>),
}

impl std::fmt::Debug for Inner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Inner::Value(err) => f.debug_tuple("Value").field(err).finish(),
            Inner::Parse(err) => f.debug_tuple("Parse").field(err).finish(),
            Inner::Convert(err) => f.debug_tuple("Convert").field(err).finish(),
            Inner::Param(err) => f.debug_tuple("Param").field(err).finish(),
            Inner::Other(_) => f.debug_tuple("Other").field(&"...").finish(),
        }
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
impl From<crate::ParamError> for Inner {
    fn from(err: crate::ParamError) -> Self {
        Self::Param(err)
    }
}
impl From<crate::ConvertError> for Inner {
    fn from(err: crate::ConvertError) -> Self {
        Self::Convert(err)
    }
}
impl From<Box<dyn IntoReportBoxed + 'static>> for Inner {
    fn from(err: Box<dyn IntoReportBoxed + 'static>) -> Self {
        Self::Other(err)
    }
}

#[derive(Debug)]
pub struct Error(Inner);

impl<T: Into<Inner>> From<T> for Error {
    fn from(err: T) -> Self {
        Error(err.into())
    }
}

impl IntoReport for Error {
    fn into_report(self, filename: &str) -> ariadne::Report<AriadneSpan> {
        match self.0 {
            Inner::Parse(err) => err.into_report(filename),
            Inner::Convert(err) => err.into_report(filename),
            Inner::Value(err) => err.into_report(filename),
            Inner::Param(err) => err.into_report(filename),
            Inner::Other(err) => err.into_report_boxed(filename),
        }
    }
}

/// Allows an error to be converted into a Report. Automatically implemented by
/// the `report!` macro.
pub trait IntoReport {
    /// Create a report given the filename of the error's source
    fn into_report(self, filename: &str) -> ariadne::Report<AriadneSpan>;

    /// Make thir error into a boxed trait object
    fn boxed(self) -> Box<dyn IntoReportBoxed>
    where
        Self: Sized + 'static,
    {
        Box::new(self)
    }
}

/// Like `IntoReport` but consumes `Box<Self>` to allow generating reports from
/// boxed trait objects.
pub trait IntoReportBoxed: IntoReport {
    fn into_report_boxed(self: Box<Self>, filename: &str) -> ariadne::Report<AriadneSpan>;
}

impl<T: IntoReport> IntoReportBoxed for T {
    fn into_report_boxed(self: Box<Self>, filename: &str) -> ariadne::Report<AriadneSpan> {
        self.into_report(filename)
    }
}

impl<'a, T: IntoReportBoxed + 'a> From<T> for Box<dyn IntoReportBoxed + 'a> {
    fn from(value: T) -> Self {
        Box::new(value)
    }
}

impl IntoReport for parser::Error {
    fn into_report(self, filename: &str) -> ariadne::Report<AriadneSpan> {
        // This is a different method
        self.into_report(filename)
    }
}

/// Declaratively generate a report
#[macro_export]
macro_rules! report {
    ( __init__ $filename:ident, delegate ( $error:expr $(,)? ) ) => {
         return $crate::IntoReport::into_report($error, $filename)
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
        impl $crate::IntoReport for $type {
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
