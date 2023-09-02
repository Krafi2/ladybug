// TODO: include info about the file this span comes from
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn as_range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

impl From<Span> for std::ops::Range<usize> {
    fn from(val: Span) -> Self {
        val.as_range()
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(range: std::ops::Range<usize>) -> Self {
        Self::new(range.start, range.end)
    }
}

impl chumsky::Span for Span {
    type Context = ();
    type Offset = usize;

    fn new(_context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self::new(range.start, range.end)
    }

    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

pub struct AriadneSpan<'a> {
    pub file: &'a str,
    pub span: Span,
}

impl<'a> AriadneSpan<'a> {
    pub fn new<S: Into<Span>>(file: &'a str, span: S) -> Self {
        Self {
            file,
            span: span.into(),
        }
    }
}

impl<'a> ariadne::Span for AriadneSpan<'a> {
    type SourceId = &'a str;

    fn source(&self) -> &Self::SourceId {
        &self.file
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new<S: Into<Span>>(inner: T, span: S) -> Self {
        Self {
            inner,
            span: span.into(),
        }
    }

    pub fn map<O, F: FnOnce(T) -> O>(self, f: F) -> Spanned<O> {
        Spanned::new(f(self.inner), self.span)
    }
}

impl<T> AsRef<T> for Spanned<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T> AsMut<T> for Spanned<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner && self.span == other.span
    }
}

impl<T: Eq> Eq for Spanned<T> {}

/// A utility trait for binding tokens with their spans.
pub(crate) trait Spanner<I: Clone, O>: chumsky::Parser<I, O>
where
    Self: Sized,
    Self::Error: chumsky::Error<I, Span = Span>,
{
    /// Map the parser's output from `O` to to `(O, Span)`
    fn spanned(self) -> chumsky::combinator::MapWithSpan<Self, fn(O, Span) -> Spanned<O>, O> {
        self.map_with_span(|tok, span| Spanned::new(tok, span))
    }
}

impl<P, I: Clone, O> Spanner<I, O> for P
where
    P: chumsky::Parser<I, O>,
    Self::Error: chumsky::Error<I, Span = Span>,
{
}
