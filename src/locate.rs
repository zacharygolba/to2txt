use std::cmp::Ordering;
use std::fmt::{self, Debug, Formatter};
use std::mem;

use crate::parser::Input;

#[cfg(feature = "serde")]
use serde::Serialize;

pub trait Translate {
    fn origin(&self) -> Option<&Span>;
    fn translate_l(&mut self, len: usize);
    fn translate_r(&mut self, len: usize);

    fn abs_distance<T: Translate>(&self, other: &T) -> Option<usize> {
        other.origin()?.start().checked_sub(self.origin()?.end())
    }
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, PartialEq, PartialOrd)]
pub struct Span(usize, usize);

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug)]
pub struct Located<T> {
    pub(crate) value: T,
    pub(crate) span: Span,
}

impl<T> Located<T> {
    /// The location of the associated item.
    ///
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// A reference to value of the associated item.
    ///
    pub fn value(&self) -> &T {
        &self.value
    }
}

impl<T> Located<T> {
    pub(crate) const fn new(span: Span, value: T) -> Self {
        Self { span, value }
    }

    pub(crate) fn replace(&mut self, value: T) -> T {
        mem::replace(&mut self.value, value)
    }

    pub(crate) fn map<U, F>(self, f: F) -> Located<U>
    where
        F: FnOnce(T) -> U,
    {
        Located {
            span: self.span,
            value: f(self.value),
        }
    }
}

impl<T: PartialEq> PartialEq for Located<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value().eq(other.value())
    }
}

impl<T: PartialOrd> PartialOrd for Located<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value().partial_cmp(other.value())
    }
}

impl Span {
    pub fn start(&self) -> usize {
        self.0
    }

    pub fn end(&self) -> usize {
        self.1
    }
}

impl Span {
    pub(crate) fn new(start: usize, end: usize) -> Self {
        Self(start, end)
    }

    pub(crate) fn move_right(self, len: usize) -> Self {
        Self(self.0 + len, self.1 + len)
    }

    pub(crate) fn locate(input: &Input, len: usize) -> Self {
        let start = input.get_utf8_column() - 1;
        Self(start, start + len)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Span({}, {})", self.start(), self.end())
    }
}

impl Translate for Span {
    fn origin(&self) -> Option<&Span> {
        Some(self)
    }

    fn translate_l(&mut self, len: usize) {
        self.0 -= len;
        self.1 -= len;
    }

    fn translate_r(&mut self, len: usize) {
        self.0 += len;
        self.1 += len;
    }
}

impl<T> Translate for Located<T> {
    fn origin(&self) -> Option<&Span> {
        Some(self.span())
    }

    fn translate_l(&mut self, len: usize) {
        self.span.translate_l(len);
    }

    fn translate_r(&mut self, len: usize) {
        self.span.translate_r(len);
    }
}

impl<T: Translate> Translate for Option<T> {
    fn origin(&self) -> Option<&Span> {
        self.as_ref().and_then(Translate::origin)
    }

    fn translate_l(&mut self, len: usize) {
        if let Some(location) = self {
            location.translate_l(len);
        }
    }

    fn translate_r(&mut self, len: usize) {
        if let Some(location) = self {
            location.translate_r(len);
        }
    }
}

impl<T: Translate> Translate for &'_ mut T {
    fn origin(&self) -> Option<&Span> {
        T::origin(self)
    }

    fn translate_l(&mut self, len: usize) {
        T::translate_l(self, len)
    }

    fn translate_r(&mut self, len: usize) {
        T::translate_r(self, len)
    }
}

impl Translate for () {
    fn translate_l(&mut self, _: usize) {}
    fn translate_r(&mut self, _: usize) {}

    fn origin(&self) -> Option<&Span> {
        None
    }
}

macro_rules! impl_translate_tuple {
    ( $first:ident, $( $name:ident ),+ ) => {
        #[allow(non_snake_case)]
        impl< $first, $($name),+ > Translate
            for ( $first, $($name,)+ )
        where
            $first: Translate,
            $($name: Translate,)+
        {
            fn origin(&self) -> Option<&Span> {
                let ( $first, $($name,)+ ) = self;
                $first.origin()$(.or_else(|| $name.origin()))+
            }

            fn translate_l(&mut self, len: usize) {
                let ( $first, $($name,)+ ) = self;
                $first.translate_l(len);
                $($name.translate_l(len);)+
            }

            fn translate_r(&mut self, len: usize) {
                let ( $first, $($name,)+ ) = self;
                $first.translate_r(len);
                $($name.translate_r(len);)+
            }
        }
    };
}

impl_translate_tuple!(A, B);
impl_translate_tuple!(A, B, C);
impl_translate_tuple!(A, B, C, D);
