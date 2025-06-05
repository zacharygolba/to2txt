use chrono::NaiveDate;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Formatter};

#[cfg(feature = "serde")]
use serde::ser::SerializeStruct;
#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::parser::{self, Input};

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct Description<'a>(pub(crate) Located<Cow<'a, str>>);

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct Priority(pub(crate) Located<char>);

#[cfg_attr(
    feature = "serde",
    derive(Serialize),
    serde(tag = "type", rename_all = "lowercase")
)]
#[derive(Clone, Debug, PartialEq)]
pub enum Tag<'a> {
    Context(Located<&'a str>),
    Project(Located<&'a str>),
    Named(Located<(&'a str, &'a str)>),
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Headers {
    pub x: Option<Span>,
    pub priority: Option<Priority>,
    pub date_completed: Option<Located<NaiveDate>>,
    pub date_started: Option<Located<NaiveDate>>,
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct Located<T> {
    pub(crate) data: T,
    pub(crate) span: Span,
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Span {
    start: usize,
    end: usize,
}

/// A task from a todo list.
///
#[derive(Clone)]
pub struct Todo<'a> {
    pub(crate) line: u32,
    pub(crate) headers: Option<Headers>,
    pub(crate) description: Description<'a>,
}

#[cfg(feature = "serde")]
struct SerializeTags<'a> {
    todo: &'a Todo<'a>,
}

impl Description<'_> {
    /// A reference to the description text.
    ///
    pub fn text(&self) -> &str {
        self.0.data.as_ref()
    }

    pub fn span(&self) -> &Span {
        self.0.span()
    }

    /// Returns an owned string containing the description text.
    ///
    pub fn into_string(self) -> String {
        self.0.data.into_owned()
    }
}

impl<T> Located<T> {
    /// A reference to value of the associated item.
    ///
    pub fn data(&self) -> &T {
        &self.data
    }

    /// The location of the associated item.
    ///
    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl Priority {
    /// Returns the `char` that is used for prioritization and ordering.
    ///
    pub fn rank(&self) -> char {
        self.0.data
    }

    pub fn span(&self) -> &Span {
        self.0.span()
    }
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.rank()
            .partial_cmp(&other.rank())
            .map(Ordering::reverse)
    }
}

impl Span {
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }
}

impl Span {
    pub(crate) fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub(crate) fn locate(at: &Input, len: usize) -> Self {
        let start = at.get_utf8_column() - 1;
        let end = start + len;
        Self { start, end }
    }
}

impl Todo<'_> {
    /// Returns a reference to the value of the todo's description.
    ///
    pub fn description(&self) -> &Description {
        &self.description
    }

    pub fn headers(&self) -> Option<&Headers> {
        self.headers.as_ref()
    }

    /// True if the todo starts with a lowercase "x" or has a `completed` date.
    ///
    pub fn is_done(&self) -> bool {
        self.headers().is_some_and(|headers| match headers {
            Headers { x: Some(_), .. }
            | Headers {
                date_completed: Some(_),
                ..
            } => true,
            _ => false,
        })
    }

    /// The line number of the todo.
    ///
    pub fn line(&self) -> u32 {
        self.line
    }

    /// Returns an iterator over the tags in the todo's description.
    ///
    pub fn tags(&self) -> impl Iterator<Item = Tag> {
        parser::tags(self.description())
    }
}

impl<'a> Todo<'a> {
    /// Returns a clone of self with the description allocated on the heap.
    ///
    pub fn into_owned(self) -> Todo<'static> {
        let Description(Located { data, span }) = self.description;

        Todo {
            description: Description(Located {
                data: Cow::Owned(data.into_owned()),
                span,
            }),
            ..self
        }
    }
}

impl Debug for Todo<'_> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        struct DebugTags<'a> {
            todo: &'a Todo<'a>,
        }

        impl Debug for DebugTags<'_> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                f.debug_list().entries(self.todo.tags()).finish()
            }
        }

        fmt.debug_struct("Todo")
            .field("line", &self.line)
            .field("headers", &self.headers)
            .field("description", &self.description)
            .field("tags", &DebugTags { todo: self })
            .finish()
    }
}

#[cfg(feature = "serde")]
impl Serialize for SerializeTags<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_seq(self.todo.tags())
    }
}

#[cfg(feature = "serde")]
impl Serialize for Todo<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut state = serializer.serialize_struct("Todo", 1)?;
        let mut checkmark = None;
        let mut priority = None;
        let mut completed = None;
        let mut started = None;

        if let Some(headers) = self.headers() {
            checkmark = headers.x.as_ref();
            priority = headers.priority.as_ref();
            completed = headers.date_completed.as_ref();
            started = headers.date_started.as_ref();
        }

        state.serialize_field("checkmark", &checkmark)?;
        state.serialize_field("priority", &priority)?;
        state.serialize_field("completed", &completed)?;
        state.serialize_field("started", &started)?;
        state.serialize_field("description", &self.description)?;
        state.serialize_field("tags", &SerializeTags { todo: &self })?;

        state.end()
    }
}

#[cfg(test)]
mod tests {
    use super::{Located, Priority, Span};

    #[test]
    fn test_priority_order() {
        let a = Priority(Located {
            data: 'A',
            span: Span::new(0, 0),
        });

        let b = Priority(Located {
            data: 'B',
            span: Span::new(0, 0),
        });

        assert!(
            a > b,
            "Priority should be ordered alphabetically in descending order."
        )
    }
}
