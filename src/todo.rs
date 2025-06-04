use chrono::NaiveDate;
use std::borrow::Cow;
use std::cmp::Ordering;

#[cfg(feature = "serde")]
use serde::ser::SerializeStruct;
#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::parse::Input;
use crate::tag::{tags, Tag};

/// TODO: docs
///
#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Priority(pub(crate) Located<char>);

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Located<T> {
    pub data: T,
    pub span: Span,
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd)]
pub struct Span {
    line: u32,
    offset: (usize, usize),
}

/// A task from a todo list.
///
#[derive(Clone, Debug)]
pub struct Todo<'a> {
    pub checkmark: Option<Span>,
    pub priority: Option<Priority>,
    pub completed: Option<Located<NaiveDate>>,
    pub started: Option<Located<NaiveDate>>,
    pub description: Located<Cow<'a, str>>,

    pub(crate) _priv: (),
}

impl Priority {
    /// Returns the `char` that is used for prioritization and ordering.
    ///
    pub fn rank(&self) -> char {
        self.0.data
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
    /// The line number of the associated item.
    ///
    pub fn line(&self) -> u32 {
        self.line
    }

    /// The byte offset of the first character of the associated item.
    ///
    pub fn start(&self) -> usize {
        self.offset.0
    }

    /// The byte offset of the last character of the associated item.
    ///
    pub fn end(&self) -> usize {
        self.offset.1
    }
}

impl Span {
    pub(crate) fn new(line: u32, offset: (usize, usize)) -> Self {
        Self { line, offset }
    }

    pub(crate) fn locate(at: &Input, len: usize) -> Self {
        let start = at.get_utf8_column() - 1;

        Self {
            line: at.location_line(),
            offset: (start, start + len),
        }
    }
}

impl Todo<'_> {
    /// Returns a reference to the value of the todo's description.
    ///
    pub fn description(&self) -> &str {
        self.description.data.as_ref()
    }

    /// True if the todo starts with a lowercase "x" or has a `completed` date.
    ///
    pub fn is_done(&self) -> bool {
        self.checkmark.is_some() || self.completed.is_some()
    }

    /// Returns an iterator over the tags in the todo's description.
    ///
    pub fn tags(&self) -> impl Iterator<Item = Tag> {
        let description = &self.description;
        tags(description.span, description.data.as_ref())
    }
}

impl<'a> Todo<'a> {
    /// Returns a clone of self with the description allocated on the heap.
    ///
    pub fn into_owned(self) -> Todo<'static> {
        let description = self.description;

        Todo {
            description: Located {
                data: Cow::Owned(description.data.into_owned()),
                span: description.span,
            },
            ..self
        }
    }
}

#[cfg(feature = "serde")]
impl Serialize for Todo<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use crate::tag::SerializeTags;

        let mut state = serializer.serialize_struct("Todo", 1)?;
        let tags = SerializeTags(self.description.span, self.description());

        state.serialize_field("checkmark", &self.checkmark)?;
        state.serialize_field("priority", &self.priority)?;
        state.serialize_field("completed", &self.completed)?;
        state.serialize_field("started", &self.started)?;
        state.serialize_field("description", &self.description)?;
        state.serialize_field("tags", &tags)?;

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
            span: Span::new(0, (0, 0)),
        });

        let b = Priority(Located {
            data: 'B',
            span: Span::new(0, (0, 0)),
        });

        assert!(
            a > b,
            "Priority should be ordered alphabetically in descending order."
        )
    }
}
