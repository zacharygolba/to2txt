use chrono::NaiveDate;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Formatter};

#[cfg(feature = "serde")]
use serde::ser::SerializeStruct;
#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::parser::{self, Located, Span};

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

/// A task from a todo list.
///
#[derive(Clone)]
#[non_exhaustive]
pub struct Todo<'a> {
    pub line: u32,
    pub x: Option<Span>,
    pub priority: Option<Priority>,
    pub date_completed: Option<Located<NaiveDate>>,
    pub date_started: Option<Located<NaiveDate>>,
    pub description: Description<'a>,
}

impl Description<'_> {
    pub fn span(&self) -> &Span {
        self.0.span()
    }

    /// A reference to the description text.
    ///
    pub fn value(&self) -> &str {
        self.0.value.as_ref()
    }

    /// Returns an owned string containing the description text.
    ///
    pub fn into_string(self) -> String {
        self.0.value.into_owned()
    }
}

impl Priority {
    pub fn span(&self) -> &Span {
        self.0.span()
    }

    /// A reference to the ASCII uppercase character that is used for
    /// priority-based ordering.
    ///
    pub fn value(&self) -> &char {
        &self.0.value
    }
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value()
            .partial_cmp(other.value())
            .map(Ordering::reverse)
    }
}

impl Todo<'_> {
    /// True if the todo starts with a lowercase "x" or has a `date_completed`.
    ///
    pub fn is_done(&self) -> bool {
        matches!(
            self,
            Self { x: Some(_), .. }
                | Self {
                    date_completed: Some(_),
                    ..
                }
        )
    }

    /// Returns an iterator over the tags in the todo's description.
    ///
    pub fn tags(&self) -> impl Iterator<Item = Tag<'_>> {
        parser::parse_tags(&self.description)
    }

    /// Returns a clone of self with the description allocated on the heap.
    ///
    pub fn into_owned(self) -> Todo<'static> {
        let Description(Located { value: data, span }) = self.description;

        Todo {
            description: Description(Located {
                value: Cow::Owned(data.into_owned()),
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
            .field("x", &self.x)
            .field("priority", &self.priority)
            .field("date_completed", &self.date_completed)
            .field("date_started", &self.date_started)
            .field("description", &self.description)
            .field("tags", &DebugTags { todo: self })
            .finish()
    }
}

#[cfg(feature = "serde")]
impl Serialize for Todo<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        struct SerializeTags<'a> {
            todo: &'a Todo<'a>,
        }

        impl Serialize for SerializeTags<'_> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                serializer.collect_seq(self.todo.tags())
            }
        }

        let mut state = serializer.serialize_struct("Todo", 1)?;

        state.serialize_field("x", &self.x)?;
        state.serialize_field("priority", &self.priority)?;
        state.serialize_field("date_completed", &self.date_completed)?;
        state.serialize_field("date_started", &self.date_started)?;
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
            value: 'A',
            span: Span::new(0, 0),
        });

        let b = Priority(Located {
            value: 'B',
            span: Span::new(0, 0),
        });

        assert!(
            a > b,
            "Priority should be ordered alphabetically in descending order."
        )
    }
}
