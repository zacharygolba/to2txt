use chrono::NaiveDate;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};

#[cfg(feature = "serde")]
use serde::ser::SerializeStruct;
#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::parser::{self, Located, Span};

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
#[rustfmt::skip]
#[repr(u8)]
pub enum Priority {
    A = 65, B, C, D, E, F, G, H, I, J, K, L, M,
    N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

#[cfg_attr(feature = "serde", derive(Serialize), serde(tag = "type"))]
#[derive(Clone, Debug, PartialEq)]
pub enum Tag<'a> {
    Context(Located<&'a str>),
    Project(Located<&'a str>),
    Named(Located<(&'a str, &'a str)>),
}

/// A task from a todo list.
///
#[derive(Clone)]
pub struct Task<'a> {
    pub(crate) line: u32,
    pub(crate) x: Option<Span>,
    pub(crate) priority: Option<Located<Priority>>,
    pub(crate) completed: Option<Located<NaiveDate>>,
    pub(crate) started: Option<Located<NaiveDate>>,
    pub(crate) description: Located<Cow<'a, str>>,
}

impl Display for Priority {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&((*self as u8) as char), f)
    }
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some((*self as u8).cmp(&(*other as u8)).reverse())
    }
}

impl Task<'_> {
    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn x(&self) -> Option<&Span> {
        self.x.as_ref()
    }

    pub fn priority(&self) -> Option<&Located<Priority>> {
        self.priority.as_ref()
    }

    pub fn completed(&self) -> Option<&Located<NaiveDate>> {
        self.completed.as_ref()
    }

    pub fn started(&self) -> Option<&Located<NaiveDate>> {
        self.started.as_ref()
    }

    pub fn description(&self) -> Located<&str> {
        let description = &self.description;

        Located {
            value: description.value.as_ref(),
            span: description.span.clone(),
        }
    }

    /// Returns an iterator over the tags in the todo's description.
    ///
    pub fn tags(&self) -> impl Iterator<Item = Tag<'_>> {
        let description = &self.description;
        let offset = description.span().start();

        parser::parse_tags(offset, description.value())
    }

    /// True if the todo starts with a lowercase "x" or has a `date_completed`.
    ///
    pub fn is_done(&self) -> bool {
        matches!(
            self,
            Self { x: Some(_), .. }
                | Self {
                    completed: Some(_),
                    ..
                }
        )
    }

    /// Returns a clone of self with the description allocated on the heap.
    ///
    pub fn into_owned(self) -> Task<'static> {
        Task {
            description: self.description.map(|value| Cow::Owned(value.into_owned())),
            ..self
        }
    }
}

impl Debug for Task<'_> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        struct DebugTags<'a> {
            todo: &'a Task<'a>,
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
            .field("completed", &self.completed)
            .field("started", &self.started)
            .field("description", &self.description)
            .field("tags", &DebugTags { todo: self })
            .finish()
    }
}

impl Display for Task<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.x.is_some() {
            write!(f, "x ")?;
        }

        if let Some(priority) = self.priority.as_ref() {
            write!(f, "({}) ", priority.value())?;
        }

        if let Some(completed) = self.completed.as_ref() {
            write!(f, "{} ", completed.value(),)?;
        }

        if let Some(started) = self.started.as_ref() {
            write!(f, "{} ", started.value())?;
        }

        f.write_str(self.description.value())
    }
}

#[cfg(feature = "serde")]
impl Serialize for Task<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        struct SerializeTags<'a> {
            todo: &'a Task<'a>,
        }

        impl Serialize for SerializeTags<'_> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                serializer.collect_seq(self.todo.tags())
            }
        }

        let mut state = serializer.serialize_struct("Todo", 1)?;

        state.serialize_field("x", &self.x)?;
        state.serialize_field("priority", &self.priority)?;
        state.serialize_field("completed", &self.completed)?;
        state.serialize_field("started", &self.started)?;
        state.serialize_field("description", &self.description)?;
        state.serialize_field("tags", &SerializeTags { todo: &self })?;

        state.end()
    }
}

#[cfg(test)]
mod tests {
    use super::Priority;

    #[test]
    fn test_priority_order() {
        assert!(
            Priority::A > Priority::B,
            "Priority should be ordered alphabetically in descending order."
        )
    }
}
