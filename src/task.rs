use chrono::NaiveDate;
use nom::Parser;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};
use std::str::FromStr;

#[cfg(feature = "serde")]
use serde::ser::SerializeStruct;
#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::parser::{self, Token};
use crate::Span;

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
#[rustfmt::skip]
#[repr(u8)]
pub enum Priority {
    A = 65, B, C, D, E, F, G, H, I, J, K, L, M,
    N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

#[cfg_attr(
    feature = "serde",
    derive(Serialize),
    serde(tag = "type", rename_all = "lowercase")
)]
#[derive(Clone, Debug, PartialEq)]
pub enum Tag<'a> {
    Context(Token<&'a str>),
    Project(Token<&'a str>),
    Named(Token<(&'a str, &'a str)>),
}

/// A task from a todo list.
///
#[derive(Clone)]
#[non_exhaustive]
pub struct Task<'a> {
    pub line: u32,
    pub x: Option<Span>,
    pub priority: Option<Token<Priority>>,
    pub completed: Option<Token<NaiveDate>>,
    pub started: Option<Token<NaiveDate>>,
    pub description: Token<Cow<'a, str>>,
}

impl Display for Priority {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({})", (*self as u8) as char)
    }
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (*self as u8)
            .partial_cmp(&(*other as u8))
            .map(Ordering::reverse)
    }
}

impl Task<'_> {
    /// Returns an iterator over the tags in the todo's description.
    ///
    pub fn tags(&self) -> impl Iterator<Item = Tag<'_>> {
        let description = &self.description;
        let offset = description.span().start();

        parser::parse_tags(offset, description.value())
    }

    /// True if the todo starts with a lowercase "x" or has a `completed` date.
    ///
    pub fn is_complete(&self) -> bool {
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
            description: self.description.map(|value| {
                Cow::Owned(value.into_owned()) // Allocation
            }),
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
        let x = &self.x;
        let priority = &self.priority;
        let completed = &self.completed;
        let started = &self.started;
        let description = &self.description;

        if x.is_some() {
            write!(f, "x ")?;
        }

        if let Some(value) = priority.as_ref().map(Token::value) {
            write!(f, "{} ", value)?;
        }

        if let Some(value) = completed.as_ref().map(Token::value) {
            write!(f, "{} ", value)?;
        }

        if let Some(value) = started.as_ref().map(Token::value) {
            write!(f, "{} ", value)?;
        }

        f.write_str(description.value())
    }
}

impl FromStr for Task<'static> {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match parser::parse_task().parse(input.into()) {
            Ok((_, Some(task))) => Ok(task.into_owned()),
            _ => Err("unexpected end of input".to_owned()),
        }
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
