use chrono::NaiveDate;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};
use std::str::FromStr;

#[cfg(feature = "serde")]
use serde::ser::SerializeStruct;
#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::parser::{self, Token};

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
    pub x: Option<Token<char>>,
    pub priority: Option<Token<Priority>>,
    pub completed_on: Option<Token<NaiveDate>>,
    pub started_on: Option<Token<NaiveDate>>,
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
        let input = description.value();

        parser::tags(description.start(), input)
    }

    /// True if the todo starts with a lowercase "x" or has a `completed` date.
    ///
    pub fn is_done(&self) -> bool {
        matches!(
            self,
            Self { x: Some(_), .. }
                | Self {
                    completed_on: Some(_),
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
        struct DebugTags<'a>(&'a Token<Cow<'a, str>>);

        impl Debug for DebugTags<'_> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                let Self(token) = *self;
                let input = token.value();
                let tags = parser::tags(token.start(), input);

                f.debug_list().entries(tags).finish()
            }
        }

        let description = &self.description;

        fmt.debug_struct("Todo")
            .field("line", &self.line)
            .field("x", &self.x)
            .field("priority", &self.priority)
            .field("completed_on", &self.completed_on)
            .field("started_on", &self.started_on)
            .field("description", description)
            .field("tags", &DebugTags(description))
            .finish()
    }
}

impl Display for Task<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let x = &self.x;
        let priority = &self.priority;
        let completed = &self.completed_on;
        let started = &self.started_on;
        let description = &self.description;

        if x.is_some() {
            write!(f, "x ")?;
        }

        if let Some(token) = priority.as_ref() {
            write!(f, "{} ", token.value())?;
        }

        if let Some(token) = completed.as_ref() {
            write!(f, "{} ", token.value())?;
        }

        if let Some(token) = started.as_ref() {
            write!(f, "{} ", token.value())?;
        }

        f.write_str(description.value())
    }
}

impl FromStr for Task<'static> {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match parser::task(input.into()) {
            Ok((_, Some(task))) => Ok(task.into_owned()),
            _ => Err("unexpected end of input".to_owned()), // Allocation
        }
    }
}

#[cfg(feature = "serde")]
impl Serialize for Task<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        struct SerializeTags<'a>(&'a Token<Cow<'a, str>>);

        impl Serialize for SerializeTags<'_> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                let Self(token) = *self;
                let input = token.value();
                let tags = parser::tags(token.start(), input);

                serializer.collect_seq(tags)
            }
        }

        let mut state = serializer.serialize_struct("Todo", 1)?;
        let description = &self.description;

        state.serialize_field("x", &self.x)?;
        state.serialize_field("priority", &self.priority)?;
        state.serialize_field("completed_on", &self.completed_on)?;
        state.serialize_field("started_on", &self.started_on)?;
        state.serialize_field("description", description)?;
        state.serialize_field("tags", &SerializeTags(description))?;

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
