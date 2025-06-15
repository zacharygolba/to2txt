use chrono::NaiveDate;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};
use std::str::FromStr;

#[cfg(feature = "serde")]
use serde::{Serialize, Serializer, ser::SerializeStruct};

use crate::parser::{self, Token};

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
#[rustfmt::skip]
#[repr(u32)]
pub enum Priority {
    A = 65, B, C, D, E, F, G, H, I, J, K, L, M,
    N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

#[cfg_attr(
    feature = "serde",
    derive(Serialize),
    serde(tag = "type", content = "data", rename_all = "lowercase")
)]
#[derive(Clone, Debug)]
pub enum Tag<'a> {
    Context(&'a str),
    Project(&'a str),
    Named(&'a str, &'a str),
}

/// A task from a todo list.
///
#[derive(Clone)]
#[non_exhaustive]
pub struct Task<'a> {
    pub x: Option<Token<bool>>,
    pub line: u32,
    pub priority: Option<Token<Priority>>,
    pub completed_on: Option<Token<NaiveDate>>,
    pub started_on: Option<Token<NaiveDate>>,
    pub description: Token<Cow<'a, str>>,
}

impl Display for Priority {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({})", &((*self as u8) as char))
    }
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(u32::partial_cmp(&(*self as _), &(*other as _))?.reverse())
    }
}

impl Task<'_> {
    /// True when the task starts with a "x".
    ///
    pub fn x(&self) -> bool {
        self.x.is_some()
    }

    /// The line number of the task.
    ///
    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn priority(&self) -> Option<&Priority> {
        self.priority.as_ref().map(Token::value)
    }

    pub fn completed_on(&self) -> Option<&NaiveDate> {
        self.completed_on.as_ref().map(Token::value)
    }

    pub fn started_on(&self) -> Option<&NaiveDate> {
        self.started_on.as_ref().map(Token::value)
    }

    /// A str to the task's description text.
    ///
    pub fn description(&self) -> &str {
        self.description.as_str()
    }

    /// An iterator over the tags in the todo's description.
    ///
    pub fn tags(&self) -> impl Iterator<Item = Token<Tag<'_>>> {
        parser::tags(self.description.as_input(self.line))
    }

    /// True when `x` is some or `completed_on` is some.
    ///
    pub fn is_done(&self) -> bool {
        matches!(
            (&self.x, &self.completed_on),
            (Some(_), None) | (None, Some(_))
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
        struct DebugTags<'a>(u32, &'a Token<Cow<'a, str>>);

        impl Debug for DebugTags<'_> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                let Self(line, description) = *self;
                let tags = parser::tags(description.as_input(line));

                f.debug_list().entries(tags).finish()
            }
        }

        let description = &self.description;

        fmt.debug_struct("Todo")
            .field("x", &self.x)
            .field("line", &self.line)
            .field("priority", &self.priority)
            .field("completed_on", &self.completed_on)
            .field("started_on", &self.started_on)
            .field("description", description)
            .field("tags", &DebugTags(self.line, description))
            .finish()
    }
}

impl Display for Task<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.x() {
            write!(f, "x ")?;
        }

        if let Some(priority) = self.priority() {
            write!(f, "{} ", priority)?;
        }

        if let Some(completed_on) = self.completed_on() {
            write!(f, "{} ", completed_on)?;
        }

        if let Some(started_on) = self.started_on() {
            write!(f, "{} ", started_on)?;
        }

        f.write_str(self.description.as_str())
    }
}

impl FromStr for Task<'static> {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match parser::task1(input.into()) {
            Ok((_, task)) => Ok(task.into_owned()),
            _ => Err("unexpected end of input".to_owned()), // Allocation
        }
    }
}

#[cfg(feature = "serde")]
impl Serialize for Task<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        struct Description<'a>(u32, &'a Token<Cow<'a, str>>);
        struct Tags<'a>(u32, &'a Token<Cow<'a, str>>);

        impl Serialize for Description<'_> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                let Self(line, token) = *self;
                let mut state = serializer.serialize_struct("Description", 2)?;

                state.serialize_field("text", token)?;
                state.serialize_field("tags", &Tags(line, token))?;

                state.end()
            }
        }

        impl Serialize for Tags<'_> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                let Self(line, token) = *self;
                let tags = parser::tags(token.as_input(line));

                serializer.collect_seq(tags)
            }
        }

        let mut state = serializer.serialize_struct("Task", 6)?;

        state.serialize_field("x", &self.x())?;
        state.serialize_field("priority", &self.priority())?;
        state.serialize_field("started_on", &self.started_on())?;
        state.serialize_field("completed_on", &self.completed_on())?;
        state.serialize_field("description", &Description(self.line, &self.description))?;

        state.end()
    }
}
