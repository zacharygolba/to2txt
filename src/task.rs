use chrono::NaiveDate;
use nom::Parser;
use nom::character::complete::space0;
use nom::sequence::preceded;
use nom_locate::LocatedSpan;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};

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
pub struct Task<'a> {
    pub x: Option<Token<bool>>,
    pub priority: Option<Token<Priority>>,
    pub finished_on: Option<Token<NaiveDate>>,
    pub started_on: Option<Token<NaiveDate>>,
    pub description: Token<Cow<'a, str>>,

    pub(crate) line: u32,
}

impl Display for Priority {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({})", &((*self as u8) as char))
    }
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (*self as u32)
            .partial_cmp(&(*other as _))
            .map(Ordering::reverse)
    }
}

impl Task<'_> {
    /// The line number of the task.
    ///
    pub fn line(&self) -> u32 {
        self.line
    }

    /// True when the task starts with a "x".
    ///
    pub fn x(&self) -> bool {
        self.x.is_some()
    }

    pub fn priority(&self) -> Option<&Priority> {
        Some(self.priority.as_ref()?.value())
    }

    pub fn finished_on(&self) -> Option<&NaiveDate> {
        Some(self.finished_on.as_ref()?.value())
    }

    pub fn started_on(&self) -> Option<&NaiveDate> {
        Some(self.started_on.as_ref()?.value())
    }

    /// A str to the task's description text.
    ///
    pub fn description(&self) -> &str {
        self.description.as_str()
    }

    /// An iterator over the tags in the todo's description.
    ///
    pub fn tags(&self) -> impl Iterator<Item = Token<Tag<'_>>> {
        let description = &self.description;
        let fragment = description.as_str();
        let offset = description.span().start();

        // Safety:
        //
        // The tags of a task are parsed lazily to avoid a dynamic, per-task
        // allocation. In order to facilitate this, we have to create a new
        // LocatedSpan identical to the one used to create the task's
        // description token.
        //
        // We know that the line number of the provided task and the span of
        // it's description are valid. Therefore, we know that a LocatedSpan
        // created from these values will be structurally identical to the
        // source of the description token.
        //
        let input = unsafe { LocatedSpan::new_from_raw_offset(offset, self.line, fragment, ()) };

        parser::tags(input)
    }

    /// True when `x` is some or `finished_on` is some.
    ///
    pub fn is_done(&self) -> bool {
        matches!(
            (&self.x, &self.finished_on),
            (Some(_), Some(_)) | (Some(_), None) | (None, Some(_))
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

impl<'a> Task<'a> {
    /// Parse an individual task from the first line of `input`.
    ///
    /// If `input` is empty or contains only whitespace, `None` is returned.
    ///
    pub fn from_str_opt(input: &'a str) -> Option<Task<'a>> {
        match preceded(space0, parser::task1).parse(input.into()) {
            Ok((_, task)) if task.is_empty() => None,
            Ok((_, task)) => Some(task),
            Err(_) => None,
        }
    }
}

impl Task<'_> {
    /// Returns true if the task contains only whitespace
    ///
    pub(crate) fn is_empty(&self) -> bool {
        match self {
            Self {
                x: None,
                priority: None,
                finished_on: None,
                started_on: None,
                description,
                ..
            } => description.as_str().trim_ascii_end().is_empty(),
            _ => false,
        }
    }
}

impl Debug for Task<'_> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        struct DebugTags<'a>(&'a Task<'a>);

        impl Debug for DebugTags<'_> {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                f.debug_list().entries(self.0.tags()).finish()
            }
        }

        fmt.debug_struct("Todo")
            .field("line", &self.line)
            .field("x", &self.x)
            .field("priority", &self.priority)
            .field("finished_on", &self.finished_on)
            .field("started_on", &self.started_on)
            .field("description", &self.description)
            .field("tags", &DebugTags(self))
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

        if let Some(finished_on) = self.finished_on() {
            write!(f, "{} ", finished_on)?;
        }

        if let Some(started_on) = self.started_on() {
            write!(f, "{} ", started_on)?;
        }

        f.write_str(self.description())
    }
}

#[cfg(feature = "serde")]
impl Serialize for Task<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeStruct;

        struct Description<'a>(&'a Task<'a>);
        struct Tags<'a>(&'a Task<'a>);

        impl Serialize for Description<'_> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                let Self(task) = *self;
                let mut state = serializer.serialize_struct("Description", 2)?;

                state.serialize_field("text", task.description())?;
                state.serialize_field("tags", &Tags(task))?;

                state.end()
            }
        }

        impl Serialize for Tags<'_> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                serializer.collect_seq(self.0.tags())
            }
        }

        let mut state = serializer.serialize_struct("Task", 5)?;

        state.serialize_field("is_done", &self.is_done())?;
        state.serialize_field("priority", &self.priority())?;
        state.serialize_field("finished_on", &self.finished_on())?;
        state.serialize_field("started_on", &self.started_on())?;
        state.serialize_field("description", &Description(self))?;

        state.end()
    }
}

#[cfg(test)]
mod tests {
    use super::Task;

    #[test]
    fn test_from_str_opt() {
        assert!(Task::from_str_opt("").is_none(), "empty as input is None");

        assert!(
            Task::from_str_opt(" ").is_none(),
            "whitespace as input is None"
        );

        assert!(
            Task::from_str_opt("(A) ").is_some(),
            "a task with headers is Some"
        );
        assert!(
            Task::from_str_opt("feed tomato plants +garden @home").is_some(),
            "a task with a description is Some",
        );
    }

    #[test]
    fn test_parse_tags() {
        let input = "feed the tomato plants @home +garden due:2025-06-10";
        let task = Task::from_str_opt(input).unwrap();
        let vec = task.tags().collect::<Vec<_>>();

        assert_eq!(vec.len(), 3);
    }
}
