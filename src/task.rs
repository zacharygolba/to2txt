use chrono::NaiveDate;
use nom::Parser;
use nom::character::complete::space0;
use nom::sequence::preceded;
use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};

#[cfg(feature = "serde")]
use serde::Serialize;

use crate::parser::{self, Token};

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
#[rustfmt::skip]
#[repr(u8)]
pub enum Priority {
    A = 65, B, C, D, E, F, G, H, I, J, K, L, M,
    N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
}

// #[cfg_attr(
//     feature = "serde",
//     derive(Serialize),
//     serde(tag = "type", content = "data", rename_all = "lowercase")
// )]
#[derive(Clone, Debug)]
pub enum Tag<'a> {
    Context(Token<'a, &'a str>),
    Project(Token<'a, &'a str>),
    Named(Token<'a>, Token<'a>),
}

/// A task from a todo list.
///
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Task<'a> {
    pub x: Option<Token<'a>>,
    pub priority: Option<Token<'a, Priority>>,
    pub finished_on: Option<Token<'a, NaiveDate>>,
    pub started_on: Option<Token<'a, NaiveDate>>,
    pub description: Token<'a>,
}

/// Returns true if the task contains only whitespace
///
fn is_empty(task: &Task) -> bool {
    if let Task {
        x: None,
        priority: None,
        finished_on: None,
        started_on: None,
        description,
    } = task
    {
        description.fragment().trim_ascii_end().is_empty()
    } else {
        false
    }
}

impl Display for Priority {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&((*self as u8) as char), f)
    }
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (*self as u32)
            .partial_cmp(&(*other as _))
            .map(Ordering::reverse)
    }
}

impl<'a> Task<'a> {
    pub fn line(&self) -> u32 {
        self.description.line()
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
        self.description.fragment()
    }

    /// An iterator over the tags in the todo's description.
    ///
    pub fn tags(&self) -> impl Iterator<Item = Token<'a, Tag<'a>>> {
        parser::tags(&self.description)
    }

    /// True when `x` is some or `finished_on` is some.
    ///
    pub fn is_done(&self) -> bool {
        matches!(
            (&self.x, &self.finished_on),
            (Some(_), Some(_)) | (Some(_), None) | (None, Some(_))
        )
    }

    /// Parse an individual task from the first line of `input`.
    ///
    /// If `input` is empty or contains only whitespace, `None` is returned.
    ///
    pub fn from_str_opt(input: &'a str) -> Option<Task<'a>> {
        match preceded(space0, parser::task1).parse(input.into()) {
            Ok((_, task)) if is_empty(&task) => None,
            Ok((_, task)) => Some(task),
            Err(_) => None,
        }
    }
}

impl Display for Task<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(x) = &self.x {
            f.write_str(x.fragment())?;
            f.write_str(" ")?;
        }

        if let Some(priority) = &self.priority {
            f.write_str(priority.fragment())?;
            f.write_str(" ")?;
        }

        if let Some(finished_on) = &self.finished_on {
            f.write_str(finished_on.fragment())?;
            f.write_str(" ")?;
        }

        if let Some(started_on) = &self.started_on {
            f.write_str(started_on.fragment())?;
            f.write_str(" ")?;
        }

        f.write_str(self.description())
    }
}
