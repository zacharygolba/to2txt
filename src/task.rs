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

use crate::locate::{Located, Span, Translate};
use crate::parser::{self};

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
    serde(tag = "type", rename_all = "UPPERCASE")
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
pub struct Task<'a> {
    pub(crate) line: u32,
    pub(crate) x: Option<Located<char>>,
    pub(crate) priority: Option<Located<Priority>>,
    pub(crate) completed: Option<Located<NaiveDate>>,
    pub(crate) started: Option<Located<NaiveDate>>,
    pub(crate) description: Located<Cow<'a, str>>,
}

fn set_opt_task_field<A, S, T>(
    antecedent: A,
    mut successive: S,
    field: &mut Option<Located<T>>,
    value: Option<T>,
    len: usize,
) where
    A: Translate,
    S: Translate,
{
    match (field, value) {
        (Some(existing), Some(next)) => {
            existing.replace(next);
        }
        (option_mut @ None, Some(next)) => {
            let gap = antecedent.abs_distance(&successive).unwrap_or(1);
            let start = antecedent.origin().map_or(0, |span| span.end() + gap);
            *option_mut = Some(Located::new(Span::new(start, start + len), next));

            successive.translate_r(len + gap);
        }
        (option_mut @ Some(_), None) => {
            let gap = antecedent.abs_distance(option_mut).unwrap_or(1);
            *option_mut = None;

            successive.translate_l(gap + len);
        }
        (None, None) => {
            // noop
        }
    }
}

impl Display for Priority {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({})", (*self as u8) as char)
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
        Some(self.x.as_ref()?.span())
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

    pub fn mark_complete(&mut self) {
        let x = &mut self.x;
        let mut successive = (
            self.priority.as_mut(),
            self.completed.as_mut(),
            self.started.as_mut(),
            &mut self.description,
        );

        if x.is_some() {
            *x = None;
            successive.translate_r(2);
        }
    }

    pub fn mark_incomplete(&mut self) {
        let x = &mut self.x;
        let mut successive = (
            self.priority.as_mut(),
            self.completed.as_mut(),
            self.started.as_mut(),
            &mut self.description,
        );

        if x.is_some() {
            *x = None;
            successive.translate_l(2);
        }
    }

    pub fn priority(&self) -> Option<&Located<Priority>> {
        self.priority.as_ref()
    }

    pub fn set_priority(&mut self, value: Option<Priority>) {
        set_opt_task_field(
            &mut self.x,
            (
                &mut self.completed,
                &mut self.started,
                &mut self.description,
            ),
            &mut self.priority,
            value,
            3,
        );
    }

    pub fn completed(&self) -> Option<&Located<NaiveDate>> {
        self.completed.as_ref()
    }

    pub fn set_completed(&mut self, value: Option<NaiveDate>) {
        set_opt_task_field(
            (&mut self.x, &mut self.priority),
            (&mut self.started, &mut self.description),
            &mut self.completed,
            value,
            10,
        );
    }

    pub fn started(&self) -> Option<&Located<NaiveDate>> {
        self.started.as_ref()
    }

    pub fn set_started(&mut self, value: Option<NaiveDate>) {
        set_opt_task_field(
            (&mut self.x, &mut self.priority, &mut self.completed),
            &mut self.description,
            &mut self.started,
            value,
            10,
        );
    }

    pub fn description(&self) -> Located<&str> {
        let description = &self.description;

        Located {
            value: description.value.as_ref(),
            span: description.span.clone(),
        }
    }

    pub fn set_description(&mut self, value: String) {
        let offset = self.description().span().start();
        let span = Span::new(offset, offset + value.len());

        self.description = Located::new(span, Cow::Owned(value));
    }

    /// Returns an iterator over the tags in the todo's description.
    ///
    pub fn tags(&self) -> impl Iterator<Item = Tag<'_>> {
        let description = &self.description;
        let offset = description.span().start();

        parser::parse_tags(offset, description.value())
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
            write!(f, "{} ", priority.value())?;
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
    use crate::Task;

    use super::Priority;

    type Error = Box<dyn std::error::Error>;

    #[test]
    fn test_set_priority() -> Result<(), Error> {
        let mut with_x: Task = "x feed the tomato plants".parse()?;
        let mut with_dates: Task = "2025-06-10 2025-06-10 feed the tomato plants".parse()?;
        let mut with_existing: Task = "(A) feed the tomato plants".parse()?;

        with_x.set_priority(Some(Priority::A));
        println!("{}", with_x);

        with_x.mark_incomplete();
        println!("{}", with_x);

        with_dates.set_priority(Some(Priority::A));
        println!("{}", with_dates);

        with_existing.set_priority(Some(Priority::B));
        println!("{}", with_existing);

        with_existing.set_priority(None);
        println!("{}", with_existing);

        Ok(())
    }

    #[test]
    fn test_priority_order() {
        assert!(
            Priority::A > Priority::B,
            "Priority should be ordered alphabetically in descending order."
        )
    }
}
