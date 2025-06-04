use chrono::NaiveDate;
use nom::bytes::complete::{is_not, tag, take};
use nom::character::complete::{line_ending, one_of, space0};
use nom::combinator::{iterator, map, map_opt, map_res, opt};
use nom::sequence::{delimited, preceded, terminated};
use nom::{IResult, Parser};
use nom_locate::position;
use std::borrow::Cow;
use std::cmp::Ordering;

#[cfg(feature = "serde")]
use serde::ser::SerializeStruct;
#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::tag::{tags, Tag};

type Dates = (YearMonthDay, Option<YearMonthDay>);
type Parts<'a> = (Option<Span>, Option<Priority>, Option<Dates>, Input<'a>);
type Error<'a> = nom::error::Error<Input<'a>>;
type Input<'a> = nom_locate::LocatedSpan<&'a str>;

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug)]
pub struct Description<'a> {
    text: Cow<'a, str>,
    span: Span,
}

/// TODO: docs
///
#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Priority {
    rank: char,
    span: Span,
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd)]
pub struct Span {
    line: u32,
    col: (usize, usize),
}

/// A task from a todo list.
///
#[derive(Clone, Debug)]
pub struct Todo<'a> {
    checkmark: Option<Span>,
    priority: Option<Priority>,
    completed: Option<YearMonthDay>,
    started: Option<YearMonthDay>,
    description: Description<'a>,
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct YearMonthDay {
    date: NaiveDate,
    span: Span,
}

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(value: &str) -> impl Iterator<Item = Todo> {
    iterator(value.into(), delimited(eol, todo(), eol))
}

fn checkmark<'a>() -> impl Parser<Input<'a>, Output = Option<Span>, Error = Error<'a>> {
    opt(map(terminated(tag("x"), tag(" ")), |at| {
        Span::locate(&at, 1)
    }))
}

fn eol(input: Input) -> IResult<Input, (), Error> {
    let mut rest = input;

    loop {
        match line_ending::<_, Error>(rest) {
            Ok((next, _)) => rest = next,
            Err(_) => break,
        }
    }

    Ok((rest, ()))
}

fn priority<'a>() -> impl Parser<Input<'a>, Output = Priority, Error = Error<'a>> {
    let parser = (
        map(position, |at| Span::locate(&at, 3)),
        terminated(
            delimited(tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
            tag(" "),
        ),
    );

    map(parser, |(span, rank)| Priority { rank, span })
}

fn todo<'a>() -> impl Parser<Input<'a>, Output = Todo<'a>, Error = Error<'a>> {
    let parser = (
        checkmark(),
        opt(priority()),
        opt((ymd(), opt(ymd()))),
        is_not("\r\n"),
    );

    map(preceded(space0, parser), Todo::from_parts)
}

fn ymd<'a>() -> impl Parser<Input<'a>, Output = YearMonthDay, Error = Error<'a>> {
    let triple = (
        terminated(
            map_res(take(4usize), |span: Input| span.fragment().parse::<i32>()),
            tag("-"),
        ),
        terminated(
            map_res(take(2usize), |span: Input| span.fragment().parse::<u32>()),
            tag("-"),
        ),
        map_res(take(2usize), |span: Input| span.fragment().parse::<u32>()),
    );

    map_opt(
        (position, terminated(triple, tag(" "))),
        |(at, (y, m, d))| {
            Some(YearMonthDay {
                date: NaiveDate::from_ymd_opt(y, m, d)?,
                span: Span::locate(&at, 10),
            })
        },
    )
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.rank.partial_cmp(&other.rank).map(Ordering::reverse)
    }
}

impl Span {
    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn start(&self) -> usize {
        self.col.0
    }

    pub fn end(&self) -> usize {
        self.col.1
    }
}

impl Span {
    pub(crate) fn new(line: u32, column: (usize, usize)) -> Self {
        Self { line, col: column }
    }

    fn locate(at: &Input, len: usize) -> Self {
        let start = at.get_column() - 1;

        Self {
            line: at.location_line(),
            col: (start, start + len),
        }
    }
}

impl Todo<'_> {
    pub fn is_done(&self) -> bool {
        self.checkmark.is_some() || self.completed.is_some()
    }

    pub fn priority(&self) -> Option<&Priority> {
        self.priority.as_ref()
    }

    pub fn completed(&self) -> Option<&NaiveDate> {
        self.completed.as_ref().map(|ymd| &ymd.date)
    }

    pub fn started(&self) -> Option<&NaiveDate> {
        self.started.as_ref().map(|ymd| &ymd.date)
    }

    pub fn description(&self) -> &str {
        self.description.text.as_ref()
    }

    pub fn tags(&self) -> impl Iterator<Item = Tag> {
        tags(self.description.span, self.description())
    }
}

impl<'a> Todo<'a> {
    pub fn parse(input: &'a str) -> Option<Self> {
        todo().parse(input.into()).map(|(_, output)| output).ok()
    }

    pub fn into_owned(self) -> Todo<'static> {
        Todo {
            description: Description {
                text: Cow::Owned(self.description.text.into_owned()),
                ..self.description
            },
            ..self
        }
    }
}

impl<'a> Todo<'a> {
    fn from_parts(parts: Parts<'a>) -> Self {
        let (checkmark, priority, dates, description) = parts;
        let (date_completed, date_started) = match dates {
            Some((a, Some(b))) => (Some(a), Some(b)),
            Some((a, None)) => (None, Some(a)),
            None => (None, None),
        };

        Self {
            checkmark,
            priority,
            completed: date_completed,
            started: date_started,
            description: Description {
                text: Cow::Borrowed(description.fragment()),
                span: Span::locate(&description, description.len()),
            },
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
        state.serialize_field("date_completed", &self.completed)?;
        state.serialize_field("date_started", &self.started)?;
        state.serialize_field("description", &self.description)?;
        state.serialize_field("tags", &tags)?;

        state.end()
    }
}

// #[cfg(test)]
// mod tests {
//     use super::{Priority, Span};

//     #[test]
//     fn test_priority_order() {
//         assert!(
//             Priority { rank: 'A', span } > Priority { rank: 'B', span },
//             "Priority should be ordered alphabetically in descending order."
//         )
//     }
// }
