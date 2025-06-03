use chrono::NaiveDate;
use nom::bytes::complete::{is_not, tag, take};
use nom::character::complete::{one_of, space1};
use nom::combinator::{into, map, map_opt, map_res, opt};
use nom::sequence::{delimited, terminated};
use nom::Parser;
use std::borrow::Cow;

#[cfg(feature = "serde")]
use serde::Serialize;

type Dates = (NaiveDate, Option<NaiveDate>);
type Parts<'a> = (bool, Option<Priority>, Option<Dates>, &'a str);
type Error<'a> = nom::error::Error<&'a str>;

/// TODO: docs
///
#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Priority(char);

/// A task from a todo list.
///
#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug)]
pub struct Todo<'a> {
    is_done: bool,
    priority: Option<Priority>,
    date_completed: Option<NaiveDate>,
    date_started: Option<NaiveDate>,
    description: Cow<'a, str>,
}

/// TODO: docs
///
pub fn from_str(value: &str) -> impl Iterator<Item = Todo> {
    value.lines().filter_map(|line| {
        let input = line.trim();

        if input.is_empty() {
            None
        } else {
            Todo::parse(input)
        }
    })
}

fn checkmark<'a>() -> impl Parser<&'a str, Output = bool, Error = Error<'a>> {
    map(opt(terminated(tag("x"), tag(" "))), |x| x.is_some())
}

fn priority<'a>() -> impl Parser<&'a str, Output = Priority, Error = Error<'a>> {
    into(terminated(
        delimited(tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
        space1,
    ))
}

fn ymd<'a>() -> impl Parser<&'a str, Output = NaiveDate, Error = Error<'a>> {
    let triple = (
        terminated(map_res(take(4usize), str::parse::<i32>), tag("-")),
        terminated(map_res(take(2usize), str::parse::<u32>), tag("-")),
        map_res(take(2usize), str::parse::<u32>),
    );

    map_opt(terminated(triple, tag(" ")), |output| {
        let (year, month, day) = output;
        NaiveDate::from_ymd_opt(year, month, day)
    })
}

impl From<char> for Priority {
    fn from(value: char) -> Self {
        Self(value)
    }
}

impl Todo<'_> {
    pub fn is_done(&self) -> bool {
        self.is_done
    }

    pub fn priority(&self) -> Option<&Priority> {
        self.priority.as_ref()
    }

    pub fn date_completed(&self) -> Option<&NaiveDate> {
        self.date_completed.as_ref()
    }

    pub fn date_started(&self) -> Option<&NaiveDate> {
        self.date_started.as_ref()
    }

    pub fn description(&self) -> &str {
        &self.description
    }
}

impl<'a> Todo<'a> {
    pub fn parse(input: &'a str) -> Option<Self> {
        let parser = (
            checkmark(),
            opt(priority()),
            opt((ymd(), opt(ymd()))),
            is_not("\r\n"),
        );

        match map(parser, Self::from_parts).parse(input) {
            Ok((_, todo)) => Some(todo),
            Err(_) => None,
        }
    }

    pub fn into_owned(self) -> Todo<'static> {
        Todo {
            description: Cow::Owned(self.description.into_owned()),
            ..self
        }
    }
}

impl<'a> Todo<'a> {
    fn from_parts(parts: Parts<'a>) -> Self {
        let (mut is_done, priority, dates, description) = parts;
        let (date_completed, date_started) = match dates {
            Some((a, Some(b))) => (Some(a), Some(b)),
            Some((a, None)) => (None, Some(a)),
            None => (None, None),
        };

        if !is_done && date_completed.is_some() {
            is_done = true;
        }

        Self {
            is_done,
            priority,
            date_completed,
            date_started,
            description: Cow::Borrowed(description.trim_end()),
        }
    }
}
