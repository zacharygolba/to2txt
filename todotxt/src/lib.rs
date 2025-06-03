use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take};
use nom::character::complete::{line_ending, one_of, space0, space1};
use nom::combinator::{into, map, map_opt, map_res, opt};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, terminated};
use nom::{IResult, Parser};
use std::borrow::Cow;

#[cfg(feature = "serde")]
use serde::Serialize;

#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Priority(char);

/// A task from a todo list.
///
#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug)]
pub struct Task<'a> {
    pub is_done: bool,
    pub priority: Option<Priority>,
    pub date_completed: Option<NaiveDate>,
    pub date_started: Option<NaiveDate>,
    pub description: Cow<'a, str>,
}

/// TODO: docs
///
pub fn parse(input: &str) -> Vec<Task> {
    separated_list0(many0(line_ending), delimited(space0, task, space0))
        .parse(input)
        .map(|(_, tasks)| tasks)
        .unwrap_or_default()
}

fn task(input: &str) -> IResult<&str, Task> {
    let mut parser = (
        map(opt(terminated(tag("x"), space1)), |option| option.is_some()),
        opt(into(terminated(
            delimited(tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
            space1,
        ))),
        opt(terminated(
            alt((
                map((ymd, space1, ymd), |(date_completed, _, date_started)| {
                    (Some(date_completed), Some(date_started))
                }),
                map(opt(ymd), |date_started| (None, date_started)),
            )),
            space0,
        )),
        is_not("\n"),
    );

    let (rest, (is_done, priority, dates, description)) = parser.parse(input)?;
    let (date_completed, date_started) = dates.unwrap_or_default();

    Ok((
        rest,
        Task {
            is_done: is_done || date_completed.is_some(),
            priority,
            date_completed,
            date_started,
            description: Cow::Borrowed(description),
        },
    ))
}

fn ymd(input: &str) -> IResult<&str, NaiveDate> {
    map_opt(
        (
            terminated(map_res(take(4usize), str::parse::<i32>), tag("-")),
            terminated(map_res(take(2usize), str::parse::<u32>), tag("-")),
            map_res(take(2usize), str::parse::<u32>),
        ),
        |(y, m, d)| NaiveDate::from_ymd_opt(y, m, d),
    )
    .parse(input)
}

impl From<char> for Priority {
    fn from(value: char) -> Self {
        Self(value)
    }
}
