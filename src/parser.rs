use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_while1};
use nom::character::complete::{line_ending, one_of, space0, space1};
use nom::combinator::{eof, iterator, map, map_opt, map_parser, map_res, opt, peek};
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::{IResult, Parser};
use nom_locate::position;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Formatter};
use std::mem;
use std::str::FromStr;

#[cfg(feature = "serde")]
use serde::Serialize;

use crate::todotxt::{Priority, Tag, Task};

type Error<'a> = nom::error::Error<Input<'a>>;
type Input<'a> = nom_locate::LocatedSpan<&'a str>;

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, PartialEq, PartialOrd)]
pub struct Span(usize, usize);

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug)]
pub struct Located<T> {
    pub(crate) value: T,
    pub(crate) span: Span,
}

pub trait Moveable {
    fn move_left(&mut self, len: usize);
    fn move_right(&mut self, len: usize);
}

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(input: &str) -> impl Iterator<Item = Task<'_>> {
    iterator(input.into(), delimited(eol, parse_task(), eol)).flatten()
}

pub fn parse_task<'a>() -> impl Parser<Input<'a>, Output = Option<Task<'a>>, Error = Error<'a>> {
    let parts = (
        map(position, |pos: Input<'a>| pos.location_line()),
        opt(map(terminated(tag("x"), space1), |pos| {
            Span::locate(&pos, 1)
        })),
        opt(terminated(parse_priority(), space1)),
        opt((ymd(), opt(ymd()))),
        alt((
            map(preceded(space0, is_not("\r\n")), |output: Input<'a>| {
                let data = output.fragment().trim_end();

                Located {
                    value: Cow::Borrowed(data),
                    span: Span::locate(&output, data.len()),
                }
            }),
            map(eof, |pos| Located {
                value: Cow::Borrowed(""),
                span: Span::locate(&pos, 0),
            }),
        )),
    );

    alt((
        map(
            map_parser(preceded(space0, is_not("\r\n")), parts),
            |(line, x, priority, dates, description)| {
                let (completed, started) = match dates {
                    Some((d1, Some(d2))) => (Some(d1), Some(d2)),
                    Some((d1, None)) => (None, Some(d1)),
                    None => (None, None),
                };

                Some(Task {
                    line,
                    x,
                    priority,
                    completed,
                    started,
                    description,
                })
            },
        ),
        map(preceded(space0, peek(one_of("\r\n"))), |_| None),
    ))
}

pub fn parse_tags<'a>(offset: usize, input: &'a str) -> impl Iterator<Item = Tag<'a>> {
    let typed = |ctor: fn(Located<&'a str>) -> Tag<'a>| {
        move |(pos, output): (Input<'a>, Input<'a>)| {
            ctor(Located {
                span: Span::locate(&pos, output.len() + 1).move_right(offset),
                value: output.into_fragment(),
            })
        }
    };

    let parse1 = map_parser(
        preceded(space0, word()),
        opt(alt((
            map((tag("@"), word()), typed(Tag::Context)),
            map((tag("+"), word()), typed(Tag::Project)),
            map(
                separated_pair(is_not(" :"), tag(":"), word()),
                move |(name, value)| {
                    let len = name.len() + value.len() + 1;
                    Tag::Named(Located {
                        span: Span::locate(&name, len).move_right(offset),
                        value: (name.into_fragment(), value.into_fragment()),
                    })
                },
            ),
        ))),
    );

    iterator(input.into(), parse1).flatten()
}

fn parse_priority<'a>() -> impl Parser<Input<'a>, Output = Located<Priority>, Error = Error<'a>> {
    map(
        (tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
        |(pos, uppercase, _)| {
            Located::new(Span::locate(&pos, 3), unsafe {
                mem::transmute(uppercase as u8)
            })
        },
    )
}

fn eol(input: Input) -> IResult<Input, ()> {
    let mut rest = input;

    while let Ok((next, _)) = line_ending::<_, Error>(rest) {
        rest = next;
    }

    Ok((rest, ()))
}

fn parse_to<'a, R, P>(parser: P) -> impl Parser<Input<'a>, Output = R, Error = Error<'a>>
where
    R: FromStr,
    P: Parser<Input<'a>, Output = Input<'a>, Error = Error<'a>>,
{
    map_res(parser, |output| output.fragment().parse())
}

fn word<'a>() -> impl Parser<Input<'a>, Output = Input<'a>, Error = Error<'a>> {
    take_while1(|c: char| !c.is_whitespace())
}

fn ymd<'a>() -> impl Parser<Input<'a>, Output = Located<NaiveDate>, Error = Error<'a>> {
    let triple = (
        terminated(parse_to(take(4usize)), tag("-")),
        terminated(parse_to(take(2usize)), tag("-")),
        parse_to(take(2usize)),
    );

    map_opt(
        (position, terminated(triple, space1)),
        |(pos, (y, m, d))| {
            Some(Located {
                value: NaiveDate::from_ymd_opt(y, m, d)?,
                span: Span::locate(&pos, 10),
            })
        },
    )
}

impl<T> Located<T> {
    /// The location of the associated item.
    ///
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// A reference to value of the associated item.
    ///
    pub fn value(&self) -> &T {
        &self.value
    }
}

impl<T> Located<T> {
    pub(crate) const fn new(span: Span, value: T) -> Self {
        Self { span, value }
    }

    pub(crate) fn replace(&mut self, value: T) -> T {
        mem::replace(&mut self.value, value)
    }

    pub(crate) fn map<U, F>(self, f: F) -> Located<U>
    where
        F: FnOnce(T) -> U,
    {
        Located {
            span: self.span,
            value: f(self.value),
        }
    }
}

impl<T: PartialEq> PartialEq for Located<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value().eq(other.value())
    }
}

impl<T: PartialOrd> PartialOrd for Located<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value().partial_cmp(other.value())
    }
}

impl Span {
    pub fn start(&self) -> usize {
        self.0
    }

    pub fn end(&self) -> usize {
        self.1
    }
}

impl Span {
    pub(crate) fn new_unchecked(start: usize, end: usize) -> Self {
        Self(start, end)
    }

    pub(crate) fn move_right(self, len: usize) -> Self {
        Self(self.0 + len, self.1 + len)
    }

    fn locate(input: &Input, len: usize) -> Self {
        let start = input.get_utf8_column() - 1;
        Self(start, start + len)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Span({}, {})", self.start(), self.end())
    }
}

impl<T> Moveable for Located<T> {
    fn move_left(&mut self, len: usize) {
        self.span.0 -= len;
        self.span.1 -= len;
    }

    fn move_right(&mut self, len: usize) {
        self.span.0 += len;
        self.span.1 += len;
    }
}

impl<T: Moveable> Moveable for Option<T> {
    fn move_left(&mut self, len: usize) {
        if let Some(location) = self.as_mut() {
            location.move_left(len);
        }
    }

    fn move_right(&mut self, len: usize) {
        if let Some(location) = self.as_mut() {
            location.move_right(len);
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_parse_tags() {
        let input = "feed the tomato plants @home +garden due:2025-06-10";
        let tags = super::parse_tags(0, input).collect::<Vec<_>>();

        assert_eq!(tags.len(), 3);
    }
}
