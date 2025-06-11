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
use std::str::FromStr;

use crate::task::{Priority, Tag, Task};

#[cfg(feature = "serde")]
use serde::Serialize;

type Error<'a> = nom::error::Error<Input<'a>>;
type Input<'a> = nom_locate::LocatedSpan<&'a str>;

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Span(usize, usize);

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug)]
pub struct Token<T> {
    pub(crate) value: T,
    pub(crate) span: Span,
}

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(value: &str) -> impl Iterator<Item = Task<'_>> {
    iterator(Input::new(value), delimited(eol, parse_task(), eol)).flatten()
}

pub fn parse_task<'a>() -> impl Parser<Input<'a>, Output = Option<Task<'a>>, Error = Error<'a>> {
    let parts = (
        map(position::<Input, _>, |pos| pos.location_line()),
        opt(map(terminated(tag("x"), space1), |x| Span::locate(&x, 1))),
        opt(terminated(parse_priority(), space1)),
        opt((ymd(), opt(ymd()))),
        alt((
            map(preceded(space0, is_not("\r\n")), |o: Input<'a>| {
                let value = Cow::Borrowed(o.into_fragment().trim_end());
                Token::new(Span::locate(&o, value.len()), value)
            }),
            map(eof, |o| Token::new(Span::locate(&o, 0), Cow::Borrowed(""))),
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

pub fn parse_tags<'a>(offset: usize, description: &'a str) -> impl Iterator<Item = Tag<'a>> {
    let typed = |ctor: fn(Token<&'a str>) -> Tag<'a>| {
        move |(pos, output): (Input<'a>, Input<'a>)| {
            ctor(Token::new(
                Span::locate_from(&pos, output.len() + 1, offset),
                output.into_fragment(),
            ))
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

                    Tag::Named(Token::new(
                        Span::locate_from(&name, len, offset),
                        (name.into_fragment(), value.into_fragment()),
                    ))
                },
            ),
        ))),
    );

    iterator(Input::new(description), parse1).flatten()
}

fn parse_priority<'a>() -> impl Parser<Input<'a>, Output = Token<Priority>, Error = Error<'a>> {
    map(
        (tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
        |(pos, uppercase, _)| {
            let span = Span::locate(&pos, 3);

            Token::new(span, unsafe {
                // Safety:
                //
                // The one_of combinator always produces an ASCII uppercase char.
                // The Priority enum variants intentionally map to the ASCII
                // uppercase range.
                //
                std::mem::transmute::<u8, Priority>(uppercase as u8)
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

fn ymd<'a>() -> impl Parser<Input<'a>, Output = Token<NaiveDate>, Error = Error<'a>> {
    let triple = (
        terminated(parse_to(take(4usize)), tag("-")),
        terminated(parse_to(take(2usize)), tag("-")),
        parse_to(take(2usize)),
    );

    map_opt(
        (position, terminated(triple, space1)),
        |(pos, (y, m, d))| {
            Some(Token {
                value: NaiveDate::from_ymd_opt(y, m, d)?,
                span: Span::locate(&pos, 10),
            })
        },
    )
}

impl<T> Token<T> {
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

impl<T> Token<T> {
    pub(crate) fn new(span: Span, value: T) -> Self {
        Self { span, value }
    }

    pub(crate) fn map<U>(self, f: impl FnOnce(T) -> U) -> Token<U> {
        Token {
            value: f(self.value),
            span: self.span,
        }
    }
}

impl<T: PartialEq> PartialEq for Token<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value().eq(other.value())
    }
}

impl<T: PartialOrd> PartialOrd for Token<T> {
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
    pub(crate) fn locate(input: &Input, len: usize) -> Self {
        let start = input.get_utf8_column() - 1;
        Self(start, start + len)
    }

    pub(crate) fn locate_from(input: &Input, len: usize, offset: usize) -> Self {
        let mut span = Self::locate(input, len);

        span.0 += offset;
        span.1 += offset;

        span
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
