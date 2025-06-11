use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_while1};
use nom::character::complete::{char, line_ending, one_of, space0, space1};
use nom::combinator::{eof, iterator, map, map_parser, map_res, opt};
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::{IResult, Parser};
use nom_locate::position;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::mem;
use std::str::FromStr;

#[cfg(feature = "serde")]
use serde::Serialize;

use crate::task::{Priority, Tag, Task};

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
pub fn from_str(input: &str) -> impl Iterator<Item = Task<'_>> {
    iterator(input.into(), delimited(eol, task, eol))
}

pub fn task(input: Input) -> IResult<Input, Task> {
    let mut parser = map_parser(
        is_not("\r\n"),
        (
            preceded(space0, position),
            opt(terminated(char('x'), space1)),
            opt(terminated(
                (tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
                space1,
            )),
            opt((position, terminated(ymd, space1))),
            opt((position, terminated(ymd, space1))),
            alt((preceded(space0, is_not("\r\n")), eof)),
        ),
    );

    let (rest, parts) = parser.parse(input)?;
    let (pos, x, priority, completed, started, description) = parts;

    let description_text = description.into_fragment().trim_end();

    Ok((
        rest,
        Task {
            line: pos.location_line(),
            x: x.map(|value| Token::new(Span::locate(&pos, 1), value)),
            priority: priority.map(|(pos, uppercase, _)| {
                Token::new(
                    Span::locate(&pos, 3),
                    // Safety:
                    // The one_of combinator ensures uppercase is 65..=90.
                    unsafe { mem::transmute::<u8, Priority>(uppercase as u8) },
                )
            }),
            completed: completed.and_then(date_from_ymd),
            started: started.and_then(date_from_ymd),
            description: Token::new(
                Span::locate(&description, description_text.len()),
                Cow::Borrowed(description_text),
            ),
        },
    ))
}

pub fn tags<'a>(offset: usize, description: &'a str) -> impl Iterator<Item = Tag<'a>> {
    let typed = |ctor: fn(Token<&'a str>) -> Tag<'a>| {
        move |(pos, output): (Input<'a>, Input<'a>)| {
            ctor(Token::new(
                Span::locate_from(&pos, output.len() + 1, offset),
                output.into_fragment(),
            ))
        }
    };

    let parse1 = map_parser(
        preceded(space0, word),
        opt(alt((
            map((tag("@"), word), typed(Tag::Context)),
            map((tag("+"), word), typed(Tag::Project)),
            map(
                separated_pair(is_not(" :"), tag(":"), word),
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

    iterator(description.into(), parse1).flatten()
}

fn date_from_ymd(output: (Input, (i32, u32, u32))) -> Option<Token<NaiveDate>> {
    let (pos, (y, m, d)) = output;

    Some(Token::new(
        Span::locate(&pos, 10),
        NaiveDate::from_ymd_opt(y, m, d)?,
    ))
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

fn word(input: Input) -> IResult<Input, Input> {
    take_while1(|c: char| !c.is_whitespace()).parse(input)
}

fn ymd(input: Input) -> IResult<Input, (i32, u32, u32)> {
    let mut parser = (
        terminated(parse_to(take(4usize)), tag("-")),
        terminated(parse_to(take(2usize)), tag("-")),
        parse_to(take(2usize)),
    );

    parser.parse(input)
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
        let tags = super::tags(0, input).collect::<Vec<_>>();

        assert_eq!(tags.len(), 3);
    }
}
