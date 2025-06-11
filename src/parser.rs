use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_while1};
use nom::character::complete::{char, line_ending, one_of, space0, space1};
use nom::combinator::{iterator, map, map_parser, map_res, opt, rest};
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::{IResult, Parser};
use nom_locate::position;
use std::cmp::Ordering;
use std::str::FromStr;

#[cfg(feature = "serde")]
use serde::Serialize;

use crate::task::{Tag, Task};

type Error<'a> = nom::error::Error<Input<'a>>;

pub(crate) type Input<'a> = nom_locate::LocatedSpan<&'a str>;
pub(crate) type Parts<'a> = (
    Input<'a>,
    Option<char>,
    Option<(Input<'a>, char, Input<'a>)>,
    Option<(Input<'a>, (i32, u32, u32))>,
    Option<(Input<'a>, (i32, u32, u32))>,
    Input<'a>,
);

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
    iterator(input.into(), delimited(eol, task, eol)).flatten()
}

pub fn task(input: Input) -> IResult<Input, Option<Task>> {
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
            map(preceded(space0, rest), trim_end),
        ),
    );

    parser.parse(input).map(|(rest, parts)| match &parts {
        (_, None, None, None, None, d) if d.is_empty() => (rest, None),
        _ => (rest, Some(Task::from_parts(parts))),
    })
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

fn trim_end(input: Input) -> Input {
    // Safety:
    // We do not changing the original offset. The returned input can always
    // produce valid index range.
    unsafe {
        Input::new_from_raw_offset(
            input.location_offset(),
            input.location_line(),
            input.into_fragment().trim_end(),
            (),
        )
    }
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

impl<T> Token<T> {
    /// A reference to value of the associated item.
    ///
    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn start(&self) -> usize {
        self.span.0
    }

    pub fn end(&self) -> usize {
        self.span.1
    }
}

impl<T> Token<T> {
    pub(crate) fn new(span: Span, value: T) -> Self {
        Self { span, value }
    }

    pub(crate) fn map<U, F>(self, f: F) -> Token<U>
    where
        F: FnOnce(T) -> U,
    {
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

#[cfg(test)]
mod tests {
    #[test]
    fn test_parse_tags() {
        let input = "feed the tomato plants @home +garden due:2025-06-10";
        let tags = super::tags(0, input).collect::<Vec<_>>();

        assert_eq!(tags.len(), 3);
    }
}
