use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_while1};
use nom::character::complete::{char, line_ending, space0, space1};
use nom::character::streaming::one_of;
use nom::combinator::{iterator, map, map_parser, map_res, opt, rest, value};
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::{IResult, Parser};
use nom_locate::position;
use std::borrow::Cow;
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
    value: T,
    span: Span,
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
            opt(value(true, terminated(char('x'), space1))),
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
        (_, None, None, None, None, description) if description.is_empty() => (rest, None),
        (from, x, priority, completed_on, started_on, description) => (
            rest,
            Some(Task {
                line: from.location_line(),
                x: x.map(|value| Token::new(1, &from, value)),
                priority: priority.map(|(f, ascii, _)| {
                    Token::new(
                        3,
                        &f,
                        // Safety:
                        // The one_of combinator ensures uppercase is 65..=90.
                        unsafe { mem::transmute::<u32, Priority>(ascii as _) },
                    )
                }),
                completed_on: completed_on.and_then(date_from_ymd),
                started_on: started_on.and_then(date_from_ymd),
                description: Token::new(
                    description.len(),
                    &description,
                    Cow::Borrowed(description.into_fragment()),
                ),
            }),
        ),
    })
}

pub fn tags<'a>(offset: usize, description: &'a str) -> impl Iterator<Item = Tag<'a>> {
    let typed = |ctor: fn(Token<&'a str>) -> Tag<'a>| {
        move |(from, output): (Input<'a>, Input<'a>)| {
            let len = output.len() + 1;
            let value = output.into_fragment();
            ctor(Token::new_with_offset(len, offset, &from, value))
        }
    };

    let parse1 = map_parser(
        preceded(space0, word),
        opt(alt((
            map((tag("@"), word), typed(Tag::Context)),
            map((tag("+"), word), typed(Tag::Project)),
            map(
                separated_pair(is_not(" :"), tag(":"), word),
                move |(k, v)| {
                    Tag::Named(
                        Token::new_with_offset(k.len(), offset, &k, k.into_fragment()),
                        Token::new_with_offset(v.len(), offset, &v, v.into_fragment()),
                    )
                },
            ),
        ))),
    );

    iterator(description.into(), parse1).flatten()
}

fn date_from_ymd(output: (Input, (i32, u32, u32))) -> Option<Token<NaiveDate>> {
    let (from, (y, m, d)) = output;
    let value = NaiveDate::from_ymd_opt(y, m, d)?;

    Some(Token::new(10, &from, value))
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
    pub fn start(&self) -> usize {
        self.0
    }

    pub fn end(&self) -> usize {
        self.1
    }
}

impl<T> Token<T> {
    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl<T: Copy> Token<T> {
    pub fn value(&self) -> &T {
        &self.value
    }
}

impl Token<Cow<'_, str>> {
    pub fn as_str(&self) -> &str {
        self.value.as_ref()
    }

    pub fn into_string(self) -> String {
        self.value.into_owned()
    }
}

impl<T> Token<T> {
    pub(crate) fn map<U, F>(self, f: F) -> Token<U>
    where
        F: FnOnce(T) -> U,
    {
        Token {
            value: f(self.value),
            span: self.span,
        }
    }

    fn new_with_offset(len: usize, offset: usize, from: &Input, value: T) -> Self {
        let mut token = Self::new(len, from, value);
        let Span(start, end) = &mut token.span;

        *start += offset;
        *end += offset;

        token
    }

    fn new(len: usize, from: &Input, value: T) -> Self {
        let start = from.get_utf8_column() - 1;
        let span = Span(start, start + len);

        Self { value, span }
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
