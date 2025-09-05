use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_till1};
use nom::character::complete::{multispace0, one_of, space0, space1};
use nom::combinator::{iterator, map, map_opt, map_parser, map_res, opt, recognize, rest};
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::{IResult, Input, Offset, Parser};
use nom_locate::LocatedSpan;
use std::mem;
use std::str::FromStr;

use crate::task::{Priority, Tag, Task};

type Error<'a> = nom::error::Error<Span<'a>>;
type Span<'a, T = ()> = LocatedSpan<&'a str, T>;

#[derive(Clone, Debug)]
pub struct Token<'a, T = ()> {
    span: Span<'a, T>,
}

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(input: &str) -> impl Iterator<Item = Task<'_>> {
    iterator(input.into(), delimited(multispace0, task1, multispace0))
}

pub fn tags<'a>(input: &Token<'a>) -> impl Iterator<Item = Token<'a, Tag<'a>>> {
    let tag1 = map_parser(
        preceded(space0, word),
        opt(alt((
            item(map(recognize((tag("@"), word)), |context| {
                let value = &context.fragment()[1..];
                Tag::Context(Token::new(context.map_extra(|_| value)))
            })),
            item(map(recognize((tag("+"), word)), |project| {
                let value = &project.fragment()[1..];
                Tag::Project(Token::new(project.map_extra(|_| value)))
            })),
            item(map(
                separated_pair(is_not(" :"), tag(":"), word),
                |(key, value)| Tag::Named(Token::new(key), Token::new(value)),
            )),
        ))),
    );

    iterator(input.to_span(), tag1).flatten()
}

/// Parse an individual task from the input.
///
pub fn task1(input: Span) -> IResult<Span, Task> {
    let parts = (
        opt(xspace1(map(tag("x"), Token::new))),
        opt(xspace1(item(map(
            delimited(tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
            // Safety: The one_of combinator ensures uppercase is 65..=90.
            |uppercase| unsafe { mem::transmute::<u8, Priority>(uppercase as _) },
        )))),
        opt(xspace1(item(date))),
        opt(xspace1(item(date))),
        map(rest, Token::new),
    );

    let mut parser = map(
        map_parser(not_line_ending, parts),
        |(x, priority, mut finished_on, mut started_on, description)| {
            if started_on.is_none() {
                mem::swap(&mut finished_on, &mut started_on);
            }

            Task {
                x,
                priority,
                finished_on,
                started_on,
                description,
            }
        },
    );

    parser.parse_complete(input)
}

fn date(input: Span) -> IResult<Span, NaiveDate> {
    let triple = (
        terminated(parse_to(take(4usize)), tag("-")),
        terminated(parse_to(take(2usize)), tag("-")),
        parse_to(take(2usize)),
    );

    map_opt(triple, |(y, m, d)| NaiveDate::from_ymd_opt(y, m, d)).parse_complete(input)
}

fn item<'a, P>(
    mut parser: P,
) -> impl Parser<Span<'a>, Output = Token<'a, P::Output>, Error = Error<'a>>
where
    P: Parser<Span<'a>, Error = Error<'a>>,
{
    move |input| {
        let (rest, value) = parser.parse_complete(input)?;
        let span = input.take(input.offset(&rest)).map_extra(|_| value);

        Ok((rest, Token::new(span)))
    }
}

fn not_line_ending(input: Span) -> IResult<Span, Span> {
    let (mut rest, line) = is_not("\r\n").parse_complete(input)?;

    if rest.starts_with("\r\n") {
        rest = rest.take_from(2);
    } else if rest.starts_with("\n") || rest.starts_with("\r") {
        rest = rest.take_from(1);
    }

    Ok((rest, line))
}

fn parse_to<'a, R, P>(parser: P) -> impl Parser<Span<'a>, Output = R, Error = Error<'a>>
where
    R: FromStr,
    P: Parser<Span<'a>, Output = Span<'a>, Error = Error<'a>>,
{
    map_res(parser, |output| output.fragment().parse())
}

fn word(input: Span) -> IResult<Span, Span> {
    take_till1(char::is_whitespace).parse_complete(input)
}

/// Apply the provided parser to the input until one or more space or tab
/// character is found.
///
fn xspace1<'a, P>(parser: P) -> impl Parser<Span<'a>, Output = P::Output, Error = P::Error>
where
    P: Parser<Span<'a>>,
{
    terminated(parser, space1)
}

impl<T> Token<'_, T> {
    /// A str to the source from which self was recognized.
    ///
    pub fn fragment(&self) -> &str {
        self.span.fragment()
    }

    /// A reference to the literal value of the token.
    ///
    pub fn value(&self) -> &T {
        &self.span.extra
    }

    pub fn line(&self) -> u32 {
        self.span.location_line()
    }

    /// The UTF-8 column number of self in the parser input.
    ///
    pub fn column(&self) -> usize {
        self.span.get_utf8_column()
    }

    /// The start and end byte index of self in the parser input.
    ///
    pub fn offset(&self) -> (usize, usize) {
        let span = &self.span;
        let start = span.location_offset();

        (start, start + span.len())
    }

    /// The index of the first byte of self in the parser input.
    ///
    pub fn start(&self) -> usize {
        self.span.location_offset()
    }

    /// The index of the last byte of self in the parser input.
    ///
    pub fn end(&self) -> usize {
        let span = &self.span;
        span.location_offset() + span.len()
    }
}

impl<'a, T> Token<'a, T> {
    #[inline]
    fn new(span: Span<'a, T>) -> Self {
        Self { span }
    }
}

impl<'a, T: Copy> Token<'a, T> {
    #[inline]
    pub(crate) fn to_span(&self) -> Span<'a, T> {
        self.span
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_from_str() {
        assert_eq!(super::from_str("feed tomato plants").count(), 1);
        assert_eq!(
            super::from_str("feed tomato plants\n  water palm\nfeed monstera").count(),
            3
        );
    }

    #[test]
    fn test_not_line_ending() {
        const INPUTS: [&str; 2] = [
            "read until new line\r\nline 2",
            "read until new line\nline 2",
        ];

        for input in INPUTS.into_iter() {
            let (line2, line1) = super::not_line_ending(input.into()).unwrap();

            assert_eq!(
                line1.into_fragment(),
                "read until new line",
                "the input is read until the terminating sequence is reached",
            );

            assert_eq!(
                line2.into_fragment(),
                "line 2",
                "the terminating sequence is removed from the output",
            );
        }
    }

    #[test]
    fn test_task1() {
        super::task1("feed tomato plants".into()).unwrap();
        super::task1("x feed tomato plants".into()).unwrap();
        super::task1("(A) feed tomato plants".into()).unwrap();
        super::task1("2025-06-15 feed tomato plants".into()).unwrap();
        super::task1("2025-06-15 2025-06-15 feed tomato plants".into()).unwrap();
    }
}
