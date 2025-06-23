use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_till1};
use nom::character::complete::{multispace0, one_of, space0, space1};
use nom::combinator::{iterator, map, map_opt, map_parser, map_res, opt, rest};
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::{IResult, Input as ImplInput, Offset, Parser};
use nom_locate::LocatedSpan;
use std::mem;
use std::str::FromStr;

use crate::task::{Priority, Tag, Task};

type Error<'a> = nom::error::Error<LocatedSpan<&'a str>>;
type Input<'a> = LocatedSpan<&'a str>;

#[derive(Clone, Debug)]
pub struct Token<'a, T = ()> {
    span: LocatedSpan<&'a str, T>,
}

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(input: &str) -> impl Iterator<Item = Task<'_>> {
    iterator(input.into(), delimited(multispace0, task1, multispace0))
}

pub fn task1(input: Input) -> IResult<Input, Task> {
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

    parser.parse(input)
}

pub fn tags<'a>(input: &Token<'a>) -> impl Iterator<Item = Token<'a, Tag<'a>>> {
    let tag1 = map_parser(
        preceded(space0, word),
        opt(alt((
            item(map(preceded(tag("@"), word), |context| {
                Tag::Context(Token::new(context))
            })),
            item(map(preceded(tag("+"), word), |project| {
                Tag::Project(Token::new(project))
            })),
            item(map(
                separated_pair(is_not(" :"), tag(":"), word),
                |(key, value)| Tag::Named(Token::new(key), Token::new(value)),
            )),
        ))),
    );

    iterator(input.span, tag1).flatten()
}

fn date(input: Input) -> IResult<Input, NaiveDate> {
    let mut parser = map_opt(
        (
            terminated(parse_to(take(4usize)), tag("-")),
            terminated(parse_to(take(2usize)), tag("-")),
            parse_to(take(2usize)),
        ),
        |(y, m, d)| NaiveDate::from_ymd_opt(y, m, d),
    );

    parser.parse(input)
}

fn item<'a, P>(mut parser: P) -> impl FnMut(Input<'a>) -> IResult<Input<'a>, Token<'a, P::Output>>
where
    P: Parser<Input<'a>, Error = Error<'a>>,
{
    move |input| {
        let (rest, value) = parser.parse(input)?;
        let span = input.take(input.offset(&rest)).map_extra(|_| value);

        Ok((rest, Token::new(span)))
    }
}

fn not_line_ending(input: Input) -> IResult<Input, Input> {
    let mut parser = map(is_not("\n"), |output: Input| {
        if output.ends_with('\r') {
            output.take(output.len() - 1)
        } else {
            output
        }
    });

    parser.parse(input)
}

fn parse_to<'a, R, P>(parser: P) -> impl Parser<Input<'a>, Output = R, Error = Error<'a>>
where
    R: FromStr,
    P: Parser<Input<'a>, Output = Input<'a>, Error = Error<'a>>,
{
    map_res(parser, |output| output.fragment().parse())
}

fn word(input: Input) -> IResult<Input, Input> {
    take_till1(char::is_whitespace).parse(input)
}

/// Apply the provided parser to the input until one or more space or tab
/// character is found.
///
fn xspace1<'a, P>(parser: P) -> impl Parser<Input<'a>, Output = P::Output, Error = P::Error>
where
    P: Parser<Input<'a>>,
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
    pub(crate) fn new(span: LocatedSpan<&'a str, T>) -> Self {
        Self { span }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_span_range() {
        const INPUT: &str = "
            feed tomato plants
            x (Z) 2025-06-15 2025-06-01 write tests for +to2txt
            x (A) 2025-06-10 2025-06-01 deploy repl example to vercel +to2txt
        ";

        let mut tasks = super::from_str(INPUT);

        let first = tasks.next().unwrap();
        let second = tasks.next().unwrap();
        let third = tasks.next().unwrap();

        assert!(
            tasks.next().is_none(),
            "expected todo.txt to only have 3 tasks"
        );

        {
            let description = &first.description;

            assert_eq!(
                INPUT.get(description.start()..description.end()),
                Some("feed tomato plants"),
            );
        }

        {
            let description = &second.description;

            assert_eq!(
                INPUT.get(description.start()..description.end()),
                Some("write tests for +to2txt"),
            );
        }

        {
            let description = &third.description;

            assert_eq!(
                INPUT.get(description.start()..description.end()),
                Some("deploy repl example to vercel +to2txt"),
            );
        }
    }

    #[test]
    fn test_from_str() {
        assert_eq!(super::from_str("feed tomato plants").count(), 1);
        assert_eq!(
            super::from_str("feed tomato plants\n  water palm\nfeed monstera").count(),
            3
        );
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
