use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_till1};
use nom::character::complete::{multispace0, one_of, space0, space1};
use nom::combinator::{iterator, map, map_opt, map_parser, map_res, opt, recognize, rest};
use nom::sequence::{delimited, preceded, terminated};
use nom::{IResult, Input as ImplInput, Parser};
use nom_locate::{LocatedSpan, position};
use std::borrow::Cow;
use std::mem;
use std::str::FromStr;

#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::task::{Priority, Tag, Task};

type Error<'a> = nom::error::Error<Input<'a>>;
type Input<'a, T = ()> = LocatedSpan<&'a str, T>;

#[derive(Clone, Debug)]
pub struct Span {
    column: usize,
    start: usize,
    len: usize,
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug)]
pub struct Token<T> {
    span: Span,
    #[cfg_attr(feature = "serde", serde(flatten))]
    value: T,
}

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(input: &str) -> impl Iterator<Item = Task<'_>> {
    iterator(input.into(), delimited(multispace0, task1, multispace0))
}

pub fn task1(input: Input) -> IResult<Input, Task> {
    let parts = (
        opt(xspace1(map(tag("x"), |x| Token {
            span: Span::new(1, &x),
            value: true,
        }))),
        opt(xspace1(map(
            (tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
            |(open, uppercase, _)| Token {
                span: Span::new(3, &open),
                // Safety: The one_of combinator ensures uppercase is 65..=90.
                value: unsafe { mem::transmute::<u8, Priority>(uppercase as _) },
            },
        ))),
        opt(xspace1(date)),
        opt(xspace1(date)),
        map(rest, |description: Input| {
            (
                description.location_line(),
                Token {
                    span: Span::new(description.len(), &description),
                    value: Cow::Borrowed(description.into_fragment()),
                },
            )
        }),
    );

    let mut parser = map(
        map_parser(not_line_ending, parts),
        |(x, priority, mut finished_on, mut started_on, (line, description))| {
            if started_on.is_none() {
                mem::swap(&mut finished_on, &mut started_on);
            }

            Task {
                x,
                priority,
                finished_on,
                started_on,
                description,
                line,
            }
        },
    );

    parser.parse(input)
}

pub fn tags<'a>(input: Input<'a>) -> impl Iterator<Item = Token<Tag<'a>>> {
    let typed = |ctor: fn(&'a str) -> Tag<'a>| {
        move |value: Input<'a>| Token {
            span: Span::new(value.len(), &value),
            value: ctor(&value.into_fragment()[1..]),
        }
    };

    let tag1 = map_parser(
        preceded(space0, word),
        opt(alt((
            map(recognize(preceded(tag("@"), word)), typed(Tag::Context)),
            map(recognize(preceded(tag("+"), word)), typed(Tag::Project)),
            map((is_not(" :"), tag(":"), word), |(key, colon, value)| {
                Token {
                    span: Span::new(key.len() + colon.len() + value.len(), &key),
                    value: Tag::Named(key.into_fragment(), value.into_fragment()),
                }
            }),
        ))),
    );

    iterator(input, tag1).flatten()
}

fn date(input: Input) -> IResult<Input, Token<NaiveDate>> {
    let ymd = (
        terminated(parse_to(take(4usize)), tag("-")),
        terminated(parse_to(take(2usize)), tag("-")),
        parse_to(take(2usize)),
    );

    let mut parser = map_opt((position, ymd), |(start, (y, m, d))| {
        Some(Token {
            span: Span::new(10, &start),
            value: NaiveDate::from_ymd_opt(y, m, d)?,
        })
    });

    parser.parse(input)
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

impl Span {
    /// The UTF-8 column number of the token.
    ///
    pub fn column(&self) -> usize {
        self.column
    }

    /// The index of the first byte of the token.
    ///
    pub fn start(&self) -> usize {
        self.start
    }

    /// The index of the last byte of the token.
    ///
    pub fn end(&self) -> usize {
        self.start() + self.len()
    }

    /// The length in bytes of the token.
    ///
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.len
    }
}

impl Span {
    fn new(len: usize, source: &Input) -> Self {
        Self {
            column: source.get_utf8_column(),
            start: source.location_offset(),
            len,
        }
    }
}

#[cfg(feature = "serde")]
impl Serialize for Span {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeTupleStruct;

        let mut state = serializer.serialize_tuple_struct("Span", 2)?;

        state.serialize_field(&self.start)?;
        state.serialize_field(&self.end())?;

        state.end()
    }
}

impl<T> Token<T> {
    /// A reference to the location of the token in the parser input.
    ///
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// A reference to the literal value that the token represents.
    ///
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
            let span = description.span();

            assert_eq!(
                INPUT.get(span.start()..span.end()),
                Some("feed tomato plants"),
            );
        }

        {
            let description = &second.description;
            let span = description.span();

            assert_eq!(
                INPUT.get(span.start()..span.end()),
                Some("write tests for +to2txt"),
            );
        }

        {
            let description = &third.description;
            let span = description.span();

            assert_eq!(
                INPUT.get(span.start()..span.end()),
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
