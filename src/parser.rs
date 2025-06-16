use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_till1};
use nom::character::complete::{multispace0, one_of, space0, space1};
use nom::combinator::{iterator, map, map_opt, map_parser, map_res, opt, rest};
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::{IResult, Input, Parser};
use nom_locate::position;
use std::borrow::Cow;
use std::mem;
use std::str::FromStr;

#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::task::{Priority, Tag, Task};

type Error<'a> = nom::error::Error<Text<'a>>;
type Text<'a> = nom_locate::LocatedSpan<&'a str>;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Span {
    column: usize,
    start: usize,
    len: usize,
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug)]
pub struct Token<T> {
    #[cfg_attr(feature = "serde", serde(flatten))]
    value: T,
    span: Span,
}

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(input: &str) -> impl Iterator<Item = Task<'_>> {
    iterator(input.into(), delimited(multispace0, task1, multispace0))
}

pub fn task1(input: Text) -> IResult<Text, Task> {
    let parts = (
        position,
        opt(xspace1(map(tag("x"), |x| Token {
            value: true,
            span: Span::new(1, &x),
        }))),
        opt(xspace1(map(
            (tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
            |(open, uppercase, _)| Token {
                // Safety: The one_of combinator ensures uppercase is 65..=90.
                value: unsafe { mem::transmute::<u32, Priority>(uppercase as _) },
                span: Span::new(3, &open),
            },
        ))),
        opt(xspace1(date)),
        opt(xspace1(date)),
        map(rest, |output: Text| {
            let value = Cow::Borrowed(output.fragment().trim_ascii_end());
            let span = Span::new(value.len(), &output);

            Token { value, span }
        }),
    );

    let mut parser = map(
        map_parser(not_line_ending, parts),
        |(start, x, priority, mut finished_on, mut started_on, description)| {
            let line = start.location_line();

            if started_on.is_none() {
                mem::swap(&mut finished_on, &mut started_on);
            }

            Task {
                x,
                line,
                priority,
                finished_on,
                started_on,
                description,
            }
        },
    );

    parser.parse(input)
}

pub fn tags<'a>(input: Text<'a>) -> impl Iterator<Item = Token<Tag<'a>>> {
    let typed = |ctor: fn(&'a str) -> Tag<'a>| {
        move |(from, output): (Text<'a>, Text<'a>)| Token {
            value: ctor(output.fragment()),
            span: Span::new(output.len() + 1, &from),
        }
    };

    let tag1 = map_parser(
        preceded(space0, word),
        opt(alt((
            map((tag("@"), word), typed(Tag::Context)),
            map((tag("+"), word), typed(Tag::Project)),
            map(separated_pair(is_not(" :"), tag(":"), word), |(k, v)| {
                Token {
                    value: Tag::Named(k.fragment(), v.fragment()),
                    span: Span::new(k.len() + v.len() + 1, &k),
                }
            }),
        ))),
    );

    iterator(input, tag1).flatten()
}

fn date(input: Text) -> IResult<Text, Token<NaiveDate>> {
    let ymd = (
        terminated(parse_to(take(4usize)), tag("-")),
        terminated(parse_to(take(2usize)), tag("-")),
        parse_to(take(2usize)),
    );

    let mut parser = map_opt((position, ymd), |(start, (y, m, d))| {
        Some(Token {
            value: NaiveDate::from_ymd_opt(y, m, d)?,
            span: Span::new(10, &start),
        })
    });

    parser.parse(input)
}

fn not_line_ending(input: Text) -> IResult<Text, Text> {
    let mut parser = map(is_not("\n"), |output: Text| {
        if output.ends_with('\r') {
            output.take(output.len() - 1)
        } else {
            output
        }
    });

    parser.parse(input)
}

fn parse_to<'a, R, P>(parser: P) -> impl Parser<Text<'a>, Output = R, Error = Error<'a>>
where
    R: FromStr,
    P: Parser<Text<'a>, Output = Text<'a>, Error = Error<'a>>,
{
    map_res(parser, |output| output.fragment().parse())
}

fn word(input: Text) -> IResult<Text, Text> {
    take_till1(char::is_whitespace).parse(input)
}

/// Apply the provided parser to the input until one or more space or tab
/// character is found.
///
fn xspace1<'a, P>(parser: P) -> impl Parser<Text<'a>, Output = P::Output, Error = P::Error>
where
    P: Parser<Text<'a>>,
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
    fn new(len: usize, source: &Text) -> Self {
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
    /// A reference to the literal value that the token represents.
    ///
    pub fn value(&self) -> &T {
        &self.value
    }

    /// A reference to the location of the token in the parser input.
    ///
    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl Token<Cow<'_, str>> {
    pub fn as_str(&self) -> &str {
        self.value.as_ref()
    }

    pub fn into_string(self) -> String {
        self.value.into_owned()
    }

    pub(crate) fn as_input(&self, line: u32) -> Text<'_> {
        let fragment = self.as_str();
        let span = self.span();

        unsafe {
            // Safety:
            //
            // Any span produced from the following type will be >=
            // span.start() and <= span.end().
            //
            Text::new_from_raw_offset(span.start(), line, fragment, ())
        }
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
    fn test_parse_tags() {
        const INPUT: &str = "feed the tomato plants @home +garden due:2025-06-10";
        let (_, task) = super::task1(INPUT.into()).unwrap();
        let tags = super::tags(task.description.as_input(task.line)).collect::<Vec<_>>();

        assert_eq!(tags.len(), 3);
    }

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
