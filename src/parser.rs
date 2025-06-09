use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_while1};
use nom::character::complete::{line_ending, one_of, space0, space1};
use nom::combinator::{eof, iterator, map, map_opt, map_parser, map_res, opt, peek};
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::{IResult, Parser};
use nom_locate::position;
use std::borrow::Cow;
use std::fmt::{self, Debug, Formatter};
use std::str::FromStr;

#[cfg(feature = "serde")]
use serde::Serialize;

use crate::todotxt::{Description, Priority, Tag, Todo};

type Error<'a> = nom::error::Error<Input<'a>>;
type Input<'a> = nom_locate::LocatedSpan<&'a str>;

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Debug, PartialEq)]
pub struct Located<T> {
    pub(crate) span: Span,
    pub(crate) value: T,
}

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, PartialEq, PartialOrd)]
pub struct Span(usize, usize);

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(input: &str) -> impl Iterator<Item = Todo<'_>> {
    let parse1 = alt((
        map(map_parser(preceded(space0, is_not("\r\n")), todo()), Some),
        map(preceded(space0, peek(one_of("\r\n"))), |_| None),
    ));

    iterator(input.into(), delimited(eol, parse1, eol)).flatten()
}

pub fn parse_tags<'a>(description: &'a Description) -> impl Iterator<Item = Tag<'a>> {
    let typed = |ctor: fn(Located<&'a str>) -> Tag<'a>| {
        move |(pos, output): (Input<'a>, Input<'a>)| {
            let len = output.fragment().len() + 1;
            let from = description.span().start();

            ctor(Located {
                value: output.into_fragment(),
                span: Span::locate_from(&pos, len, from),
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
                |(name, value)| {
                    let len = name.fragment().len() + value.fragment().len() + 1;
                    let from = description.span().start();

                    Tag::Named(Located {
                        value: (name.into_fragment(), value.into_fragment()),
                        span: Span::locate_from(&name, len, from),
                    })
                },
            ),
        ))),
    );

    iterator(description.value().into(), parse1).flatten()
}

fn todo<'a>() -> impl Parser<Input<'a>, Output = Todo<'a>, Error = Error<'a>> {
    let priority = (
        position,
        terminated(
            delimited(tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
            space1,
        ),
    );

    map(
        (
            map(position, |pos: Input<'a>| pos.location_line()),
            opt(map(terminated(tag("x"), space1), |pos| {
                Span::locate(&pos, 1)
            })),
            opt(map(priority, |(pos, data)| {
                let span = Span::locate(&pos, 3);
                Priority(Located { value: data, span })
            })),
            opt((ymd(), opt(ymd()))),
            alt((
                map(preceded(space0, is_not("\r\n")), |output: Input<'a>| {
                    let data = output.fragment().trim_end();

                    Description(Located {
                        value: Cow::Borrowed(data),
                        span: Span::locate(&output, data.len()),
                    })
                }),
                map(eof, |pos| {
                    Description(Located {
                        value: Cow::Borrowed(""),
                        span: Span::locate(&pos, 0),
                    })
                }),
            )),
        ),
        |(line, x, priority, dates, description)| {
            let (date_completed, date_started) = match dates {
                Some((first, Some(second))) => (Some(first), Some(second)),
                Some((first, None)) => (None, Some(first)),
                None => (None, None),
            };

            Todo {
                line,
                x,
                priority,
                date_completed,
                date_started,
                description,
            }
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

impl Span {
    pub fn start(&self) -> usize {
        self.0
    }

    pub fn end(&self) -> usize {
        self.1
    }
}

impl Span {
    #[cfg(test)]
    pub(crate) fn new(start: usize, end: usize) -> Self {
        Self(start, end)
    }

    fn locate(input: &Input, len: usize) -> Self {
        let start = input.get_utf8_column() - 1;
        Self(start, start + len)
    }

    fn locate_from(input: &Input, len: usize, offset: usize) -> Self {
        let Self(start, end) = Self::locate(input, len);
        Self(start + offset, end + offset)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Span({}, {})", self.start(), self.end())
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use super::{Located, Span};
    use crate::todotxt::Description;

    #[test]
    fn test_parse_tags() {
        let text = "feed the tomato plants @home +garden due:2025-06-10";
        let desc = Description(Located {
            value: Cow::Borrowed(text),
            span: Span::new(3, text.len() + 3),
        });

        assert_eq!(super::parse_tags(&desc).collect::<Vec<_>>().len(), 3);
    }
}
