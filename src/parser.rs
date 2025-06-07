use chrono::NaiveDate;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take, take_while1};
use nom::character::complete::{line_ending, one_of, space0, space1};
use nom::combinator::{iterator, map, map_opt, map_parser, map_res, opt, verify};
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::{IResult, Parser};
use nom_locate::position;
use std::borrow::Cow;
use std::str::FromStr;

use crate::todotxt::{Description, Located, Priority, Span, Tag, Todo};

pub type Input<'a> = nom_locate::LocatedSpan<&'a str>;

type Error<'a> = nom::error::Error<Input<'a>>;
type Headers = (
    Option<Span>,
    Option<Priority>,
    Option<(Located<NaiveDate>, Option<Located<NaiveDate>>)>,
);

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(value: &str) -> impl Iterator<Item = Todo<'_>> {
    iterator(value.into(), delimited(eol, todo(), eol)).filter(|todo| {
        !todo.description.text().trim().is_empty()
            || !matches!(
                todo,
                Todo {
                    x: None,
                    priority: None,
                    date_completed: None,
                    date_started: None,
                    ..
                }
            )
    })
}

pub fn tags<'a>(description: &'a Description) -> impl Iterator<Item = Tag<'a>> {
    let typed = |ctor: fn(Located<&'a str>) -> Tag<'a>| {
        move |(pos, output): (Input<'a>, Input<'a>)| {
            let len = output.fragment().len() + 1;
            let from = description.span().start();

            ctor(Located {
                data: output.into_fragment(),
                span: Span::locate_from(&pos, len, from),
            })
        }
    };

    let parse1 = map_parser(
        preceded(space0, word()),
        opt(alt((
            map((tag("@"), word()), typed(Tag::Context)),
            map((tag("+"), word()), typed(Tag::Project)),
            map(separated_pair(word(), tag(":"), word()), |(name, value)| {
                let len = name.fragment().len() + value.fragment().len() + 1;
                let from = description.span().start();

                Tag::Named(Located {
                    data: (name.into_fragment(), value.into_fragment()),
                    span: Span::locate_from(&name, len, from),
                })
            }),
        ))),
    );

    iterator(description.text().into(), parse1).flatten()
}

/// Returns a parser that parses a single task on a todo list.
///
pub fn todo<'a>() -> impl Parser<Input<'a>, Output = Todo<'a>, Error = Error<'a>> {
    alt((
        map(
            (position, preceded(space0, headers()), description()),
            |(pos, headers, description)| {
                let (x, priority, dates) = headers;
                let (date_completed, date_started) = match dates {
                    Some((first, Some(second))) => (Some(first), Some(second)),
                    Some((first, None)) => (None, Some(first)),
                    None => (None, None),
                };
                Todo {
                    line: pos.location_line(),
                    x,
                    priority,
                    date_completed,
                    date_started,
                    description,
                }
            },
        ),
        map((position, description()), |(pos, description)| Todo {
            line: pos.location_line(),
            x: None,
            priority: None,
            date_completed: None,
            date_started: None,
            description,
        }),
    ))
}

fn description<'a>() -> impl Parser<Input<'a>, Output = Description<'a>, Error = Error<'a>> {
    map(is_not("\r\n"), |output: Input<'a>| {
        let text = output.fragment().trim_end();
        let offset = text.len() - text.trim_start().len();
        let span_start = output.get_utf8_column() - 1 + offset;

        Description(Located {
            data: Cow::Borrowed(&text[offset..]),
            span: Span::new(span_start, span_start + text.len()),
        })
    })
}

fn headers<'a>() -> impl Parser<Input<'a>, Output = Headers, Error = Error<'a>> {
    verify(
        (
            opt(map(terminated(tag("x"), space1), |pos| {
                Span::locate(&pos, 1)
            })),
            opt(map(
                (
                    position,
                    terminated(
                        delimited(tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
                        space1,
                    ),
                ),
                |(pos, data)| {
                    let span = Span::locate(&pos, 3);
                    Priority(Located { data, span })
                },
            )),
            opt((ymd(), opt(ymd()))),
        ),
        |output| !matches!(output, (None, None, None)),
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

    map_opt((position, terminated(triple, space1)), |(at, (y, m, d))| {
        Some(Located {
            data: NaiveDate::from_ymd_opt(y, m, d)?,
            span: Span::locate(&at, 10),
        })
    })
}

#[cfg(test)]
mod tests {
    use crate::todotxt::{Description, Located, Span};
    use std::borrow::Cow;

    #[test]
    fn test_parse_tags() {
        let text = "feed the tomato plants @home +garden";
        let desc = Description(Located {
            data: Cow::Borrowed(text),
            span: Span::new(3, text.len() + 3),
        });

        assert_eq!(super::tags(&desc).collect::<Vec<_>>().len(), 2);
    }
}
