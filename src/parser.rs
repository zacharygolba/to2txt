use std::borrow::Cow;
use std::str::{CharIndices, FromStr};

use chrono::NaiveDate;
use nom::bytes::complete::{is_not, tag, take};
use nom::character::complete::{line_ending, one_of, space0};
use nom::combinator::{iterator, map, map_opt, map_res, opt};
use nom::sequence::{delimited, preceded, terminated};
use nom::{IResult, Parser};
use nom_locate::position;

use crate::todotxt::{Located, Priority, Span, Tag, Todo};

pub type Error<'a> = nom::error::Error<Input<'a>>;
pub type Input<'a> = nom_locate::LocatedSpan<&'a str>;

struct SplitWhitespace<'a> {
    iter: CharIndices<'a>,
    len: usize,
}

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(value: &str) -> impl Iterator<Item = Todo> {
    iterator(value.into(), delimited(eol, todo(), eol))
}

pub fn tags(at: Span, description: &str) -> impl Iterator<Item = Tag> {
    let line = at.line();
    let offset = at.start();

    SplitWhitespace::new(description).filter_map(move |(start, end)| {
        let token = &description[start..end];
        let span = Span::new(line, (start + offset, end + offset));

        if token.starts_with('@') && token.len() > 1 {
            return Some(Tag::Context(Located {
                data: &token[1..],
                span,
            }));
        }

        if token.starts_with('+') && token.len() > 1 {
            return Some(Tag::Project(Located {
                data: &token[1..],
                span,
            }));
        }

        token.split_once(':').and_then(|pair| {
            if !pair.0.is_empty() && !pair.1.is_empty() {
                Some(Tag::Named(Located { data: pair, span }))
            } else {
                None
            }
        })
    })
}

/// Returns a parser that parses a single task on a todo list.
///
pub fn todo<'a>() -> impl Parser<Input<'a>, Output = Todo<'a>, Error = Error<'a>> {
    let parser = preceded(
        space0,
        (
            checkmark(),
            opt(priority()),
            opt((ymd(), opt(ymd()))),
            map(is_not("\r\n"), |output: Input<'a>| Located {
                data: Cow::Borrowed(*output.fragment()),
                span: Span::locate(&output, output.len()),
            }),
        ),
    );

    map(parser, |(checkmark, priority, dates, description)| {
        let (completed, started) = match dates {
            Some((first, Some(second))) => (Some(first), Some(second)),
            Some((first, None)) => (None, Some(first)),
            None => (None, None),
        };

        Todo {
            checkmark,
            priority,
            completed,
            started,
            description,
            _priv: (),
        }
    })
}

fn checkmark<'a>() -> impl Parser<Input<'a>, Output = Option<Span>, Error = Error<'a>> {
    opt(map(terminated(tag("x"), tag(" ")), |at| {
        Span::locate(&at, 1)
    }))
}

fn eol(input: Input) -> IResult<Input, (), Error> {
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

fn priority<'a>() -> impl Parser<Input<'a>, Output = Priority, Error = Error<'a>> {
    let parser = (
        map(position, |at| Span::locate(&at, 3)),
        terminated(
            delimited(tag("("), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag(")")),
            tag(" "),
        ),
    );

    map(parser, |(span, data)| Priority(Located { data, span }))
}

fn ymd<'a>() -> impl Parser<Input<'a>, Output = Located<NaiveDate>, Error = Error<'a>> {
    let triple = (
        terminated(parse_to(take(4usize)), tag("-")),
        terminated(parse_to(take(2usize)), tag("-")),
        parse_to(take(2usize)),
    );

    map_opt(
        (position, terminated(triple, tag(" "))),
        |(at, (y, m, d))| {
            Some(Located {
                data: NaiveDate::from_ymd_opt(y, m, d)?,
                span: Span::locate(&at, 10),
            })
        },
    )
}

impl<'a> SplitWhitespace<'a> {
    fn new(value: &'a str) -> Self {
        Self {
            iter: value.char_indices(),
            len: value.len(),
        }
    }
}

impl<'a> Iterator for SplitWhitespace<'a> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let iter = &mut self.iter;

        let start = iter.find_map(|(i, c)| if c.is_whitespace() { None } else { Some(i) })?;
        let end = iter
            .find_map(|(i, c)| if c.is_whitespace() { Some(i) } else { None })
            .unwrap_or(self.len);

        Some((start, end))
    }
}
