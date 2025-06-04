use std::str::CharIndices;

#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::todo::{Located, Span};

#[cfg(feature = "serde")]
pub struct SerializeTags<'a>(pub Span, pub &'a str);

#[cfg_attr(
    feature = "serde",
    derive(Serialize),
    serde(tag = "type", rename_all = "lowercase")
)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Tag<'a> {
    Context(Located<&'a str>),
    Project(Located<&'a str>),
    Named(Located<(&'a str, &'a str)>),
}

struct Tokens<'a> {
    iter: CharIndices<'a>,
    len: usize,
}

pub fn tags(at: Span, description: &str) -> impl Iterator<Item = Tag> {
    let line = at.line();
    let offset = at.start();

    Tokens::new(description).filter_map(move |(start, end)| {
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

#[cfg(feature = "serde")]
impl Serialize for SerializeTags<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_seq(tags(self.0, self.1))
    }
}

impl<'a> Tokens<'a> {
    fn new(value: &'a str) -> Self {
        Self {
            iter: value.char_indices(),
            len: value.len(),
        }
    }
}

impl<'a> Iterator for Tokens<'a> {
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
