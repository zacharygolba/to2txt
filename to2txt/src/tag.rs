use std::str::CharIndices;

#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

#[cfg(feature = "serde")]
pub struct SerializeTags<'a>(pub &'a str);

#[cfg_attr(feature = "serde", derive(Serialize))]
#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd)]
pub struct Span(usize, usize);

#[cfg_attr(
    feature = "serde",
    derive(Serialize),
    serde(tag = "type", rename_all = "lowercase")
)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd)]
pub enum Tag<'a> {
    Context {
        span: Span,
        value: &'a str,
    },
    Project {
        span: Span,
        value: &'a str,
    },
    Named {
        span: Span,
        name: &'a str,
        value: &'a str,
    },
}

struct Tokens<'a> {
    iter: CharIndices<'a>,
    len: usize,
}

pub fn tags(description: &str) -> impl Iterator<Item = Tag> {
    Tokens::new(description).filter_map(|span| {
        let token = &description[span.0..span.1];

        if token.starts_with('@') && token.len() > 1 {
            return Some(Tag::Context {
                span,
                value: &token[1..],
            });
        }

        if token.starts_with('+') && token.len() > 1 {
            return Some(Tag::Project {
                span,
                value: &token[1..],
            });
        }

        token.split_once(':').and_then(|(name, value)| {
            if !name.is_empty() && !value.is_empty() {
                Some(Tag::Named { span, name, value })
            } else {
                None
            }
        })
    })
}

#[cfg(feature = "serde")]
impl Serialize for SerializeTags<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_seq(tags(self.0))
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
    type Item = Span;

    fn next(&mut self) -> Option<Self::Item> {
        let iter = &mut self.iter;

        let start = iter.find_map(|(i, c)| if c.is_whitespace() { None } else { Some(i) })?;
        let end = iter
            .find_map(|(i, c)| if c.is_whitespace() { Some(i) } else { None })
            .unwrap_or(self.len);

        Some(Span(start, end))
    }
}
