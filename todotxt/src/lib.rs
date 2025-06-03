mod tag;
mod todo;

pub use tag::{Span, Tag};
pub use todo::{Priority, Todo};

/// Parse a todo list from the provided `&str`.
///
pub fn from_str(value: &str) -> impl Iterator<Item = Todo> {
    value.lines().filter_map(|line| {
        let input = line.trim();

        if !input.is_empty() {
            Todo::parse(input)
        } else {
            None
        }
    })
}
