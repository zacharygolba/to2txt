use std::io::{self, Read};

type Error = Box<dyn std::error::Error>;

fn main() -> Result<(), Error> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let todos: Vec<_> = to2txt::from_str(&input).collect();
    serde_json::to_writer_pretty(io::stdout(), &todos)?;

    Ok(())
}
