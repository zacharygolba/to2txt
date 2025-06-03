use failure::Error;
use std::io::{self, Read};

fn main() -> Result<(), Error> {
    let mut input = String::new();

    io::stdin().read_to_string(&mut input)?;
    Ok(serde_json::to_writer_pretty(
        io::stdout(),
        &to2txt::parse(&input),
    )?)
}
