mod s_expr;

use std::fmt::Write;
use wasm_bindgen::prelude::*;

use s_expr::WriteSExpr;

#[wasm_bindgen]
pub fn parse(input: &str) -> Result<String, String> {
    let mut output = String::new();

    for task in to2txt::from_str(input) {
        let s_expr = WriteSExpr::new(&task);
        if let Err(e) = writeln!(&mut output, "{}", s_expr) {
            return Err(e.to_string());
        }
    }

    Ok(output)
}
