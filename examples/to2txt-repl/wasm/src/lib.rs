use to2txt::Todo;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn parse(input: &str) -> String {
    let todos: Vec<Todo> = to2txt::from_str(input).collect();
    serde_json::to_string_pretty(&todos).unwrap_or_default()
}
