use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn parse(input: &str) -> String {
    format!("{:#?}", to2txt::parse(input))
}
