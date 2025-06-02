use todotxt::prelude::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn parse(input: &str) -> String {
    if input.is_empty() {
        String::new()
    } else {
        format!("{:#?}", input.tasks().collect::<Vec<Task<'_>>>())
    }
}
