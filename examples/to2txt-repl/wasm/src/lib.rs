use std::fmt::{self, Debug, Formatter};

use wasm_bindgen::prelude::*;

struct Tasks<'a>(&'a str);

impl Debug for Tasks<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_list().entries(to2txt::from_str(self.0)).finish()
    }
}

#[wasm_bindgen]
pub fn parse(input: &str) -> String {
    format!("{:#?}", Tasks(input))
}
