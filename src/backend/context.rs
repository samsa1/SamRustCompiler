use super::reg::{Label, Operand, RegQ};
use crate::ast::common::*;
use crate::ast::typed_rust::*;

use std::collections::HashMap;

pub struct Context {
    nb_str: usize,
    strings: HashMap<String, Label>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            nb_str: 0,
            strings: HashMap::new(),
        }
    }

    pub fn get_proj_offset(&self, typ: &PostType, proj: &Projector) -> usize {
        todo!()
    }

    pub fn get_print_fun(&self) -> Label {
        Label::from_str("printf")
    }

    pub fn get_label_for_string(&mut self, str: String) -> Label {
        if self.strings.contains_key(&str) {
            self.strings.get(&str).unwrap().clone()
        } else {
            let label = Label::from_str(&format!("rust_string_{}", self.nb_str));
            self.strings.insert(str, label.clone());
            label
        }
    }

    pub fn get_pos_var(&self, var: &str) -> Operand<RegQ> {
        todo!()
    }
}
