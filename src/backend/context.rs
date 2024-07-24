use crate::ast::common::{NamePath, PathUL};
use std::collections::HashMap;
use write_x86_64::reg::Label;

pub struct Context {
    nb_if_labels: usize,
    nb_while_labels: usize,
    fun_name: PathUL<()>,
    vars: Vec<HashMap<usize, i64>>,
    return_offset: i64,
}

impl Context {
    pub fn new() -> Self {
        Self {
            nb_if_labels: 0,
            nb_while_labels: 0,
            fun_name: PathUL::new(Vec::new()),
            vars: Vec::new(),
            return_offset: 0,
        }
    }

    pub fn set_fun_name(&mut self, fun_name: PathUL<()>) {
        self.fun_name = fun_name;
    }

    pub fn set_return_offset(&mut self, return_offset: i64) {
        self.return_offset = return_offset;
    }

    pub fn self_label(&self) -> Label {
        self.fun_label(&self.fun_name)
    }

    pub fn init(&mut self, args: Vec<(usize, usize)>) {
        let mut offset = 16;
        let mut vars_1 = HashMap::new();
        for (arg_id, size) in args.into_iter().rev() {
            vars_1.insert(arg_id, offset);
            offset += size as i64;
        }
        self.vars = vec![vars_1];
        self.return_offset = offset
    }

    pub fn find(&self, id: usize) -> i64 {
        for layer in self.vars.iter().rev() {
            match layer.get(&id) {
                Some(id) => return *id,
                None => (),
            }
        }
        panic!("ICE")
    }

    pub fn gen_if_labels(&mut self) -> (Label, Label) {
        let str1 = format!("rust_if_else_{}", self.nb_if_labels);
        let str2 = format!("rust_if_end_{}", self.nb_if_labels);
        self.nb_if_labels += 1;
        (Label::from_str(str1), Label::from_str(str2))
    }

    pub fn gen_while_labels(&mut self) -> (Label, Label) {
        let str1 = format!("rust_while_cond_{}", self.nb_while_labels);
        let str2 = format!("rust_while_end_{}", self.nb_while_labels);
        self.nb_while_labels += 1;
        (Label::from_str(str1), Label::from_str(str2))
    }

    fn fun_label_inner(&self, fun_name: &PathUL<()>) -> String {
        let mut fun_name2 = String::new();
        fun_name2.push_str("user_fun");
        for el in fun_name.get_content() {
            match el {
                NamePath::Name(s) => {
                    fun_name2.push_str("..");
                    fun_name2.push_str(s);
                }
                NamePath::Specialisation(_) => todo!(),
            }
        }
        fun_name2
    }

    pub fn fun_label(&self, fun_name: &PathUL<()>) -> Label {
        Label::from_str(self.fun_label_inner(fun_name))
    }

    pub fn bloc_label(&self, b: usize) -> Label {
        let mut str = self.fun_label_inner(&self.fun_name);
        str.push_str(&format!("..bloc{}", b));
        Label::from_str(str)
    }

    pub fn ret_label(&self) -> Label {
        let mut str = self.fun_label_inner(&self.fun_name);
        str.push_str("..ret");
        Label::from_str(str)
    }

    pub fn string_label(&self, fun_name: &PathUL<()>) -> Label {
        let mut fun_name2 = String::new();
        fun_name2.push_str("user_string");
        for el in fun_name.get_content() {
            match el {
                NamePath::Name(s) => {
                    fun_name2.push_str("..");
                    fun_name2.push_str(s);
                }
                NamePath::Specialisation(_) => todo!(),
            }
        }
        Label::from_str(fun_name2)
    }

    pub fn add_layer(&mut self) {
        self.vars.push(HashMap::new())
    }

    pub fn drop_layer(&mut self) {
        assert!(self.vars.pop().is_some());
    }

    pub fn insert(&mut self, id: usize, size: usize, stack_offset: u64) -> u64 {
        self.vars
            .last_mut()
            .unwrap()
            .insert(id, -(stack_offset as i64 + size as i64));
        stack_offset + size as u64
    }

    pub fn get_return_offset(&self) -> i64 {
        self.return_offset
    }
}
