use std::collections::{HashMap, HashSet};
use crate::ast::common::Ident;
use crate::ast::typed_rust::PostType;

#[derive(Clone, Debug)]
pub struct GlobalContext {
    enums : HashMap<String, HashSet<String>>,
    structs : HashMap<String, HashMap<String, PostType>>,
    known_types : HashMap<String, PostType>,
}

impl GlobalContext {

    pub fn new() -> Self {
        Self {
            enums : HashMap::new(),
            structs : HashMap::new(),
            known_types : HashMap::new(),
        }
    }

    pub fn insert(&mut self, name : String, typ : PostType) -> Option<PostType> {
        self.known_types.insert(name, typ)
    }

    pub fn get_known_types(&self) -> &HashMap<String, PostType> {
        &self.known_types
    }

    pub fn get_typ(&self, name : &str) -> Option<&PostType> {
        self.known_types.get(name)
    }
}



#[derive(Clone, Debug)]
pub struct LocalContext {
    vars : Vec<HashMap<String, PostType>>,
}

impl LocalContext {

    pub fn new() -> Self {
        Self {
            vars : Vec::new()
        }
    }

    pub fn add_layer(&mut self) {
        self.vars.push(HashMap::new())
    }

    pub fn pop_layer(&mut self) -> Option<HashMap<String, PostType>> {
        self.vars.pop()
    }

    pub fn get_typ(&self, var_name : &Ident) -> Option<&PostType> {
        for hashmap in self.vars.iter().rev() {
            if let Some(typ) = hashmap.get(var_name.get_content()) {
                return Some(typ)
            }
        }
        None
    }

    pub fn add_var(&mut self, ident : &Ident, typ : &PostType) {
        if let Some(last) = self.vars.last_mut() {
           last.insert(ident.get_content().to_string(), typ.clone());
        } else {
            panic!("should never happend")
        }
    }

    pub fn mark_as_moved(&mut self, _var_name : &Ident) {
        todo!()
    }
}