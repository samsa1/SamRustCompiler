use std::collections::hash_map::HashMap;
use crate::ast::common::Ident;
use crate::ast::typed_rust::PostType;

#[derive(Clone, Debug)]
pub struct GlobalContext {
    enums : HashMap<Ident, Vec<Ident>>,
    structs : HashMap<Ident, Vec<(Ident, PostType)>>,
}



#[derive(Clone, Debug)]
pub struct LocalContext {
    vars : Vec<HashMap<String, PostType>>,
}

impl LocalContext {
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