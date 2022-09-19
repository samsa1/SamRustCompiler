use std::collections::{HashMap, HashSet};
use crate::ast::common::Ident;
use crate::ast::typed_rust::PostType;

#[derive(Clone, Debug)]
pub struct StructInfo {
    hashmap : HashMap<String, (bool, PostType)>
}

impl StructInfo {
    pub fn get_typ(&mut self, name : &str) -> Option<&PostType> {
        match self.hashmap.get_mut(name) {
            Some(mut p) if !p.0 => { p.0 = true; Some(&p.1) },
            _ => None
        }
    }

    pub fn check_finished(self) -> Option<String> {
        for (name, (b, typ)) in self.hashmap.into_iter() {
            if !b {
                return Some(name)
            }
        }
        None
    }

    pub fn new(args : HashMap<String, PostType>) -> Self {
        let mut hashmap = HashMap::new();
        for (name, typ) in args.into_iter() {
            hashmap.insert(name, (false, typ));
        }
        Self {
            hashmap,
        }
    }

    pub fn get_field_typ(&self, name : &str) -> Option<&PostType> {
        self.hashmap.get(name).map(|x| &x.1)
    }
}

#[derive(Clone, Debug)]
pub struct GlobalContext {
    enums : HashMap<String, HashSet<String>>,
    structs : HashMap<String, StructInfo>,
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

    pub fn get_struct(&self, name : &str) -> Option<&StructInfo> {
        self.structs.get(name)
    }

    pub fn struct_infos(&self, name : &str) -> Option<StructInfo> {
        self.structs.get(name).map(|si| si.clone())
    }

    pub fn add_struct(&mut self, name : String, typ : PostType, args : HashMap<String, PostType>) -> Option<PostType> {
        self.structs.insert(name.clone(), StructInfo::new(args));
        self.insert(name, typ)
    }
}



#[derive(Clone, Debug)]
pub struct LocalContext {
    vars : Vec<HashMap<String, (bool, PostType)>>,
}

impl LocalContext {

    pub fn new(in_types : &Vec<(Ident, bool, PostType)>) -> Self {
        let mut in_types2 = HashMap::new();
        for (name, b, typ) in in_types.iter() {
            assert_eq!(true,
                in_types2.insert(name.get_content().to_string(), (*b, typ.clone())).is_none())
        }
        
        Self {
            vars : vec![in_types2]
        }
    }

    pub fn add_layer(&mut self) {
        self.vars.push(HashMap::new())
    }

    pub fn pop_layer(&mut self) -> Option<HashMap<String, (bool, PostType)>> {
        self.vars.pop()
    }

    pub fn get_typ(&self, var_name : &Ident) -> Option<&(bool, PostType)> {
        for hashmap in self.vars.iter().rev() {
            if let Some(typ) = hashmap.get(var_name.get_content()) {
                return Some(typ)
            }
        }
        None
    }

    pub fn add_var(&mut self, ident : &Ident, mutable : bool, typ : &PostType) {
        if let Some(last) = self.vars.last_mut() {
           last.insert(ident.get_content().to_string(), (mutable, typ.clone()));
        } else {
            panic!("should never happend")
        }
    }

    pub fn mark_as_moved(&mut self, _var_name : &Ident) {
        todo!()
    }
}