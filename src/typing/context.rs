use crate::ast::common::Ident;
use crate::ast::typed_rust::PostType;
use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct TraitInner {
    content: Trait,
    fun: String,
}

impl TraitInner {
    pub fn implements(&self, t: &Trait) -> Option<&str> {
        if self.content.implements(t) {
            Some(&self.fun)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Trait {
    Name(String),
    Parametrized(String, Option<PostType>),
}

impl Trait {
    pub fn implements(&self, t: &Self) -> bool {
        match (self, t) {
            (Self::Name(s), Self::Name(t)) => s == t,
            (Self::Parametrized(s, None), Self::Parametrized(t, _)) => s == t,
            (Self::Parametrized(s, Some(s1)), Self::Parametrized(t, Some(s2))) => {
                s == t && super::types::are_compatible(s1, s2)
            }
            _ => false,
        }
    }

    pub fn clone_trait() -> Self {
        Self::Name("Clone".to_string())
    }
}

#[derive(Clone, Debug)]
pub struct StructInfo {
    hashmap: HashMap<String, (bool, PostType)>,
}

impl StructInfo {
    pub fn get_typ(&mut self, name: &str) -> Option<&PostType> {
        match self.hashmap.get_mut(name) {
            Some(mut p) if !p.0 => {
                p.0 = true;
                Some(&p.1)
            }
            _ => None,
        }
    }

    pub fn check_finished(self) -> Option<String> {
        for (name, (b, _)) in self.hashmap.into_iter() {
            if !b {
                return Some(name);
            }
        }
        None
    }

    pub fn new(args: HashMap<String, PostType>) -> Self {
        let mut hashmap = HashMap::new();
        for (name, typ) in args.into_iter() {
            hashmap.insert(name, (false, typ));
        }
        Self { hashmap }
    }

    pub fn get_field_typ(&self, name: &str) -> Option<&PostType> {
        self.hashmap.get(name).map(|x| &x.1)
    }
}

#[derive(Clone, Debug)]
pub struct GlobalContext {
    enums: HashMap<String, HashSet<String>>,
    structs: HashMap<String, StructInfo>,
    implemented_traits: HashMap<PostType, HashSet<TraitInner>>,
    known_types: HashMap<String, PostType>,
    sizes: HashMap<String, usize>,
}

impl GlobalContext {
    pub fn new() -> Self {
        Self {
            enums: HashMap::new(),
            structs: HashMap::new(),
            implemented_traits: HashMap::new(),
            known_types: HashMap::new(),
            sizes: HashMap::new(),
        }
    }

    pub fn insert_size(&mut self, name: String, size: usize) -> Option<usize> {
        self.sizes.insert(name, size)
    }

    pub fn insert(&mut self, name: String, typ: PostType) -> Option<PostType> {
        self.known_types.insert(name, typ)
    }

    pub fn implement_trait(&mut self, typ: &PostType, t: Trait, fun_name: String) -> bool {
        match self.implemented_traits.get_mut(typ) {
            None => {
                let mut hashset = HashSet::new();
                hashset.insert(TraitInner {
                    content: t,
                    fun: fun_name,
                });
                assert!(self
                    .implemented_traits
                    .insert(typ.clone(), hashset)
                    .is_none());
                true
            }
            Some(traits) => traits.insert(TraitInner {
                content: t,
                fun: fun_name,
            }),
        }
    }

    pub fn has_trait(&self, name: &PostType, t: &Trait) -> Option<&str> {
        for traits in self.implemented_traits.get(name).unwrap().iter() {
            println!("{:?} -> {:?} <=> {:?}", name, traits, t);
            let out = traits.implements(t);
            if out.is_some() {
                println!("yes");
                return out;
            }
        }
        None
    }

    pub fn get_known_types(&self) -> &HashMap<String, PostType> {
        &self.known_types
    }

    pub fn get_typ(&self, name: &str) -> Option<&PostType> {
        self.known_types.get(name)
    }

    pub fn get_size(&self, name: &str) -> Option<usize> {
        self.sizes.get(name).copied()
    }

    pub fn get_struct(&self, name: &str) -> Option<&StructInfo> {
        self.structs.get(name)
    }

    pub fn struct_infos(&self, name: &str) -> Option<StructInfo> {
        self.structs.get(name).cloned()
    }

    pub fn add_struct(
        &mut self,
        name: String,
        typ: PostType,
        args: HashMap<String, PostType>,
    ) -> Option<PostType> {
        self.structs.insert(name.clone(), StructInfo::new(args));
        self.insert(name, typ)
    }
}

#[derive(Clone, Debug)]
pub struct LocalContext {
    vars: Vec<HashMap<String, (bool, PostType)>>,
}

impl LocalContext {
    pub fn new(in_types: &[(Ident, bool, PostType)]) -> Self {
        let mut in_types2 = HashMap::new();
        for (name, b, typ) in in_types.iter() {
            assert!(in_types2
                .insert(name.get_content().to_string(), (*b, typ.clone()))
                .is_none())
        }

        Self {
            vars: vec![in_types2],
        }
    }

    pub fn add_layer(&mut self) {
        self.vars.push(HashMap::new())
    }

    pub fn pop_layer(&mut self) -> Option<HashMap<String, (bool, PostType)>> {
        self.vars.pop()
    }

    pub fn get_typ(&self, var_name: &Ident) -> Option<&(bool, PostType)> {
        for hashmap in self.vars.iter().rev() {
            if let Some(typ) = hashmap.get(var_name.get_content()) {
                return Some(typ);
            }
        }
        None
    }

    pub fn add_var(&mut self, ident: &Ident, mutable: bool, typ: &PostType) {
        if let Some(last) = self.vars.last_mut() {
            last.insert(ident.get_content().to_string(), (mutable, typ.clone()));
        } else {
            panic!("should never happend")
        }
    }

    pub fn mark_as_moved(&mut self, _var_name: &Ident) {
        todo!()
    }
}
