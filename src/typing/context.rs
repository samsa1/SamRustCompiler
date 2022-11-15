use super::consts::Val;
use crate::ast::common::{Ident, Location, TypedUnaop, PathUL, Path, NamePath};
use crate::ast::typed_rust::{Expr, ExprInner, PostType, PostTypeInner};
use crate::frontend::Module;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct TraitInner {
    content: Trait,
//    fun: PathUL<()>,
}

impl TraitInner {
    pub fn implements(&self, t: &Trait) -> bool {
        self.content.implements(t)
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
    is_pub : bool,
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

    pub fn new(is_pub : bool, args: HashMap<String, PostType>) -> Self {
        let mut hashmap = HashMap::new();
        for (name, typ) in args.into_iter() {
            hashmap.insert(name, (false, typ));
        }
        Self { is_pub, hashmap }
    }

    pub fn get_field_typ(&self, name: &str) -> Option<&PostType> {
        self.hashmap.get(name).map(|x| &x.1)
    }

    pub fn get_pub(&self) -> bool {
        self.is_pub
    }
}

#[derive(Debug)]
pub struct Const {
    pub typ: PostType,
    value: Val,
}

impl Const {
    pub fn new(typ: PostType, value: Val) -> Self {
        Self { typ, value }
    }

    pub fn get_value(&self) -> &Val {
        &self.value
    }

    pub fn get_expr(&self) -> Expr {
        match &self.value {
            Val::Uinteger(i, _) => Expr {
                content: Box::new(ExprInner::Int(*i)),
                loc: Location::default(),
                typed: self.typ.clone(),
            },
            Val::Integer(i, _) if *i >= 0 => Expr {
                content: Box::new(ExprInner::Int(*i as u64)),
                loc: Location::default(),
                typed: self.typ.clone(),
            },
            Val::Integer(i, s) => Expr {
                content: Box::new(ExprInner::UnaOp(
                    TypedUnaop::Neg(*s),
                    Expr {
                        content: Box::new(ExprInner::Int((-*i) as u64)),
                        loc: Location::default(),
                        typed: self.typ.clone(),
                    },
                )),
                loc: Location::default(),
                typed: self.typ.clone(),
            },
            Val::String(s) => Expr {
                content: Box::new(ExprInner::String(s.to_string())),
                loc: Location::default(),
                typed: self.typ.clone(),
            },
            Val::Bool(b) => Expr {
                content: Box::new(ExprInner::Bool(*b)),
                loc: Location::default(),
                typed: self.typ.clone(),
            },
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct ModuleInterface {
    structs: HashMap<String, StructInfo>,
    implemented_traits: HashMap<PostType, HashSet<TraitInner>>,
    methods: HashMap<String, HashMap<String, String>>,
    functions : HashMap<String, (bool, PostType)>,
    submodules : HashMap<String, (bool, ModuleInterface)>,
/*     known_types: HashMap<String, PostType>,*/
    sizes: HashMap<String, usize>,
    constants: HashMap<String, (bool, Const)>,
}

impl ModuleInterface {
    pub fn new_inner(module : &Module<crate::ast::rust::File>) -> Self {
        let mut submodules = HashMap::new();
        for (name, (b, module)) in module.submodules.iter() {
            let new = Self::new(module);
            assert!(submodules.insert(name.to_string(), (*b, new)).is_none());
        }
        Self {
            structs: HashMap::new(),
            implemented_traits: HashMap::new(),
            methods: HashMap::new(),
            functions: HashMap::new(),
            submodules,
            sizes: HashMap::new(),
            constants: HashMap::new(),
        }
    }
    pub fn new(module : &Module<crate::ast::rust::File>) -> Self {
        let mut submodules = HashMap::new();
        submodules.insert("crate".to_string(), (true, Self::new_inner(module)));
        let free_type = PostType {
            content: PostTypeInner::FreeType("T".to_string()),
        };
        let vec_type = PostType {
            content: PostTypeInner::Struct(PathUL::from_vec(vec!["std", "vec", "Vec"]), vec![free_type.clone()]),
        };
        let ref_vec_type = PostType {
            content: PostTypeInner::Ref(false, Box::new(vec_type.clone())),
        };
        let mut_ref_vec_type = PostType {
            content: PostTypeInner::Ref(true, Box::new(vec_type.clone())),
        };
        let fun_typ = PostType {
            content: PostTypeInner::Fun(vec!["T".to_string()], vec![], Box::new(vec_type)),
        };
    
        let mut vec_mod = ModuleInterface::empty();
    
        vec_mod.impl_fun("new".to_string(), true, fun_typ);
        let fun_typ = PostType {
            content: PostTypeInner::Fun(
                vec!["T".to_string()],
                vec![ref_vec_type],
                Box::new(PostType::usize()),
            ),
        };
        vec_mod.impl_fun("len".to_string(), true, fun_typ);
    
        let fun_typ = PostType {
            content: PostTypeInner::Fun(
                vec!["T".to_string()],
                vec![mut_ref_vec_type, free_type],
                Box::new(PostType::unit()),
            ),
        };
        vec_mod.impl_fun("push".to_string(), true, fun_typ);
    
        let mut vec_upper_mod = ModuleInterface::empty();
        vec_upper_mod.insert("Vec".to_string(), true, vec_mod);
        vec_upper_mod.impl_method("Vec", "len".to_string(), "len".to_string());
        vec_upper_mod.impl_method("Vec", "push".to_string(), "push".to_string());
    
        let mut std_mod = ModuleInterface::empty();
        std_mod.insert("vec".to_string(), true, vec_upper_mod);
        submodules.insert("std".to_string(), (true, std_mod));
        Self {
            structs: HashMap::new(),
            implemented_traits: HashMap::new(),
            methods: HashMap::new(),
            functions: HashMap::new(),
            submodules,
            sizes: HashMap::new(),
            constants: HashMap::new(),
        }
    }

    pub fn empty() -> Self {
        Self {
            structs: HashMap::new(),
            implemented_traits: HashMap::new(),
            methods: HashMap::new(),
            functions: HashMap::new(),
            submodules : HashMap::new(),
            sizes: HashMap::new(),
            constants: HashMap::new(),
        }
    }

    fn get_struct_inner(&self, path : &Vec<NamePath<(), String>>, pos : usize) -> Option<(bool, &StructInfo)> {
        println!("{} {:?}", pos, path);
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.structs.get(name).map(|el| (el.get_pub(), el)),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((b, sb)) => sb.get_struct_inner(path, pos + 1).map(|(b2, i)| (b2 && *b, i)),
                },
                _ => todo!(),
            }
        }
    }

    pub fn get_struct(&self, path : &PathUL<()>) -> Option<(bool, &StructInfo)> {
        self.get_struct_inner(path.get_content(), 0)
    }

    fn get_traits_inner(&self, path : &Vec<NamePath<(), String>>, pos : usize, maxi : usize, typ : &PostType) -> Option<(bool, &HashSet<TraitInner>)> {
        if pos == maxi {
            self.implemented_traits.get(typ).map(|el| (true, el))
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((b, sb)) => sb.get_traits_inner(path, pos + 1, maxi, typ).map(|(b2, i)| (b2 && *b, i)),
                },
                _ => None
            }
        }
    }

    fn get_fun_inner(&self, path : &Vec<NamePath<(), Ident>>, pos : usize) -> Option<(bool, &PostType)> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.functions.get(name.get_content()).map(|(b, t)| (*b, t)),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name.get_content()) {
                    None => None,
                    Some((b1, sb)) => sb.get_fun_inner(path, pos + 1).map(|(b2, i)| (b2 && *b1, i)),
                },
                _ => None
            }
        }
    }

    pub fn get_fun(&self, path : &Path<()>) -> Option<(bool, &PostType)> {
        self.get_fun_inner(path.get_content(), 0)
    }

    pub fn get_top_fun(&self, name : &str) -> Option<&(bool, PostType)> {
        self.functions.get(name)
    }

    fn get_const_inner(&self, path : &Vec<NamePath<(), Ident>>, pos : usize) -> Option<(bool, &Const)> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.constants.get(name.get_content()).map(|(b, c)| (*b, c)),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name.get_content()) {
                    None => None,
                    Some((b1, sb)) => sb.get_const_inner(path, pos + 1).map(|(b2, c)| (b2 && *b1, c)),
                },
                _ => None
            }
        }
    }

    pub fn get_const(&self, path : &Path<()>) -> Option<(bool, &Const)> {
        self.get_const_inner(path.get_content(), 0)
    }

    fn get_methods_inner(&self, path : &Vec<NamePath<(), String>>, pos : usize) -> Option<&HashMap<String, String>> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.methods.get(name),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((_, sb)) => sb.get_methods_inner(path, pos + 1),
                },
                _ => None
            }
        }
    }

    fn get_methods_mut_inner(&mut self, path : &Vec<NamePath<(), Ident>>, pos : usize) -> Option<&mut HashMap<String, String>> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.methods.get_mut(name.get_content()),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get_mut(name.get_content()) {
                    None => None,
                    Some((_, sb)) => sb.get_methods_mut_inner(path, pos + 1),
                },
                _ => None
            }
        }
    }

    pub fn get_methods(&self, path : &PathUL<()>) -> Option<&HashMap<String, String>> {
        self.get_methods_inner(path.get_content(), 0)
    }

    fn get_size_inner(&self, path : &Vec<NamePath<(), String>>, pos : usize) -> Option<&usize> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.sizes.get(name),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((_, sb)) => sb.get_size_inner(path, pos + 1),
                },
                _ => None
            }
        }
    }

    pub fn get_size(&self, path : &PathUL<(), String>) -> Option<&usize> {
        self.get_size_inner(path.get_content(), 0)
    }

    fn get_module_inner(&self, path : &Vec<NamePath<(), String>>, pos : usize) -> Option<(bool, &Self)> {
        if pos == path.len() {
            Some((true, self))
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((b1, sb)) => sb.get_module_inner(path, pos + 1).map(|(b2, i)| (b2 && *b1, i)),
                },
                _ => None
            }
        }
    }

    fn get_module(&self, path : &PathUL<()>) -> Option<(bool, &Self)> {
        self.get_module_inner(path.get_content(), 0)
    }

    fn get_mut_module_inner(&mut self, path : &Vec<NamePath<(), String>>, pos : usize) -> Option<(bool, &mut Self)> {
        if pos == path.len() {
            Some((true, self))
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get_mut(name) {
                    None => None,
                    Some((b1, sb)) => sb.get_mut_module_inner(path, pos + 1).map(|(b2, i)| (b2 && *b1, i)),
                },
                _ => None
            }
        }
    }

    fn get_mut_module(&mut self, path : &PathUL<()>) -> Option<(bool, &mut Self)> {
        self.get_mut_module_inner(path.get_content(), 0)
    }

    pub fn insert_top_size(&mut self, name : String, size : usize) -> Option<usize> {
        self.sizes.insert(name, size)
    }


    fn insert_size_inner(&mut self, path : &Vec<NamePath<(), String>>, pos : usize, size : usize) -> Option<usize> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.sizes.insert(name.to_string(), size),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get_mut(name) {
                    None => None,
                    Some((_, sb)) => sb.insert_size_inner(path, pos + 1, size),
                },
                _ => None
            }
        }
    }

    pub fn insert_size(&mut self, path : PathUL<(), String>, size : usize) -> Option<usize> {
        self.insert_size_inner(path.get_content(), 0, size)
    }

    fn insert_struct_inner(&mut self, path : &Vec<NamePath<(), String>>, pos : usize, info : StructInfo) -> Option<StructInfo> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.structs.insert(name.to_string(), info),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get_mut(name) {
                    None => None,
                    Some((_, sb)) => sb.insert_struct_inner(path, pos + 1, info),
                },
                _ => None
            }
        }
    }

    pub fn insert_struct(&mut self, path : PathUL<(), String>, is_pub : bool, args : HashMap<String, PostType>) -> Option<StructInfo> {
        self.insert_struct_inner(path.get_content(), 0, StructInfo::new(is_pub, args))
    }


    pub fn impl_trait(&mut self, typ : &PostType, t : Trait) -> bool {
        match self.implemented_traits.get_mut(typ) {
            None => {
                let mut set = HashSet::new();
                set.insert(TraitInner { content : t, });
                self.implemented_traits.insert(typ.clone(), set).is_none()
            },
            Some(set) => set.insert(TraitInner { content : t }),
        }
    }

    pub fn impl_method(&mut self, struct_name : &str, method : String, fun_name : String) -> Option<String> {
        let map = match self.methods.get_mut(struct_name) {
            None => {
                let map = HashMap::new();
                self.methods.insert(struct_name.to_string(), map);
                self.methods.get_mut(struct_name).unwrap()
            },
            Some(map) => map
        };
        map.insert(method, fun_name)
    }

    pub fn impl_fun(&mut self, fun_name : String, public : bool, typ : PostType) -> Option<(bool, PostType)> {
        self.functions.insert(fun_name, (public, typ))
    }

    pub fn insert(&mut self, name : String, public : bool, submod : ModuleInterface) -> Option<(bool, ModuleInterface)> {
        self.submodules.insert(name, (public, submod))
    }
}

#[derive(Debug)]
pub struct GlobalContext {
    path : PathUL<()>,
    modules : ModuleInterface,
    /* structs: HashMap<String, StructInfo>,
    implemented_traits: HashMap<PostType, HashSet<TraitInner>>,
    methods: HashMap<String, HashMap<String, String>>,
    known_types: HashMap<String, PostType>,
    sizes: HashMap<String, usize>,
    constants: HashMap<String, Const>,*/
}

impl GlobalContext {
    pub fn new(path : PathUL<()>, modules : ModuleInterface) -> Self {
        Self {
            path,
            modules,
        }
    }

    pub fn extract_module(self) -> ModuleInterface {
        self.modules
    }

    pub fn has_trait(&self, typ : &PostType, t : &Trait) -> Option<PathUL<()>> {
        match &typ.content {
            PostTypeInner::Struct(path, args) => {
                let typ = match &path.get_content()[path.get_content().len() - 1] {
                    NamePath::Name(s) => PostType { content: PostTypeInner::Struct(PathUL::new(vec![NamePath::Name(s.clone())]), Vec::new()) },
                    _ => todo!(),
                };
                let (_, trait_set) = self.modules.get_traits_inner(path.get_content(), 0, path.get_content().len() - 1, &typ)?;
                for traits in trait_set {
                    if traits.implements(t) {
                        return Some(path.clone());
                    }
                };
                None
            }
            PostTypeInner::BuiltIn(bi) => {
                let (_, trait_set) = self.modules.get_traits_inner(&Vec::new(), 0, 0, typ)?;
                for traits in trait_set {
                    if traits.implements(t) {
                        return Some(PathUL::from_vec(vec![bi.to_str()]));
                    }
                };
                None
            },
            _ => None
        }
    }

    pub fn get_path(&self, name : &str) -> PathUL<()> {
        let mut path = self.path.clone();
        path.push(NamePath::Name(name.to_string()));
        path
    }

    pub fn impl_fun(&mut self, fun_name : String, public : bool, typ : PostType) -> Option<(bool, PostType)> {
        self.modules.get_mut_module(&self.path)?.1.impl_fun(fun_name, public, typ)
    }

    pub fn get_struct(&self, name: &str) -> Option<&StructInfo> {
        println!("{:?}", self.path);
        self.modules.get_module(&self.path)?.1.structs.get(name)
    }

    pub fn get_struct_path(&self, path: &PathUL<()>) -> Option<&StructInfo> {
        match self.modules.get_struct(path) {
            Some((_, si)) => Some(si),
            _ => None,
        }
    }

    pub fn struct_infos(&self, name: &str) -> Option<StructInfo> {
        self.get_struct(name).cloned()
    }

    pub fn struct_path(&self, name: &PathUL<()>) -> Option<StructInfo> {
        self.get_struct_path(name).cloned()
    }

    pub fn get_method_function(&self, type_name: &PathUL<()>, method: &Ident) -> Option<Option<&String>> {
        self.modules
            .get_methods(type_name)
            .map(|hm| hm.get(method.get_content()))
    }

    pub fn get_fun(&self, path: &Path<()>) -> Option<&PostType> {
        match self.modules.get_fun(path) {
            Some((_, f)) => Some(f),
            _ => None,
        }
    }

    pub fn get_top_fun(&self, name: &str) -> Option<&PostType> {
        self.modules.get_module(&self.path)?.1.functions.get(name).map(|(_, f)| f)
    }

    pub fn get_const_val(&self, path: &Path<()>) -> Option<&Const> {
        match self.modules.get_const(path) {
            Some((_, c)) => Some(c),
            _ => None,
        }
    }

    pub fn get_top_const_val(&self, name: &str) -> Option<&Const> {
        self.modules.get_module(&self.path)?.1.constants.get(name).map(|(_, f)| f)
    }

    pub fn get_module(&self, path : &PathUL<()>) -> Option<&ModuleInterface> {
        self.modules.get_module(path).map(|(_, mi)| mi)
    }

    pub fn add_const(&mut self, name : String, public : bool, typ : PostType, value : super::consts::Val) -> Option<Const> {
        let constant = Const { typ, value, };
        self.modules.get_mut_module(&self.path)?.1.constants.insert(name, (public, constant)).map(|(_, c)| c)
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
}
