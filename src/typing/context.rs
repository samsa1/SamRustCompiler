use super::consts::Val;
use crate::ast::common::{Ident, Location, NamePath, Path, PathUL};
use crate::ast::operators::TUnaop;
use crate::ast::typed_rust::{Expr, ExprInner, PostType, PostTypeInner};
use crate::frontend::Module;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub enum TraitEL {
    Fun(Vec<PostType>, PostType),
    Const(PostType),
}

#[derive(Clone, Debug)]
pub struct TraitInterface {
    elements: HashMap<String, (bool, TraitEL)>,
}

impl TraitInterface {
    pub fn new(elements: Vec<(String, TraitEL)>) -> Self {
        Self {
            elements: elements
                .into_iter()
                .map(|(s, el)| (s, (false, el)))
                .collect(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&TraitEL> {
        self.elements.get(name).map(|(_, el)| el)
    }
}

#[derive(Clone, Debug)]
pub struct TraitInfo {
    interface: TraitInterface,
    implemented: HashMap<PostType, usize>,
    free_types: HashSet<String>,
}

impl TraitInfo {
    pub fn new(interface: TraitInterface) -> Self {
        Self {
            interface,
            implemented: HashMap::new(),
            free_types: HashSet::new(),
        }
    }

    pub fn get_interface(&self) -> &TraitInterface {
        &self.interface
    }

    pub fn impl_for(&mut self, typ: PostType) -> Option<usize> {
        if self.implemented.contains_key(&typ) {
            self.implemented.get(&typ).map(|i| *i)
        } else {
            let id = self.implemented.len();
            assert!(self.implemented.insert(typ, id).is_none());
            Some(id)
        }
    }

    // true iff was not already implemented
    pub fn impl_for_free(&mut self, name: String) -> bool {
        self.free_types.insert(name)
    }

    pub fn clean_free_impl(&mut self) {
        self.free_types = HashSet::new();
    }

    fn implemented_for(&self, typ: &PostType) -> bool {
        match &typ.content {
            PostTypeInner::FreeType(str) => self.free_types.contains(str),
            _ => self.implemented.contains_key(typ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct StructInfo {
    is_pub: bool,
    size: usize,
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

    pub fn new(size: usize, is_pub: bool, args: HashMap<String, PostType>) -> Self {
        let mut hashmap = HashMap::new();
        for (name, typ) in args.into_iter() {
            hashmap.insert(name, (false, typ));
        }
        Self {
            size,
            is_pub,
            hashmap,
        }
    }

    pub fn get_field_typ(&self, name: &str) -> Option<&PostType> {
        self.hashmap.get(name).map(|x| &x.1)
    }

    pub fn get_pub(&self) -> bool {
        self.is_pub
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn args(self) -> HashMap<String, (bool, PostType)> {
        self.hashmap
    }

    pub fn change_crate_to(self, id: &str) -> Self {
        Self {
            hashmap: self
                .hashmap
                .into_iter()
                .map(|(n, (b, typ))| {
                    (
                        n,
                        (
                            b,
                            crate::passes::change_crate_name::rewrite_type(typ, "crate", id),
                        ),
                    )
                })
                .collect(),
            ..self
        }
    }
}
#[derive(Clone, Debug)]
pub struct EnumInfo {
    is_pub: bool,
    size: usize,
    hashmap: HashMap<String, (bool, Vec<PostType>, u64, usize)>,
    free_types: Vec<String>,
}

impl EnumInfo {
    pub fn new(size: usize, is_pub: bool, args: HashMap<String, (usize, Vec<PostType>)>) -> Self {
        let mut hashmap = HashMap::new();
        for (id, (name, (size, typ))) in args.into_iter().enumerate() {
            hashmap.insert(name, (false, typ, id as u64, size));
        }
        Self {
            size,
            is_pub,
            hashmap,
            free_types: Vec::new(),
        }
    }

    pub fn update_constructor(&mut self, name: &str) -> Option<(bool, &Vec<PostType>)> {
        match self.hashmap.get_mut(name) {
            Some(mut p) => {
                let old = p.0;
                p.0 = true;
                Some((old, &p.1))
            }
            _ => None,
        }
    }

    pub fn get_constructor(&self, name: &str) -> Option<(bool, &Vec<PostType>)> {
        self.hashmap.get(name).map(|(b, v, _, _)| (*b, v))
    }

    pub fn get_cons_id(&self, name: &str) -> Option<(u64, usize)> {
        self.hashmap.get(name).map(|(_, _, id, pad)| (*id, *pad))
    }

    pub fn get_free_types(&self) -> &Vec<String> {
        &self.free_types
    }

    pub fn check_finished(self) -> Option<String> {
        for (name, (b, _, _, _)) in self.hashmap.into_iter() {
            if !b {
                return Some(name);
            }
        }
        None
    }

    pub fn get_pub(&self) -> bool {
        self.is_pub
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn change_crate_to(self, id: &str) -> Self {
        Self {
            hashmap: self
                .hashmap
                .into_iter()
                .map(|(n, (b, vec_typ, i1, i2))| {
                    (
                        n,
                        (
                            b,
                            vec_typ
                                .into_iter()
                                .map(|typ| {
                                    crate::passes::change_crate_name::rewrite_type(typ, "crate", id)
                                })
                                .collect(),
                            i1,
                            i2,
                        ),
                    )
                })
                .collect(),
            ..self
        }
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
                    TUnaop::Neg(*s),
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
            Val::Struct(_, _) => todo!(),
            Val::Tuple(_) => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunInfo {
    is_pub: bool,
    free_types: Vec<(String, Vec<PathUL<()>>)>,
    args: Vec<PostType>,
    out_type: PostType,
    dependancies: HashMap<PathUL<()>, HashSet<Vec<PostType>>>,
    specialised: HashMap<Vec<PostType>, usize>,
}

impl FunInfo {
    fn new(
        is_pub: bool,
        free_types: Vec<(String, Vec<PathUL<()>>)>,
        args: Vec<PostType>,
        out_type: PostType,
    ) -> Self {
        Self {
            is_pub,
            free_types,
            args,
            out_type,
            dependancies: HashMap::new(),
            specialised: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    pub fn get_pub(&self) -> bool {
        self.is_pub
    }

    pub fn get_typ(&self) -> (&Vec<(String, Vec<PathUL<()>>)>, &Vec<PostType>, &PostType) {
        (&self.free_types, &self.args, &self.out_type)
    }

    pub fn get_free(&self) -> &Vec<(String, Vec<PathUL<()>>)> {
        &self.free_types
    }

    pub fn get_args(&self) -> &Vec<PostType> {
        &self.args
    }

    pub fn get_out(&self) -> &PostType {
        &self.out_type
    }

    pub fn get_dependancies(&self) -> &HashMap<PathUL<()>, HashSet<Vec<PostType>>> {
        &self.dependancies
    }

    pub fn set_dependancies(&mut self, dependancies: HashMap<PathUL<()>, HashSet<Vec<PostType>>>) {
        assert!(self.dependancies.is_empty());
        self.dependancies = dependancies
    }

    pub fn add_version(&mut self, args: &Vec<PostType>) -> bool {
        match self.specialised.get(args) {
            Some(_) => false,
            None => {
                let v = self.specialised.len();
                self.specialised.insert(args.clone(), v);
                true
            }
        }
    }

    pub fn get_ids(&self) -> &HashMap<Vec<PostType>, usize> {
        &self.specialised
    }

    pub fn get_id(&self, args: &Vec<PostType>) -> Option<&usize> {
        self.specialised.get(args)
    }

    pub fn change_crate_to(self, id: &str) -> Self {
        assert!(self.specialised.is_empty());
        Self {
            is_pub: self.is_pub,
            free_types: self.free_types,
            args: self
                .args
                .into_iter()
                .map(|typ| crate::passes::change_crate_name::rewrite_type(typ, "crate", id))
                .collect(),
            out_type: crate::passes::change_crate_name::rewrite_type(self.out_type, "crate", id),
            dependancies: self
                .dependancies
                .into_iter()
                .map(|(path, set)| {
                    (
                        path.rewrite_base("crate", id),
                        change_hashset_crate_to(set, id),
                    )
                })
                .collect(),
            specialised: HashMap::new(),
        }
    }
}

fn change_hashset_crate_to(set: HashSet<Vec<PostType>>, id: &str) -> HashSet<Vec<PostType>> {
    set.into_iter()
        .map(|vec| {
            vec.into_iter()
                .map(|typ| crate::passes::change_crate_name::rewrite_type(typ, "crate", id))
                .collect()
        })
        .collect()
}

fn change_hashmap_crate_to(
    hm: HashMap<String, PathUL<()>>,
    id: &str,
) -> HashMap<String, PathUL<()>> {
    hm.into_iter()
        .map(|(n, path)| (n, path.rewrite_base("crate", id)))
        .collect()
}

impl std::fmt::Debug for ModuleInterface {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleInterface")
            .field("submodules", &self.submodules)
            .field("sizes", &self.sizes)
            .finish()
    }
}

pub struct ModuleInterface {
    pub structs: HashMap<String, StructInfo>,
    pub enums: HashMap<String, EnumInfo>,
    pub traits: HashMap<String, TraitInfo>,
    methods: HashMap<String, HashMap<String, PathUL<()>>>,
    functions: HashMap<String, FunInfo>,
    pub submodules: HashMap<String, (bool, ModuleInterface)>,
    sizes: HashMap<String, usize>,
    constants: HashMap<String, (bool, Const)>,
}

impl ModuleInterface {
    pub fn new_inner(module: &Module<crate::ast::rust::File>) -> Self {
        let mut submodules = HashMap::new();
        for (name, (b, module)) in module.submodules.iter() {
            let new = Self::new_inner(module);
            assert!(submodules.insert(name.to_string(), (*b, new)).is_none());
        }
        Self {
            structs: HashMap::new(),
            enums: HashMap::new(),
            traits: HashMap::new(),
            methods: HashMap::new(),
            functions: HashMap::new(),
            submodules,
            sizes: HashMap::new(),
            constants: HashMap::new(),
        }
    }
    pub fn new(module: &Module<crate::ast::rust::File>) -> Self {
        let mut submodules = HashMap::new();
        submodules.insert("crate".to_string(), (true, Self::new_inner(module)));
        let free_type = PostType {
            content: PostTypeInner::FreeType("T".to_string()),
        };
        let vec_type = PostType {
            content: PostTypeInner::Struct(
                PathUL::from_vec(vec!["std", "vec", "Vec"]),
                vec![free_type.clone()],
            ),
        };
        let ref_vec_type = PostType {
            content: PostTypeInner::Ref(false, Box::new(vec_type.clone())),
        };
        let mut_ref_vec_type = PostType {
            content: PostTypeInner::Ref(true, Box::new(vec_type.clone())),
        };

        let mut vec_mod = ModuleInterface::empty();

        vec_mod.impl_fun(
            "new".to_string(),
            true,
            vec![("T".to_string(), Vec::new())],
            vec![],
            vec_type,
        );
        vec_mod.impl_fun(
            "len".to_string(),
            true,
            vec![("T".to_string(), Vec::new())],
            vec![ref_vec_type],
            PostType::usize(),
        );
        vec_mod.impl_fun(
            "push".to_string(),
            true,
            vec![("T".to_string(), Vec::new())],
            vec![mut_ref_vec_type, free_type],
            PostType::unit(),
        );

        let mut vec_upper_mod = ModuleInterface::empty();
        vec_upper_mod.insert("Vec".to_string(), true, vec_mod);
        vec_upper_mod.impl_method(
            "Vec",
            "len".to_string(),
            PathUL::from_vec(vec!["Vec", "len"]),
        );
        vec_upper_mod.impl_method(
            "Vec",
            "push".to_string(),
            PathUL::from_vec(vec!["Vec", "push"]),
        );

        let mut ops_mod = ModuleInterface::empty();
        let elements = vec![(
            "not".to_string(),
            TraitEL::Fun(vec![PostType::free("Self")], PostType::bool()),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("Not".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "neg".to_string(),
            TraitEL::Fun(vec![PostType::free("Self")], PostType::free("Self")),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("Neg".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "add".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("Add".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "sub".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("Sub".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "mul".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("Mul".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "mod".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("Mod".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "div".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("Div".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "shl".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("Shl".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "shr".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("Shr".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "bit_and".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("BitAnd".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "bit_or".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("BitOr".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "and".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("And".to_string(), TraitInfo::new(interface));

        let elements = vec![(
            "or".to_string(),
            TraitEL::Fun(
                vec![PostType::free("Self"), PostType::free("Self")],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("Or".to_string(), TraitInfo::new(interface));

        let elements = vec![
            (
                "eq".to_string(),
                TraitEL::Fun(
                    vec![PostType::free("Self"), PostType::free("Self")],
                    PostType::bool(),
                ),
            ),
            (
                "ne".to_string(),
                TraitEL::Fun(
                    vec![PostType::free("Self"), PostType::free("Self")],
                    PostType::bool(),
                ),
            ),
        ];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("PartialEq".to_string(), TraitInfo::new(interface));

        let elements = vec![
            (
                "gr".to_string(),
                TraitEL::Fun(
                    vec![PostType::free("Self"), PostType::free("Self")],
                    PostType::bool(),
                ),
            ),
            (
                "ge".to_string(),
                TraitEL::Fun(
                    vec![PostType::free("Self"), PostType::free("Self")],
                    PostType::bool(),
                ),
            ),
            (
                "lo".to_string(),
                TraitEL::Fun(
                    vec![PostType::free("Self"), PostType::free("Self")],
                    PostType::bool(),
                ),
            ),
            (
                "le".to_string(),
                TraitEL::Fun(
                    vec![PostType::free("Self"), PostType::free("Self")],
                    PostType::bool(),
                ),
            ),
        ];
        let interface = TraitInterface::new(elements);
        ops_mod.new_trait("PartialOrd".to_string(), TraitInfo::new(interface));

        let mut marker_mod = ModuleInterface::empty();
        let elements = vec![(
            "copy".to_string(),
            TraitEL::Fun(
                vec![PostType {
                    content: PostTypeInner::Ref(false, Box::new(PostType::free("Self"))),
                }],
                PostType::free("Self"),
            ),
        )];
        let interface = TraitInterface::new(elements);
        marker_mod.new_trait("Copy".to_string(), TraitInfo::new(interface));

        let mut std_mod = ModuleInterface::empty();
        std_mod.insert("vec".to_string(), true, vec_upper_mod);
        std_mod.insert("ops".to_string(), true, ops_mod);
        std_mod.insert("marker".to_string(), true, marker_mod);

        submodules.insert("std".to_string(), (true, std_mod));
        Self {
            structs: HashMap::new(),
            enums: HashMap::new(),
            traits: HashMap::new(),
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
            enums: HashMap::new(),
            traits: HashMap::new(),
            methods: HashMap::new(),
            functions: HashMap::new(),
            submodules: HashMap::new(),
            sizes: HashMap::new(),
            constants: HashMap::new(),
        }
    }

    fn extract_inner(self, id: &str) -> Self {
        let mut submodules = HashMap::new();
        for (name, (b, module)) in self.submodules.into_iter() {
            submodules.insert(name, (b, module.extract_inner(id)));
        }

        Self {
            structs: self
                .structs
                .into_iter()
                .map(|(n, si)| (n, si.change_crate_to(id)))
                .collect(),
            enums: self
                .enums
                .into_iter()
                .map(|(n, si)| (n, si.change_crate_to(id)))
                .collect(),
            traits: HashMap::new(),
            methods: self
                .methods
                .into_iter()
                .map(|(n, hm)| (n, change_hashmap_crate_to(hm, id)))
                .collect(),
            functions: self
                .functions
                .into_iter()
                .map(|(n, si)| (n, si.change_crate_to(id)))
                .collect(),
            submodules,
            sizes: self.sizes,
            constants: HashMap::new(),
        }
    }

    pub fn extract(mut self, id: &str) -> Self {
        let sub = self.submodules.remove("crate").unwrap().1;
        if id != "crate" {
            sub.extract_inner(id)
        } else {
            sub
        }
    }

    fn get_struct_inner(
        &self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
    ) -> Option<(bool, &StructInfo)> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.structs.get(name).map(|el| (el.get_pub(), el)),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((b, sb)) => sb
                        .get_struct_inner(path, pos + 1)
                        .map(|(b2, i)| (b2 && *b, i)),
                },
                _ => todo!(),
            }
        }
    }

    pub fn get_struct(&self, path: &PathUL<()>) -> Option<(bool, &StructInfo)> {
        self.get_struct_inner(path.get_content(), 0)
    }

    fn get_enum_inner(
        &self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
        take_last: bool,
    ) -> Option<(bool, &EnumInfo)> {
        if !take_last && pos == path.len() - 2 {
            match (&path[pos], &path[pos + 1]) {
                (NamePath::Name(name), NamePath::Name(constructor)) => match self.enums.get(name) {
                    Some(enum_info) if enum_info.get_constructor(constructor).is_some() => {
                        Some((enum_info.get_pub(), enum_info))
                    }
                    _ => None,
                },
                _ => None,
            }
        } else if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.enums.get(name).map(|el| (el.get_pub(), el)),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((b, sb)) => sb
                        .get_enum_inner(path, pos + 1, take_last)
                        .map(|(b2, i)| (b2 && *b, i)),
                },
                _ => todo!(),
            }
        }
    }

    pub fn get_enum(&self, path: &PathUL<()>, take_last: bool) -> Option<(bool, &EnumInfo)> {
        self.get_enum_inner(path.get_content(), 0, take_last)
    }

    fn get_trait_inner(
        &self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
    ) -> Option<(bool, &TraitInfo)> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.traits.get(name).map(|fun_info| (true, fun_info)),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((b1, sb)) => sb
                        .get_trait_inner(path, pos + 1)
                        .map(|(b2, fun_info)| (b2 && *b1, fun_info)),
                },
                _ => todo!(),
            }
        }
    }

    pub fn get_trait(&self, path: &PathUL<()>) -> Option<(bool, &TraitInfo)> {
        self.get_trait_inner(path.get_content(), 0)
    }

    fn get_mut_trait_inner(
        &mut self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
    ) -> Option<(bool, &mut TraitInfo)> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.traits.get_mut(name).map(|fun_info| (true, fun_info)),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get_mut(name) {
                    None => None,
                    Some((b1, sb)) => sb
                        .get_mut_trait_inner(path, pos + 1)
                        .map(|(b2, fun_info)| (b2 && *b1, fun_info)),
                },
                _ => todo!(),
            }
        }
    }

    pub fn get_mut_trait(&mut self, path: &PathUL<()>) -> Option<(bool, &mut TraitInfo)> {
        self.get_mut_trait_inner(path.get_content(), 0)
    }

    pub fn impl_trait(&mut self, path: &PathUL<()>, typ: PostType) -> Option<Option<usize>> {
        self.get_mut_trait(path)
            .map(|(_, trait_info)| trait_info.impl_for(typ))
    }

    fn get_fun_inner(
        &self,
        path: &Vec<NamePath<(), Ident>>,
        pos: usize,
    ) -> Option<(bool, &FunInfo)> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self
                    .functions
                    .get(name.get_content())
                    .map(|fun_info| (true, fun_info)),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name.get_content()) {
                    None => None,
                    Some((b1, sb)) => sb
                        .get_fun_inner(path, pos + 1)
                        .map(|(b2, fun_info)| (b2 && *b1, fun_info)),
                },
                _ => todo!(),
            }
        }
    }

    pub fn get_fun(&self, path: &Path<()>) -> Option<(bool, &FunInfo)> {
        self.get_fun_inner(path.get_content(), 0)
    }

    fn get_fun_pathul_inner(
        &self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
    ) -> Option<(bool, &FunInfo)> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.functions.get(name).map(|fun_info| (true, fun_info)),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((b1, sb)) => sb
                        .get_fun_pathul_inner(path, pos + 1)
                        .map(|(b2, fun_info)| (b2 && *b1, fun_info)),
                },
                _ => todo!(),
            }
        }
    }

    pub fn get_fun_pathul(&self, path: &PathUL<()>) -> Option<(bool, &FunInfo)> {
        self.get_fun_pathul_inner(path.get_content(), 0)
    }

    fn get_mut_fun_inner(
        &mut self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
    ) -> Option<&mut FunInfo> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.functions.get_mut(name),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get_mut(name) {
                    None => None,
                    Some((_, sb)) => sb.get_mut_fun_inner(path, pos + 1),
                },
                _ => todo!(),
            }
        }
    }

    pub fn get_mut_fun(&mut self, path: &PathUL<()>) -> Option<&mut FunInfo> {
        self.get_mut_fun_inner(path.get_content(), 0)
    }

    fn get_const_inner(
        &self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
    ) -> Option<(bool, &Const)> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.constants.get(name).map(|(b, c)| (*b, c)),
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((b1, sb)) => sb
                        .get_const_inner(path, pos + 1)
                        .map(|(b2, c)| (b2 && *b1, c)),
                },
                _ => None,
            }
        }
    }

    pub fn get_const(&self, path: &PathUL<()>) -> Option<(bool, &Const)> {
        self.get_const_inner(path.get_content(), 0)
    }

    fn get_methods_inner(
        &self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
    ) -> Option<&HashMap<String, PathUL<()>>> {
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
                _ => None,
            }
        }
    }

    pub fn get_methods(&self, path: &PathUL<()>) -> Option<&HashMap<String, PathUL<()>>> {
        self.get_methods_inner(path.get_content(), 0)
    }

    fn get_size_inner(&self, path: &Vec<NamePath<(), String>>, pos: usize) -> Option<&usize> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.sizes.get(name),
                NamePath::Specialisation(_) => todo!(),
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((_, sb)) => sb.get_size_inner(path, pos + 1),
                },
                _ => todo!(),
            }
        }
    }

    pub fn get_size(&self, path: &PathUL<(), String>) -> Option<&usize> {
        self.get_size_inner(path.get_content(), 0)
    }

    fn get_module_inner(
        &self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
    ) -> Option<(bool, &Self)> {
        if pos == path.len() {
            Some((true, self))
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get(name) {
                    None => None,
                    Some((b1, sb)) => sb
                        .get_module_inner(path, pos + 1)
                        .map(|(b2, i)| (b2 && *b1, i)),
                },
                _ => None,
            }
        }
    }

    fn get_module(&self, path: &PathUL<()>) -> Option<(bool, &Self)> {
        self.get_module_inner(path.get_content(), 0)
    }

    fn get_mut_module_inner(
        &mut self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
    ) -> Option<(bool, &mut Self)> {
        if pos == path.len() {
            Some((true, self))
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get_mut(name) {
                    None => None,
                    Some((b1, sb)) => sb
                        .get_mut_module_inner(path, pos + 1)
                        .map(|(b2, i)| (b2 && *b1, i)),
                },
                _ => None,
            }
        }
    }

    fn get_mut_module(&mut self, path: &PathUL<()>) -> Option<(bool, &mut Self)> {
        self.get_mut_module_inner(path.get_content(), 0)
    }

    pub fn insert_top_size(&mut self, name: String, size: usize) -> Option<usize> {
        self.sizes.insert(name, size)
    }

    fn insert_size_inner(
        &mut self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
        size: usize,
    ) -> Option<usize> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => self.sizes.insert(name.to_string(), size),
                NamePath::Specialisation(_) => todo!(),
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get_mut(name) {
                    None => todo!(),
                    Some((_, sb)) => sb.insert_size_inner(path, pos + 1, size),
                },
                _ => todo!(),
            }
        }
    }

    pub fn insert_size(&mut self, path: PathUL<(), String>, size: usize) -> Option<usize> {
        self.insert_size_inner(path.get_content(), 0, size)
    }

    fn insert_struct_inner(
        &mut self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
        info: StructInfo,
    ) -> Option<StructInfo> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => {
                    if !self.submodules.contains_key(name) {
                        self.submodules
                            .insert(name.to_string(), (true, Self::empty()));
                    }
                    self.structs.insert(name.to_string(), info)
                }
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get_mut(name) {
                    None => None,
                    Some((_, sb)) => sb.insert_struct_inner(path, pos + 1, info),
                },
                _ => None,
            }
        }
    }

    pub fn insert_struct(
        &mut self,
        path: PathUL<(), String>,
        size: usize,
        is_pub: bool,
        args: HashMap<String, PostType>,
    ) -> Option<StructInfo> {
        self.insert_struct_inner(path.get_content(), 0, StructInfo::new(size, is_pub, args))
    }

    fn insert_enum_inner(
        &mut self,
        path: &Vec<NamePath<(), String>>,
        pos: usize,
        info: EnumInfo,
    ) -> Option<EnumInfo> {
        if pos == path.len() - 1 {
            match &path[pos] {
                NamePath::Name(name) => {
                    if !self.submodules.contains_key(name) {
                        self.submodules
                            .insert(name.to_string(), (true, Self::empty()));
                    }
                    self.enums.insert(name.to_string(), info)
                }
                NamePath::Specialisation(_) => None,
            }
        } else {
            match &path[pos] {
                NamePath::Name(name) => match self.submodules.get_mut(name) {
                    None => None,
                    Some((_, sb)) => sb.insert_enum_inner(path, pos + 1, info),
                },
                _ => None,
            }
        }
    }

    pub fn insert_enum(
        &mut self,
        path: PathUL<(), String>,
        size: usize,
        is_pub: bool,
        rows: HashMap<String, (usize, Vec<PostType>)>,
    ) -> Option<EnumInfo> {
        self.insert_enum_inner(path.get_content(), 0, EnumInfo::new(size, is_pub, rows))
    }

    pub fn new_trait(&mut self, name: String, trait_info: TraitInfo) -> Option<TraitInfo> {
        self.traits.insert(name, trait_info)
    }

    pub fn impl_method(
        &mut self,
        struct_name: &str,
        method: String,
        fun_name: PathUL<()>,
    ) -> Option<PathUL<()>> {
        let map = match self.methods.get_mut(struct_name) {
            None => {
                let map = HashMap::new();
                self.methods.insert(struct_name.to_string(), map);
                self.methods.get_mut(struct_name).unwrap()
            }
            Some(map) => map,
        };
        map.insert(method, fun_name)
    }

    pub fn impl_fun(
        &mut self,
        fun_name: String,
        public: bool,
        free_types: Vec<(String, Vec<PathUL<()>>)>,
        args_types: Vec<PostType>,
        out_type: PostType,
    ) -> Option<FunInfo> {
        self.functions.insert(
            fun_name,
            FunInfo::new(public, free_types, args_types, out_type),
        )
    }

    pub fn insert(
        &mut self,
        name: String,
        public: bool,
        submod: ModuleInterface,
    ) -> Option<(bool, ModuleInterface)> {
        self.submodules.insert(name, (public, submod))
    }
}

#[derive(Debug)]
pub struct GlobalContext {
    path: PathUL<()>,
    modules: ModuleInterface,
}

impl GlobalContext {
    pub fn new(path: PathUL<()>, modules: ModuleInterface) -> Self {
        Self { path, modules }
    }

    pub fn extract_module(self) -> ModuleInterface {
        self.modules
    }

    pub fn has_trait(&self, trait_name: &PathUL<()>, typ: &PostType) -> Option<bool> {
        match self.modules.get_trait(trait_name) {
            Some((_, trait_info)) => Some(trait_info.implemented_for(typ)),
            None => None,
        }
    }

    pub fn get_trait(&self, trait_name: &PathUL<()>) -> Option<&TraitInfo> {
        match self.modules.get_trait(trait_name) {
            Some((_, trait_info)) => Some(trait_info),
            None => None,
        }
    }

    pub fn impl_trait_free(&mut self, trait_path: &PathUL<()>, name: String) -> Option<bool> {
        match self.modules.get_mut_trait(trait_path) {
            Some((_, trait_info)) => Some(trait_info.impl_for_free(name)),
            None => None,
        }
    }

    pub fn clean_trait_free(&mut self, trait_path: &PathUL<()>) -> Option<()> {
        match self.modules.get_mut_trait(trait_path) {
            Some((_, trait_info)) => Some(trait_info.clean_free_impl()),
            None => None,
        }
    }

    pub fn get_path(&self, name: &str) -> PathUL<()> {
        let mut path = self.path.clone();
        path.push(NamePath::Name(name.to_string()));
        path
    }

    pub fn impl_fun(
        &mut self,
        fun_name: String,
        public: bool,
        free_types: Vec<(String, Vec<PathUL<()>>)>,
        args_types: Vec<PostType>,
        out_type: PostType,
    ) -> Option<FunInfo> {
        self.modules
            .get_mut_module(&self.path)?
            .1
            .impl_fun(fun_name, public, free_types, args_types, out_type)
    }

    pub fn impl_fun_path(
        &mut self,
        fun_path: PathUL<()>,
        fun_name: String,
        public: bool,
        free_types: Vec<(String, Vec<PathUL<()>>)>,
        args_types: Vec<PostType>,
        out_type: PostType,
    ) -> Option<FunInfo> {
        self.modules
            .get_mut_module(&self.path)
            .unwrap()
            .1
            .get_mut_module(&fun_path)
            .unwrap()
            .1
            .impl_fun(fun_name, public, free_types, args_types, out_type)
    }

    pub fn update_dependancies(
        &mut self,
        path: &PathUL<()>,
        dependancies: HashMap<PathUL<()>, HashSet<Vec<PostType>>>,
    ) {
        self.modules
            .get_mut_fun(path)
            .unwrap()
            .set_dependancies(dependancies)
    }

    pub fn get_struct(&self, name: &str) -> Option<&StructInfo> {
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

    pub fn get_enum_name(&self, name: &str) -> Option<&EnumInfo> {
        self.modules.get_module(&self.path)?.1.enums.get(name)
    }

    pub fn get_enum(&self, path: &PathUL<()>) -> Option<&EnumInfo> {
        match self.modules.get_enum(path, true) {
            Some((_, ei)) => Some(ei),
            _ => None,
        }
    }

    pub fn is_constructor(&self, path: &PathUL<()>) -> Option<&EnumInfo> {
        self.modules.get_enum(path, false).map(|p| p.1)
    }

    pub fn enum_info(&self, name: &PathUL<()>) -> Option<EnumInfo> {
        self.get_enum(name).cloned()
    }

    pub fn get_method_function(
        &self,
        type_name: &PathUL<()>,
        method: &Ident,
    ) -> Option<PathUL<()>> {
        let path2 = self
            .modules
            .get_methods(type_name)?
            .get(method.get_content())?;
        let mut path = type_name.clone();
        path.pop();
        path.append(path2.clone());
        Some(path)
    }

    pub fn get_fun(&self, path: &PathUL<()>) -> Option<&FunInfo> {
        match self.modules.get_fun_pathul(path) {
            Some((_, fun_info)) => Some(fun_info),
            _ => None,
        }
    }

    pub fn get_fun_loc(&self, path: &Path<()>) -> Option<&FunInfo> {
        match self.modules.get_fun(path) {
            Some((_, fun_info)) => Some(fun_info),
            _ => None,
        }
    }

    pub fn get_top_fun(&self, name: &str) -> Option<&FunInfo> {
        self.modules.get_module(&self.path)?.1.functions.get(name)
    }

    pub fn get_const_val(&self, path: &PathUL<()>) -> Option<&Const> {
        match self.modules.get_const(path) {
            Some((_, c)) => Some(c),
            _ => None,
        }
    }

    pub fn get_top_const_val(&self, name: &str) -> Option<&Const> {
        self.modules
            .get_module(&self.path)?
            .1
            .constants
            .get(name)
            .map(|(_, f)| f)
    }

    pub fn add_const(
        &mut self,
        name: String,
        public: bool,
        typ: PostType,
        value: super::consts::Val,
    ) -> Option<Const> {
        let constant = Const::new(typ, value);
        self.modules
            .get_mut_module(&self.path)?
            .1
            .constants
            .insert(name, (public, constant))
            .map(|(_, c)| c)
    }

    pub fn impl_method(
        &mut self,
        struct_name: &str,
        method: String,
        fun_name: PathUL<()>,
    ) -> Option<PathUL<()>> {
        let (_, modint) = self.modules.get_mut_module(&self.path)?;
        let map = match modint.methods.get_mut(struct_name) {
            None => {
                let map = HashMap::new();
                modint.methods.insert(struct_name.to_string(), map);
                modint.methods.get_mut(struct_name).unwrap()
            }
            Some(map) => map,
        };
        map.insert(method, fun_name)
    }
}

#[derive(Clone, Debug)]
pub struct LocalContext {
    vars: Vec<HashMap<String, (bool, PostType)>>,
    fun_specialisation: HashMap<PathUL<()>, HashSet<Vec<PostType>>>,
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
            fun_specialisation: HashMap::new(),
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

    pub fn insert_fun(&mut self, path: PathUL<()>, type_vec: Vec<PostType>) {
        match self.fun_specialisation.get_mut(&path) {
            None => {
                let mut set = HashSet::new();
                set.insert(type_vec);
                assert!(self.fun_specialisation.insert(path, set).is_none())
            }
            Some(set) => {
                set.insert(type_vec);
            }
        }
    }

    pub fn add_var(&mut self, ident: &Ident, mutable: bool, typ: &PostType) {
        if let Some(last) = self.vars.last_mut() {
            last.insert(ident.get_content().to_string(), (mutable, typ.clone()));
        } else {
            panic!("should never happend")
        }
    }

    pub fn add_var2(&mut self, ident: String, mutable: bool, typ: PostType) {
        if let Some(last) = self.vars.last_mut() {
            last.insert(ident, (mutable, typ));
        } else {
            panic!("should never happend")
        }
    }

    pub fn extract_fun(self) -> HashMap<PathUL<()>, HashSet<Vec<PostType>>> {
        self.fun_specialisation
    }
}
