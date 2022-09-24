use super::context::{GlobalContext, Trait};
use super::types::{compute_size, translate_typ};
use crate::ast::typed_rust::{PostType, PostTypeInner};
use crate::ast::{common::*, rust, typed_rust};
use std::collections::{HashMap, HashSet};

const DEFAULT_TYPES: [(&str, BuiltinType); 7] = [
    ("usize", BuiltinType::Int(false, Sizes::SUsize)),
    ("isize", BuiltinType::Int(true, Sizes::SUsize)),
    ("u64", BuiltinType::Int(false, Sizes::S64)),
    ("i64", BuiltinType::Int(true, Sizes::S64)),
    ("u32", BuiltinType::Int(false, Sizes::S32)),
    ("i32", BuiltinType::Int(true, Sizes::S32)),
    ("bool", BuiltinType::Bool),
];

const DEFAULT_TRAITS_ARITH: [&str; 5] = ["Add", "Div", "Sub", "Mul", "Mod"];

const DEFAULT_TRAITS_LOGIC: [&str; 2] = ["And", "Or"];

struct Graph {
    size: usize,
    names: HashMap<String, usize>,
    edges: Vec<HashSet<usize>>,
}

impl Graph {
    fn new() -> Self {
        Self {
            size: 0,
            names: HashMap::new(),
            edges: Vec::new(),
        }
    }

    fn add_node(&mut self, name: String) {
        if self.names.get(&name) == None {
            if self.names.insert(name, self.size).is_some() {
                todo!()
            };
            self.size += 1;
            self.edges.push(HashSet::new());
        } else {
            todo!() // multiple declarations for a single typ
        }
    }

    fn add_edge(&mut self, n1: &str, n2: &str) -> Option<bool> {
        let id1 = self.names.get(n1)?;
        let id2 = self.names.get(n2)?;
        Some(self.edges[*id1].insert(*id2))
    }
}

fn explore_dependensies(
    typ: &rust::PreType,
    parent: &str,
    graph: &mut Graph,
    set: &HashSet<String>,
) {
    match &typ.content {
        rust::PreTypeInner::Ident(id) => {
            if !set.contains(id.get_content()) {
                match graph.add_edge(parent, id.get_content()) {
                    None => todo!(),
                    Some(_) => (),
                }
            }
        }
        rust::PreTypeInner::IdentParametrized(id, _) if id.get_content() == "Vec" => (),
        rust::PreTypeInner::IdentParametrized(id, _) if id.get_content() == "Box" => (),
        rust::PreTypeInner::IdentParametrized(_, _) => todo!(),
        rust::PreTypeInner::Ref(_, _) => todo!(),
        rust::PreTypeInner::Tuple(v) => {
            for typ in v.iter() {
                explore_dependensies(typ, parent, graph, set)
            }
        }
        rust::PreTypeInner::Fun(_, _) => {}
    }
}

fn visit(
    id: usize,
    p_marks: &mut Vec<bool>,
    t_marks: &mut Vec<bool>,
    graph: &Graph,
    out_vec: &mut Vec<usize>,
) -> Result<(), usize> {
    if p_marks[id] {
        return Ok(());
    }
    if t_marks[id] {
        return Err(id);
    }
    t_marks[id] = true;

    for node in graph.edges[id].iter() {
        visit(*node, p_marks, t_marks, graph, out_vec)?;
    }
    t_marks[id] = false;
    p_marks[id] = true;
    out_vec.push(id);
    Ok(())
}

fn topological_sort(structs: Vec<rust::DeclStruct>, graph: &mut Graph) -> Vec<rust::DeclStruct> {
    let mut permanent_marks = vec![false; graph.size];
    let mut temporary_marks = vec![false; graph.size];
    let mut out_vec = Vec::new();
    for node in 0..graph.size {
        if !permanent_marks[node] {
            visit(
                node,
                &mut permanent_marks,
                &mut temporary_marks,
                graph,
                &mut out_vec,
            )
            .unwrap();
        }
    }

    let mut hash_map = HashMap::new();
    for (id, struct_decl) in structs.into_iter().enumerate() {
        hash_map.insert(id, struct_decl);
    }
    let mut structs = Vec::new();
    for id in out_vec {
        structs.push(hash_map.remove(&id).unwrap())
    }
    structs
}

pub fn compute_size_builtin(b: &BuiltinType) -> usize {
    match b {
        BuiltinType::Bool => 1,
        BuiltinType::Int(_, Sizes::S32) => 4,
        BuiltinType::Int(_, Sizes::S64) => 8,
        BuiltinType::Int(_, Sizes::SUsize) => 8, /* todo!() */
    }
}

pub fn type_structs(
    structs: Vec<rust::DeclStruct>,
) -> (GlobalContext, Vec<typed_rust::DeclStruct>) {
    let mut graph = Graph::new();
    for struct_decl in structs.iter() {
        graph.add_node(struct_decl.name.get_content().to_string());
    }

    let mut sizes = GlobalContext::new();
    let mut set = HashSet::new();
    for (name, raw_type) in DEFAULT_TYPES {
        sizes.insert_size(name.to_string(), compute_size_builtin(&raw_type));
        let typ = PostType {
            content: PostTypeInner::BuiltIn(raw_type),
        };

        if raw_type.is_bool() {
            for logic_trait in DEFAULT_TRAITS_LOGIC {
                let mut fun_name = name.to_string();
                fun_name.push_str("::");
                fun_name.push_str(logic_trait);
                fun_name.push('<');
                fun_name.push_str(name);
                fun_name.push('>');
                sizes.implement_trait(
                    &typ,
                    Trait::Parametrized(logic_trait.to_string(), Some(typ.clone())),
                    fun_name.clone(),
                );
                sizes.insert(
                    fun_name,
                    typed_rust::PostType {
                        content: typed_rust::PostTypeInner::Fun(
                            vec![],
                            vec![typ.clone(), typ.clone()],
                            Box::new(typ.clone()),
                        ),
                    },
                );
            }
            let mut fun_name = name.to_string();
            fun_name.push_str("::");
            fun_name.push_str("Not");
            sizes.implement_trait(&typ, Trait::Name("Not".to_string()), fun_name.clone());
            sizes.insert(
                fun_name,
                typed_rust::PostType {
                    content: typed_rust::PostTypeInner::Fun(
                        vec![],
                        vec![typ.clone()],
                        Box::new(typ.clone()),
                    ),
                },
            );
        }

        if raw_type.is_int() {
            for arith_trait in DEFAULT_TRAITS_ARITH {
                let mut fun_name = name.to_string();
                fun_name.push_str("::");
                fun_name.push_str(arith_trait);
                fun_name.push('<');
                fun_name.push_str(name);
                fun_name.push('>');
                sizes.implement_trait(
                    &typ,
                    Trait::Parametrized(arith_trait.to_string(), Some(typ.clone())),
                    fun_name.clone(),
                );
                sizes.insert(
                    fun_name,
                    typed_rust::PostType {
                        content: typed_rust::PostTypeInner::Fun(
                            vec![],
                            vec![typ.clone(), typ.clone()],
                            Box::new(typ.clone()),
                        ),
                    },
                );
            }
            let mut fun_name = name.to_string();
            fun_name.push_str("::");
            fun_name.push_str("PartialOrd");
            fun_name.push('<');
            fun_name.push_str(name);
            fun_name.push('>');
            sizes.implement_trait(
                &typ,
                Trait::Parametrized("PartialOrd".to_string(), Some(typ.clone())),
                fun_name.clone(),
            );
            for tail in ["_le", "_lo", "_gr", "_ge"] {
                let mut fun_name2 = fun_name.clone();
                fun_name2.push_str(tail);
//                println!("implementing {}", fun_name2);
                sizes.insert(
                    fun_name2,
                    typed_rust::PostType {
                        content: typed_rust::PostTypeInner::Fun(
                            vec![],
                            vec![typ.clone(), typ.clone()],
                            Box::new(typed_rust::PostType::bool()),
                        ),
                    },
                );
            }

            let mut fun_name = name.to_string();
            fun_name.push_str("::");
            fun_name.push_str("Neg");
            sizes.implement_trait(&typ, Trait::Name("Neg".to_string()), fun_name.clone());
            sizes.insert(
                fun_name,
                typed_rust::PostType {
                    content: typed_rust::PostTypeInner::Fun(
                        vec![],
                        vec![typ.clone()],
                        Box::new(typ.clone()),
                    ),
                },
            );
        };

        let mut fun_name = name.to_string();
        fun_name.push_str("::");
        fun_name.push_str("Copy");
        sizes.implement_trait(&typ, Trait::Name("Copy".to_string()), fun_name.clone());

        let mut fun_name = name.to_string();
        fun_name.push_str("::");
        fun_name.push_str("Clone");
        sizes.implement_trait(&typ, Trait::Name("Clone".to_string()), fun_name.clone());

        let mut fun_name = name.to_string();
        fun_name.push_str("::");
        fun_name.push_str("PartialEq");
        fun_name.push('<');
        fun_name.push_str(name);
        fun_name.push('>');
        sizes.implement_trait(
            &typ,
            Trait::Parametrized("PartialEq".to_string(), Some(typ.clone())),
            fun_name.clone(),
        );
        for tail in ["_eq", "_ne"] {
            let mut fun_name2 = fun_name.clone();
            fun_name2.push_str(tail);
//            println!("implementing {}", fun_name2);
            sizes.insert(
                fun_name2,
                typed_rust::PostType {
                    content: typed_rust::PostTypeInner::Fun(
                        vec![],
                        vec![typ.clone(), typ.clone()],
                        Box::new(typed_rust::PostType::bool()),
                    ),
                },
            );
        }

        sizes.insert(name.to_string(), typ);
        set.insert(name.to_string());
    }

    for struct_decl in structs.iter() {
        for arg in struct_decl.args.iter() {
            explore_dependensies(&arg.1, struct_decl.name.get_content(), &mut graph, &set)
        }
    }

    let structs = topological_sort(structs, &mut graph);
    let mut structs2 = Vec::new();

    for struct_decl in structs.iter() {
        let mut size = 0;
        for (_, typ) in struct_decl.args.iter() {
            size += compute_size(typ, &sizes);
        }
        assert!(sizes
            .insert(
                struct_decl.name.get_content().to_string(),
                typed_rust::PostType {
                    content: typed_rust::PostTypeInner::Struct(
                        struct_decl.name.get_content().to_string()
                    ),
                }
            )
            .is_none());
        assert!(sizes
            .insert_size(struct_decl.name.get_content().to_string(), size)
            .is_none());
    }

    for struct_decl in structs.into_iter() {
        let mut args = HashMap::new();
        for (name, typ) in struct_decl.args.into_iter() {
            let typ = translate_typ(typ, &sizes);
            match args.insert(name.content(), typ) {
                None => (),
                Some(_) => todo!(),
            }
        }
        sizes.add_struct(
            struct_decl.name.get_content().to_string(),
            typed_rust::PostType {
                content: typed_rust::PostTypeInner::Struct(
                    struct_decl.name.get_content().to_string(),
                ),
            },
            args.clone(),
        );
        structs2.push(typed_rust::DeclStruct {
            size: sizes.get_size(struct_decl.name.get_content()).unwrap(),
            name: struct_decl.name,
            args,
        })
    }

    (sizes, structs2)
}
