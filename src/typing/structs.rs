use crate::ast::{rust, typed_rust, common::*};
use crate::ast::typed_rust::{PostType, PostTypeInner};
use std::collections::{HashMap, HashSet};
use super::types::translate_typ;
use super::context::GlobalContext;

const DEFAULT_TYPES : [(&'static str, BuiltinType); 7] = [
    ("usize",   BuiltinType::Int(false, Sizes::SUsize)),
    ("isize",   BuiltinType::Int(true,  Sizes::SUsize)),
    ("u64",     BuiltinType::Int(false, Sizes::S64)),
    ("i64",     BuiltinType::Int(true,  Sizes::S64)),
    ("u32",     BuiltinType::Int(false, Sizes::S32)),
    ("i32",     BuiltinType::Int(true,  Sizes::S32)),
    ("bool",    BuiltinType::Bool),
];

struct Graph {
    size : usize,
    names : HashMap<String, usize>,
    edges : Vec<HashSet<usize>>,
}

impl Graph {
    fn new() -> Self {
        Self {
            size : 0,
            names : HashMap::new(),
            edges : Vec::new(),
        }
    }

    fn add_node(&mut self, name : String) {
        if self.names.get(&name) == None {
//            println!("adding {:?} to {:?}", name, self.names);
            if let Some(_) = self.names.insert(name, self.size) {
                todo!()
            };
//            println!("added");
            self.size += 1;
            self.edges.push(HashSet::new());
        } else {
            todo!() // multiple declarations for a single typ
        }
    }

    fn add_edge(&mut self, n1 : &str, n2 : &str) -> Option<bool> {
        let id1 = self.names.get(n1)?;
        let id2 = self.names.get(n2)?;
        Some(self.edges[*id1].insert(*id2))
    
    }
}

fn explore_dependensies(typ: &rust::PreType, parent: &str, graph : &mut Graph, set : &HashSet<String>) {
    match &typ.content {
        rust::PreTypeInner::Ident(id) => {
            if !set.contains(id.get_content()) {
                match graph.add_edge(parent, id.get_content()) {
                    None => todo!(),
                    Some(_) => (),
                }
            }},
        rust::PreTypeInner::IdentParametrized(_, _) => todo!(),
        rust::PreTypeInner::Ref(_) => todo!(),
        rust::PreTypeInner::Tuple(v) => {
            for typ in v.iter() {
                explore_dependensies(typ, parent, graph, set)
            }
        },
        rust::PreTypeInner::Fun(_, _) => {},
    }
}

fn visit(id : usize, p_marks : &mut Vec<bool>, t_marks : &mut Vec<bool>, graph: &Graph, out_vec : &mut Vec<usize>) -> Result<(), usize> {
    if p_marks[id] { return Ok(()) }
    if t_marks[id] { return Err(id) }
    t_marks[id] = true;
    
    for node in graph.edges[id].iter() {
        visit(*node, p_marks, t_marks, graph, out_vec)?;
    }
    t_marks[id] = false;
    p_marks[id] = true;
    out_vec.push(id);
    Ok(())
}

fn topological_sort(structs : Vec<rust::DeclStruct>, graph : &mut Graph) -> Vec<rust::DeclStruct> {
    let mut permanent_marks = vec![false; graph.size];
    let mut temporary_marks = vec![false; graph.size];
    let mut out_vec = Vec::new();
    for node in 0..graph.size {
        if !permanent_marks[node] {
            visit(node, &mut permanent_marks, &mut temporary_marks, graph, &mut out_vec).unwrap();
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

pub fn compute_size_builtin(b : &BuiltinType) -> usize {
    match b {
        BuiltinType::Bool => 1,
        BuiltinType::Int(_, Sizes::S32) => 4,
        BuiltinType::Int(_, Sizes::S64) => 8,
        BuiltinType::Int(_, Sizes::SUsize) => 8 /* todo!() */,
    }
}

pub fn type_structs(structs : Vec<rust::DeclStruct>) -> (GlobalContext, Vec<typed_rust::DeclStruct>) {
    let mut graph = Graph::new();
    for struct_decl in structs.iter() {
        graph.add_node(struct_decl.name.get_content().to_string());
    }

    let mut sizes = GlobalContext::new();
    let mut set = HashSet::new();
    for (name, typ) in DEFAULT_TYPES {
        let typ = PostType {
            size : compute_size_builtin(&typ),
            content : PostTypeInner::BuiltIn(typ),
            mutable : true,
        };
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

    for struct_decl in structs.into_iter() {
        let mut args = HashMap::new();
        let mut size = 0;
        for (name, typ) in struct_decl.args.into_iter() {
            let typ = translate_typ(typ, &sizes);
            size += typ.size;
            match args.insert(name.content(), typ) {
                None => (),
                Some(_) => todo!(),
            }
        };
        sizes.add_struct(
            struct_decl.name.get_content().to_string(),
            typed_rust::PostType {
                content : typed_rust::PostTypeInner::Struct(struct_decl.name.get_content().to_string()),
                mutable : false,
                size,
            },
            args.clone(),
        );
        structs2.push(typed_rust::DeclStruct {
            name : struct_decl.name,
            args,
            size,
        })
    }

    (sizes, structs2)
}