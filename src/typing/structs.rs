use super::context::{Trait, ModuleInterface, GlobalContext};
use super::types::{compute_size, translate_typ};
use crate::ast::rust::{Decl, DeclStruct};
use crate::ast::typed_rust::{PostType, PostTypeInner};
use crate::ast::{common::*, rust, typed_rust};
use crate::frontend::Module;
use std::collections::{HashMap, HashSet};

const DEFAULT_TYPES: [(&str, BuiltinType); 11] = [
    ("usize", BuiltinType::Int(false, Sizes::SUsize)),
    ("isize", BuiltinType::Int(true, Sizes::SUsize)),
    ("u64", BuiltinType::Int(false, Sizes::S64)),
    ("i64", BuiltinType::Int(true, Sizes::S64)),
    ("u32", BuiltinType::Int(false, Sizes::S32)),
    ("i32", BuiltinType::Int(true, Sizes::S32)),
    ("u16", BuiltinType::Int(false, Sizes::S16)),
    ("i16", BuiltinType::Int(true, Sizes::S16)),
    ("u8", BuiltinType::Int(false, Sizes::S8)),
    ("i8", BuiltinType::Int(true, Sizes::S8)),
    ("bool", BuiltinType::Bool),
];

const DEFAULT_TRAITS_ARITH: [(&str, &str); 9] = [
    ("Add", "add"), ("Div", "div"), ("Sub", "sub"), ("Mul", "mul"), ("Mod", "mod"),
    ("BitAnd", "bit_and"), ("BitOr", "bit_or"), ("Shl", "shl"), ("Shr", "shr"),
];

const DEFAULT_TRAITS_LOGIC: [(&str, &str); 2] = [("And", "and"), ("Or", "or")];

pub struct Graph {
    size: usize,
    names: HashMap<PathUL<(), String>, usize>,
    edges: Vec<HashSet<usize>>,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            size: 0,
            names: HashMap::new(),
            edges: Vec::new(),
        }
    }

    pub fn add_node(&mut self, name: PathUL<(), String>) {
        if self.names.get(&name) == None {
            assert!(self.names.insert(name, self.size).is_none());
            self.size += 1;
            self.edges.push(HashSet::new());
        } else {
            println!("Type {name:?} has been declared multiple times");
            std::process::exit(1)
        }
    }

    pub fn add_edge(&mut self, n1: &PathUL<(), String>, n2: &PathUL<(), String>) -> Option<bool> {
        let id1 = self.names.get(n1)?;
        let id2 = self.names.get(n2)?;
        Some(self.edges[*id1].insert(*id2))
    }
}

fn test_is_not(parent: &PathUL<(), String>, typ: &rust::PreType) {
    match &typ.content {
        rust::PreTypeInner::IdentParametrizedPath(_, _) | rust::PreTypeInner::IdentPath(_) => todo!(),
        rust::PreTypeInner::IdentParametrized(id, _) | rust::PreTypeInner::Ident(id)
            if parent.get_content().len() == 1 => {
            match parent.get_content().last().unwrap() {
                NamePath::Name(id2) if id.get_content() == id2 => {
                    println!("Structure {id2} depends on itself and thus is empty type");
                    std::process::exit(1)    
                },
                NamePath::Name(_) => (),
                _ => panic!("ICE"), 
            }
        }
        rust::PreTypeInner::Ident(_) => (),

        rust::PreTypeInner::Tuple(vec) | rust::PreTypeInner::IdentParametrized(_, vec) => {
            for typ in vec {
                test_is_not(parent, typ)
            }
        }

        rust::PreTypeInner::Ref(_, typ) => test_is_not(parent, typ),

        rust::PreTypeInner::Fun(_, _) => (),
    }
}

fn explore_dependensies(
    typ: &rust::PreType,
    parent: &PathUL<(), String>,
    graph: &mut Graph,
    set: &HashSet<String>,
) {
    match &typ.content {
        rust::PreTypeInner::Ident(id) => {
            if !set.contains(id.get_content()) {
                let mut id2 = parent.clone();
                id2.pop().unwrap();
                id2.push(NamePath::Name(id.get_content().to_string()));
                match graph.add_edge(parent, &id2) {
                    None => todo!(),
                    Some(_) => (),
                }
            }
        }
        rust::PreTypeInner::IdentPath(path) => {
            match graph.add_edge(parent, &path.cleaned()) {
                None => todo!(),
                Some(_) => (),
            }
        }
        rust::PreTypeInner::IdentParametrized(_, _) => todo!(),
        rust::PreTypeInner::IdentParametrizedPath(path, param)
            if path.is_vec() && param.len() == 1 => (),
        rust::PreTypeInner::IdentParametrizedPath(_, _) => todo!(),
        rust::PreTypeInner::Ref(_, sub_type) => {
            test_is_not(parent, sub_type);
            todo!()
        }
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

pub fn topological_sort<T>(structs: Vec<T>, graph: &Graph) -> Result<Vec<T>, (usize, Vec<T>)> {
    let mut permanent_marks = vec![false; graph.size];
    let mut temporary_marks = vec![false; graph.size];
    let mut out_vec = Vec::new();
    for node in 0..graph.size {
        if !permanent_marks[node] {
            if let Err(id) = visit(
                node,
                &mut permanent_marks,
                &mut temporary_marks,
                graph,
                &mut out_vec,
            ) {
                return Err((id, structs));
            }
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
    Ok(structs)
}

pub fn add_structs_graph(module : &mut Module<rust::File>, graph : &mut Graph, path : &mut PathUL<(), String>, structs : &mut Vec<(PathUL<(), String>, DeclStruct)>) {
    let mut local_structs = Vec::new();
    local_structs.append(&mut module.content.content);
    for decl in local_structs.into_iter() {
        match decl {
            Decl::Struct(ds) => {
                let mut path2 = path.clone();
                path2.push(NamePath::Name(ds.name.get_content().to_string()));
                structs.push((path2.clone(), ds));
                graph.add_node(path2);
            },
            decl => module.content.content.push(decl),
        }
    }
    for (name, (_, submod)) in module.submodules.iter_mut() {
        path.push(NamePath::Name(name.to_string()));
        add_structs_graph(submod, graph, path, structs);
        path.pop();
    }
}

pub fn type_structs(
    modules: &mut Module<rust::File>,
) -> (ModuleInterface, Vec<typed_rust::DeclStruct>) {
    let mut graph = Graph::new();
    let mut structs = Vec::new();
    let mut sizes = ModuleInterface::new(modules);
    add_structs_graph(modules, &mut graph, &mut PathUL::new(vec![NamePath::Name("crate".to_string())]), &mut structs);

    let mut set = HashSet::new();
    for (name, raw_type) in DEFAULT_TYPES {
        sizes.insert_top_size(name.to_string(), raw_type.to_byte_size());
        let typ = PostType {
            content: PostTypeInner::BuiltIn(raw_type),
        };
        let mut sub_modules = ModuleInterface::empty();

        if raw_type.is_bool() {
            for (logic_trait, logic_fun) in DEFAULT_TRAITS_LOGIC {
                // let fun_name = vec![
                //     NamePath::Name(name.to_string()),
                //     NamePath::Name(logic_trait.to_string()),
                //     ];
                // let fun_name = PathUL::new(fun_name);
                sizes.impl_trait(
                    &typ,
                    Trait::Parametrized(logic_trait.to_string(), None),
                );
                sub_modules.impl_fun(
                    logic_fun.to_string(),
                    true,
                    typed_rust::PostType {
                        content: typed_rust::PostTypeInner::Fun(
                            vec![],
                            vec![typ.clone(), typ.clone()],
                            Box::new(typ.clone()),
                        ),
                    },
                );
            }
            // let fun_name = vec![
            //     NamePath::Name(name.to_string()),
            //     NamePath::Name("Not".to_string()),
            //     ];
            // let fun_name = PathUL::new(fun_name);
            sizes.impl_trait(&typ, Trait::Name("Not".to_string()));
            sub_modules.impl_fun(
                "not".to_string(),
                true,
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
            for (arith_trait, arith_fun) in DEFAULT_TRAITS_ARITH {
                // let fun_name = vec![
                //     NamePath::Name(name.to_string()),
                //     NamePath::Name(arith_trait.to_string()),
                //     ];
                // let fun_name = PathUL::new(fun_name);
                sizes.impl_trait(
                    &typ,
                    Trait::Parametrized(arith_trait.to_string(), None),
                );
                sub_modules.impl_fun(
                    arith_fun.to_string(),
                    true,
                    typed_rust::PostType {
                        content: typed_rust::PostTypeInner::Fun(
                            vec![],
                            vec![typ.clone(), typ.clone()],
                            Box::new(typ.clone()),
                        ),
                    },
                );
            }
            // let fun_name = vec![
            //     NamePath::Name(name.to_string()),
            //     NamePath::Name("Not".to_string()),
            //     ];
            // let fun_name = PathUL::new(fun_name);
            sizes.impl_trait(
                &typ,
                Trait::Parametrized("PartialOrd".to_string(), None),
            );
            for tail in ["le", "lo", "gr", "ge"] {
                sub_modules.impl_fun(
                    tail.to_string(),
                    true,
                    typed_rust::PostType {
                        content: typed_rust::PostTypeInner::Fun(
                            vec![],
                            vec![typ.clone(), typ.clone()],
                            Box::new(typed_rust::PostType::bool()),
                        ),
                    },
                );
            }
            // let fun_name = vec![
            //     NamePath::Name(name.to_string()),
            //     NamePath::Name("Neg".to_string()),
            //     ];
            // let fun_name = PathUL::new(fun_name);
            sizes.impl_trait(&typ, Trait::Name("Neg".to_string()));
            sub_modules.impl_fun(
                "neg".to_string(),
                true,
                typed_rust::PostType {
                    content: typed_rust::PostTypeInner::Fun(
                        vec![],
                        vec![typ.clone()],
                        Box::new(typ.clone()),
                    ),
                },
            );
        };

        // let fun_name = vec![
        //     NamePath::Name(name.to_string()),
        //     NamePath::Name("Copy".to_string()),
        //     ];
        // let fun_name = PathUL::new(fun_name);
        sizes.impl_trait(&typ, Trait::Name("Copy".to_string()));

        // let fun_name = vec![
        //     NamePath::Name(name.to_string()),
        //     NamePath::Name("Clone".to_string()),
        //     ];
        // let fun_name = PathUL::new(fun_name);
        sizes.impl_trait(&typ, Trait::Name("Clone".to_string()));

        // let fun_name = vec![
        //     NamePath::Name(name.to_string()),
        //     NamePath::Name("PartialEq".to_string()),
        //     ];
        // let fun_name = PathUL::new(fun_name);
        sizes.impl_trait(
            &typ,
            Trait::Parametrized("PartialEq".to_string(), None),
        );
        for tail in ["eq", "ne"] {
            sub_modules.impl_fun(
                tail.to_string(),
                true,
                typed_rust::PostType {
                    content: typed_rust::PostTypeInner::Fun(
                        vec![],
                        vec![typ.clone(), typ.clone()],
                        Box::new(typed_rust::PostType::bool()),
                    ),
                },
            );
        }
        sizes.insert(name.to_string(), true, sub_modules);
        set.insert(name.to_string());
    }

    for struct_decl in structs.iter() {
        for arg in struct_decl.1.args.iter() {
            explore_dependensies(&arg.1, &struct_decl.0, &mut graph, &set)
        }
    }

    let mut structs = match topological_sort(structs, &mut graph) {
        Ok(s) => s,
        Err((_id, _structs)) => {
            println!("Do error message");
//            println!("{} has infinite size", structs[id].name.get_content());
            std::process::exit(1)
        }
    };
    let mut structs2 = Vec::new();

    println!("insert vec structure");
    sizes.insert_struct(PathUL::from_vec(vec!["std", "vec", "Vec"]), true, HashMap::new());

    for struct_decl in structs.iter_mut() {
        let mut size = 0;
        for (_, typ) in struct_decl.1.args.iter() {
            size += compute_size(typ, &sizes, &mut struct_decl.0);
        }
/*        assert!(sizes
            .insert(
                struct_decl.name.get_content().to_string(),
                typed_rust::PostType {
                    content: typed_rust::PostTypeInner::Struct(
                        struct_decl.name.get_content().to_string(),
                        vec![]
                    ),
                }
            )
            .is_none());*/
        assert!(sizes
            .insert_size(struct_decl.0.clone(), size).is_none());
    }

    for struct_decl in structs.into_iter() {
        assert!(sizes.insert_struct(
            struct_decl.0.clone(),
            struct_decl.1.public,
            HashMap::new(),
        ).is_none());
        let mut args = HashMap::new();
        for (name, typ) in struct_decl.1.args.into_iter() {
            let mut path = struct_decl.0.clone();
            path.pop();
            let ctxt = GlobalContext::new(path, sizes);
            let typ = translate_typ(typ, &ctxt);
            sizes = ctxt.extract_module();
            if args.contains_key(name.get_content()) {
                println!("Type {} was declared multiple times", name.content());
                std::process::exit(1)
            } else {
                match typ {
                    None => todo!(),
                    Some(typ) => assert!(args.insert(name.content(), typ).is_none()),
                }
            }
        }
        println!("{:?}", struct_decl.0);
        structs2.push(typed_rust::DeclStruct {
            size: *sizes.get_size(&struct_decl.0).unwrap(),
            name: struct_decl.1.name,
            args: args.clone(),
        });
        sizes.insert_struct(
            struct_decl.0,
            struct_decl.1.public,
            args,
        ).unwrap();
    }

    (sizes, structs2)
}
