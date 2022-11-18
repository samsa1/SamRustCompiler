#![allow(dead_code)]
#![allow(unused)]

use crate::ast::common::*;
use crate::ast::rust::*;
use std::collections::{HashMap, HashSet};

struct LocalContext {
    variables: Vec<HashSet<String>>,
}

impl LocalContext {
    fn new() -> Self {
        Self {
            variables: Vec::new(),
        }
    }

    fn from_args(args: &Vec<(Ident, bool, PreType)>) -> Self {
        let mut set = HashSet::new();
        for (id, _, _) in args {
            set.insert(id.get_content().to_string());
        }

        Self {
            variables: vec![set],
        }
    }

    fn add_layer(&mut self) {
        self.variables.push(HashSet::new())
    }

    fn insert(&mut self, id: String) -> bool {
        let len = self.variables.len();
        self.variables[len - 1].insert(id)
    }

    fn pop_layer(&mut self) -> HashSet<String> {
        self.variables.pop().unwrap()
    }

    fn contains(&self, id: &str) -> bool {
        for layer in &self.variables {
            if layer.contains(id) {
                return true;
            }
        }
        false
    }
}

fn translate_path(mut path: Path<()>, map: &HashMap<String, Path<()>>) -> Path<()> {
    assert_ne!(path.get_content().len(), 0);
    match map.get(path.get_fst_id().unwrap()) {
        None => path,
        Some(path2) => {
            let mut path2 = path2.clone();
            path2.append_from(1, path);
            path2
        }
    }
}

fn translate_id(
    id: &Ident,
    map: &HashMap<String, Path<()>>,
    local_ctxt: &LocalContext,
) -> Option<Path<()>> {
    let path = map.get(id.get_content())?;
    if local_ctxt.contains(id.get_content()) {
        None
    } else {
        Some(path.clone())
    }
}

fn translate_type(typ: PreType, map: &HashMap<String, Path<()>>) -> PreType {
    let content = match typ.content {
        PreTypeInner::Fun(_, _) => todo!(),
        PreTypeInner::Ident(id) => match map.get(id.get_content()) {
            None => PreTypeInner::Ident(id),
            Some(path) => PreTypeInner::IdentPath(path.clone()),
        },
        PreTypeInner::IdentPath(path) => PreTypeInner::IdentPath(translate_path(path, map)),
        PreTypeInner::IdentParametrized(id, types) => {
            let types = types
                .into_iter()
                .map(|typ| translate_type(typ, map))
                .collect();
            match map.get(id.get_content()) {
                None => PreTypeInner::IdentParametrized(id, types),
                Some(path) => PreTypeInner::IdentParametrizedPath(path.clone(), types),
            }
        }
        PreTypeInner::IdentParametrizedPath(path, types) => {
            let types = types
                .into_iter()
                .map(|typ| translate_type(typ, map))
                .collect();
            PreTypeInner::IdentParametrizedPath(translate_path(path, map), types)
        }
        PreTypeInner::Ref(b, typ) => PreTypeInner::Ref(b, Box::new(translate_type(*typ, map))),
        PreTypeInner::Tuple(types) => PreTypeInner::Tuple(
            types
                .into_iter()
                .map(|typ| translate_type(typ, map))
                .collect(),
        ),
    };
    PreType { content }
}

fn rewrite_patt(
    patt: Pattern,
    local_ctxt: &mut LocalContext,
    map: &HashMap<String, Path<()>>,
) -> Pattern {
    local_ctxt.add_layer();
    for (_, id) in patt.get_arguments() {
        if map.contains_key(id.get_content()) {
            local_ctxt.insert(id.get_content().to_string());
        }
    }
    let bloc = rewrite_bloc(patt.bloc, local_ctxt, map);
    let guard = patt.guard.map(|expr| rewrite_expr(expr, local_ctxt, map));
    local_ctxt.pop_layer();

    Pattern {
        bloc,
        guard,
        ..patt
    }
}

fn rewrite_expr(
    expr: Expr,
    local_ctxt: &mut LocalContext,
    map: &HashMap<String, Path<()>>,
) -> Expr {
    let content = match *expr.content {
        ExprInner::Array(exprs) => ExprInner::Array(
            exprs
                .into_iter()
                .map(|expr| rewrite_expr(expr, local_ctxt, map))
                .collect(),
        ),
        ExprInner::BinaryOp(op, expr1, expr2) => ExprInner::BinaryOp(
            op,
            rewrite_expr(expr1, local_ctxt, map),
            rewrite_expr(expr2, local_ctxt, map),
        ),
        ExprInner::Bloc(bloc) => ExprInner::Bloc(rewrite_bloc(bloc, local_ctxt, map)),
        ExprInner::Bool(_) | ExprInner::Int(_, _) | ExprInner::String(_) => *expr.content,
        ExprInner::BuildStruct(name, args) => {
            let args = args
                .into_iter()
                .map(|(id, expr)| (id, rewrite_expr(expr, local_ctxt, map)))
                .collect();
            println!("{:?} {:?}", name, translate_id(&name, map, local_ctxt));
            match translate_id(&name, map, local_ctxt) {
                None => ExprInner::BuildStruct(name, args),
                Some(path) => ExprInner::BuildStructPath(path, args),
            }
        }
        ExprInner::BuildStructPath(path, args) => {
            let args = args
                .into_iter()
                .map(|(id, expr)| (id, rewrite_expr(expr, local_ctxt, map)))
                .collect();
            ExprInner::BuildStructPath(translate_path(path, map), args)
        }
        ExprInner::Coercion(expr, typ) => ExprInner::Coercion(
            rewrite_expr(expr, local_ctxt, map),
            typ.map(|typ| translate_type(typ, map)),
        ),
        ExprInner::Constructor(path, exprs) => {
            let exprs = exprs
                .into_iter()
                .map(|expr| rewrite_expr(expr, local_ctxt, map))
                .collect();
            ExprInner::Constructor(translate_path(path, map), exprs)
        }
        ExprInner::Deref(expr) => ExprInner::Deref(rewrite_expr(expr, local_ctxt, map)),
        ExprInner::FunCall(free, name, exprs) => {
            let exprs = exprs
                .into_iter()
                .map(|expr| rewrite_expr(expr, local_ctxt, map))
                .collect();
            match translate_id(&name, map, local_ctxt) {
                None => ExprInner::FunCall(free, name, exprs),
                Some(path) => ExprInner::FunCallPath(free, path, exprs),
            }
        }
        ExprInner::FunCallPath(free, path, exprs) => {
            let exprs = exprs
                .into_iter()
                .map(|expr| rewrite_expr(expr, local_ctxt, map))
                .collect();
            println!("{:?} {:?}", path, translate_path(path.clone(), map));
            ExprInner::FunCallPath(free, translate_path(path, map), exprs)
        }
        ExprInner::If(expr, bloc1, bloc2) => ExprInner::If(
            rewrite_expr(expr, local_ctxt, map),
            rewrite_bloc(bloc1, local_ctxt, map),
            rewrite_bloc(bloc2, local_ctxt, map),
        ),
        ExprInner::Index(expr1, expr2) => ExprInner::Index(
            rewrite_expr(expr1, local_ctxt, map),
            rewrite_expr(expr2, local_ctxt, map),
        ),
        ExprInner::MacroCall(id, args) if id.get_content() == "print" => {
            ExprInner::MacroCall(id, args)
        }
        ExprInner::MacroCall(id, _) => panic!("ICE {:?}", id),
        ExprInner::Method(expr, name, exprs) => ExprInner::Method(
            rewrite_expr(expr, local_ctxt, map),
            name,
            exprs
                .into_iter()
                .map(|expr| rewrite_expr(expr, local_ctxt, map))
                .collect(),
        ),
        ExprInner::Parenthesis(expr) => ExprInner::Parenthesis(rewrite_expr(expr, local_ctxt, map)),
        ExprInner::PatternMatching(expr, rows, opt) => {
            let expr = rewrite_expr(expr, local_ctxt, map);
            let opt = opt.map(|(b, name, bloc)| (b, name, rewrite_bloc(bloc, local_ctxt, map)));
            let rows = rows
                .into_iter()
                .map(|patt| rewrite_patt(patt, local_ctxt, map))
                .collect();
            ExprInner::PatternMatching(expr, rows, opt)
        }
        ExprInner::Proj(expr, proj) => ExprInner::Proj(rewrite_expr(expr, local_ctxt, map), proj),
        ExprInner::Ref(b, expr) => ExprInner::Ref(b, rewrite_expr(expr, local_ctxt, map)),
        ExprInner::Return(opt) => {
            ExprInner::Return(opt.map(|expr| rewrite_expr(expr, local_ctxt, map)))
        }
        ExprInner::Tuple(exprs) => ExprInner::Tuple(
            exprs
                .into_iter()
                .map(|expr| rewrite_expr(expr, local_ctxt, map))
                .collect(),
        ),
        ExprInner::UnaryOp(op, expr) => ExprInner::UnaryOp(op, rewrite_expr(expr, local_ctxt, map)),
        ExprInner::Var(id) => match translate_id(&id, map, local_ctxt) {
            None => ExprInner::Var(id),
            Some(path) => ExprInner::VarPath(path),
        },
        ExprInner::VarPath(path) => ExprInner::VarPath(translate_path(path, map)),
        ExprInner::While(expr, bloc) => ExprInner::While(
            rewrite_expr(expr, local_ctxt, map),
            rewrite_bloc(bloc, local_ctxt, map),
        ),
    };
    let content = Box::new(content);
    let typed = expr.typed.map(|typ| translate_type(typ, map));
    Expr {
        content,
        typed,
        ..expr
    }
}

fn rewrite_bloc(
    bloc: Bloc,
    local_ctxt: &mut LocalContext,
    map: &HashMap<String, Path<()>>,
) -> Bloc {
    local_ctxt.add_layer();
    let mut content = Vec::new();
    for instr in bloc.content.into_iter() {
        let instr_content = match instr.content {
            InstrInner::Expr(d, e) => InstrInner::Expr(d, rewrite_expr(e, local_ctxt, map)),
            InstrInner::Binding(mutable, id, typ, expr) => {
                let typ = typ.map(|typ| translate_type(typ, map));
                let expr = rewrite_expr(expr, local_ctxt, map);
                if map.contains_key(id.get_content()) {
                    local_ctxt.insert(id.get_content().to_string());
                }
                InstrInner::Binding(mutable, id, typ, expr)
            }
        };
        content.push(Instr {
            content: instr_content,
            ..instr
        })
    }
    local_ctxt.pop_layer();
    Bloc { content, ..bloc }
}

fn rewrite_decl(decl: Decl, map: &mut HashMap<String, Path<()>>, path: &Path<()>) -> Decl {
    match decl {
        Decl::Fun(decl_fun) => Decl::Fun(DeclFun {
            content: rewrite_bloc(
                decl_fun.content,
                &mut LocalContext::from_args(&decl_fun.args),
                map,
            ),
            args: decl_fun
                .args
                .into_iter()
                .map(|(id, b, typ)| (id, b, translate_type(typ, map)))
                .collect(),
            output: translate_type(decl_fun.output, map),
            ..decl_fun
        }),
        Decl::Enum(decl_enum) => Decl::Enum(DeclEnum {
            args: decl_enum
                .args
                .into_iter()
                .map(|(id, types)| {
                    (
                        id,
                        types
                            .into_iter()
                            .map(|typ| translate_type(typ, map))
                            .collect(),
                    )
                })
                .collect(),
            ..decl_enum
        }),

        Decl::Struct(decl_struct) => Decl::Struct(DeclStruct {
            args: decl_struct
                .args
                .into_iter()
                .map(|(id, typ)| (id, translate_type(typ, map)))
                .collect(),
            ..decl_struct
        }),
        Decl::Impl(decl_impl) => {
            let out = map.remove("Self");
            let mut path = path.clone();
            path.push(NamePath::Name(decl_impl.name.clone()));
            assert!(map
                .insert("Self".to_string(), translate_path(path, map))
                .is_none());
            let mut content = Vec::new();
            for decl_fun in decl_impl.content {
                let mut local_ctxt = LocalContext::from_args(&decl_fun.args);
                if decl_fun.self_arg.is_some() {
                    local_ctxt.insert("self".to_string());
                }
                content.push(DeclFun {
                    content: rewrite_bloc(decl_fun.content, &mut local_ctxt, map),
                    args: decl_fun
                        .args
                        .into_iter()
                        .map(|(id, b, typ)| (id, b, translate_type(typ, map)))
                        .collect(),
                    output: translate_type(decl_fun.output, map),
                    ..decl_fun
                })
            }

            match out {
                Some(t) => assert!(map.insert("Self".to_string(), t).is_none()),
                None => assert!(map.remove("Self").is_some()),
            };
            Decl::Impl(DeclImpl {
                name: decl_impl.name,
                content,
            })
        }
        Decl::Const(decl_const) => Decl::Const(DeclConst {
            expr: rewrite_expr(decl_const.expr, &mut LocalContext::new(), map),
            typ: translate_type(decl_const.typ, map),
            ..decl_const
        }),
    }
}

fn contains(map: &HashMap<String, Path<()>>, path: &Path<()>) -> bool {
    match path.get_content().get(0) {
        Some(NamePath::Name(s)) => map.contains_key(s.get_content()),
        _ => panic!("ICE"),
    }
}

fn rewrite_file(file: File, path: &Path<()>) -> File {
    let mut map = HashMap::new();
    map.insert("Vec".to_string(), Path::from_vec(vec!["std", "vec", "Vec"]));

    for open_decl in file.dep.into_iter() {
        match open_decl {
            Open::Mod(_, module, None) => {
                let mut path = path.clone();
                let name = module.get_content().to_string();
                path.push(NamePath::Name(module));
                map.insert(name, path);
            }
            Open::Mod(_, module, Some(given_name)) => {
                let mut path = path.clone();
                path.push(NamePath::Name(module));
                map.insert(given_name.get_content().to_string(), path);
            }
            Open::Use(mut path, None) => {
                while contains(&map, &path) {
                    path = translate_path(path, &map)
                }
                map.insert(path.last().unwrap().get_content().to_string(), path);
            }
            Open::Use(path, Some(given_name)) => {
                map.insert(given_name.get_content().to_string(), path);
            }
        }
    }
    println!("{:?}", map);
    for decl in file.content.iter() {
        match decl {
            Decl::Enum(decl_enum) => {
                let mut path = path.clone();
                path.push(NamePath::Name(decl_enum.name.clone()));
                map.insert(decl_enum.name.get_content().to_string(), path);
            }
            Decl::Struct(decl_struct) => {
                let mut path = path.clone();
                path.push(NamePath::Name(decl_struct.name.clone()));
                map.insert(decl_struct.name.get_content().to_string(), path);
            }
            _ => (),
        }
    }
    let content = file
        .content
        .into_iter()
        .map(|d| rewrite_decl(d, &mut map, path))
        .collect();

    File {
        content,
        dep: Vec::new(),
        ..file
    }
}

pub fn rewrite(
    m: crate::frontend::Module<File>,
    path: &mut Path<()>,
) -> crate::frontend::Module<File> {
    let content = rewrite_file(m.content, path);
    let mut submodules = Vec::new();
    for (k, (b, m_inner)) in m.submodules.into_iter() {
        path.push(NamePath::Name(Ident::new(&k, Location::default())));
        submodules.push((k, (b, rewrite(m_inner, path))));
        path.pop();
    }
    crate::frontend::Module::build(content, submodules.into_iter().collect())
}
