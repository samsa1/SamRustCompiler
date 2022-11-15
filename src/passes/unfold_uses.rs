#![allow(dead_code)]
#![allow(unused)]

use crate::ast::common::*;
use crate::ast::rust::*;
use std::collections::{HashMap, HashSet};

struct LocalContext {
    variables : Vec<HashSet<String>>,
}

impl LocalContext {
    fn new() -> Self {
        Self {
            variables : Vec::new()
        }
    }

    fn from_args(args : &Vec<(Ident, bool, PreType)>) -> Self {
        let mut set = HashSet::new();
        for (id, _, _) in args {
            set.insert(id.get_content().to_string());
        }

        Self {
            variables : vec![set]
        }
    }

    fn add_layer(&mut self) {
        self.variables.push(HashSet::new())
    } 

    fn insert(&mut self, id : String) -> bool {
        let len = self.variables.len();
        self.variables[len - 1].insert(id)
    }

    fn pop_layer(&mut self) -> HashSet<String> {
        self.variables.pop().unwrap()
    } 

    fn contains(&self, id : &str) -> bool {
        for layer in &self.variables {
            if layer.contains(id) {
                return true
            }
        }
        false
    }
}

fn translate_path(path : Path<()>, map : &HashMap<String, Path<()>>) -> Path<()> {
    assert_ne!(path.get_content().len(), 0);
    match map.get(path.get_fst_id().unwrap()) {
        None => path,
        Some(path2) => {
            let mut path2 = path2.clone();
            path2.append(path);
            path2
        }
    }
}

fn translate_id(id : &Ident, map : &HashMap<String, Path<()>>, local_ctxt : &LocalContext) -> Option<Path<()>> {
    let path = map.get(id.get_content())?;
    if local_ctxt.contains(id.get_content()) {
        None
    } else {
        Some(path.clone())
    }
}


fn translate_type(typ : PreType, map : &HashMap<String, Path<()>>) -> PreType {
    let content = match typ.content {
        PreTypeInner::Fun(_, _) => todo!(),
        PreTypeInner::Ident(id) => {
            match map.get(id.get_content()) {
                None => PreTypeInner::Ident(id),
                Some(path) => PreTypeInner::IdentPath(path.clone())
            }
        },
        PreTypeInner::IdentPath(path) =>
            PreTypeInner::IdentPath(translate_path(path, map)),
        PreTypeInner::IdentParametrized(id, types) => {
            let types = types.into_iter().map(|typ| translate_type(typ, map)).collect();
            match map.get(id.get_content()) {
                None => PreTypeInner::IdentParametrized(id, types),
                Some(path) => PreTypeInner::IdentParametrizedPath(path.clone(), types)
            }
        },
        PreTypeInner::IdentParametrizedPath(path, types) => {
            let types = types.into_iter().map(|typ| translate_type(typ, map)).collect();
            PreTypeInner::IdentParametrizedPath(translate_path(path, map), types)
        },
        PreTypeInner::Ref(b, typ) => 
            PreTypeInner::Ref(b, Box::new(translate_type(*typ, map))),
        PreTypeInner::Tuple(types) =>
            PreTypeInner::Tuple(types.into_iter().map(|typ| translate_type(typ, map)).collect()),
    };
    PreType {
        content,
    }
}

fn rewrite_expr(expr : Expr, local_ctxt : &mut LocalContext, map : &HashMap<String, Path<()>>) -> Expr {
    let content = match *expr.content {
        ExprInner::Array(exprs) =>
            ExprInner::Array(exprs.into_iter().map(|expr| rewrite_expr(expr, local_ctxt, map)).collect()),
        ExprInner::BinaryOp(op, expr1, expr2) => 
            ExprInner::BinaryOp(op, rewrite_expr(expr1, local_ctxt, map), rewrite_expr(expr2, local_ctxt, map)),
        ExprInner::Bloc(bloc) => ExprInner::Bloc(rewrite_bloc(bloc, local_ctxt, map)),
        ExprInner::Bool(_) | ExprInner::Int(_, _) | ExprInner::String(_) => *expr.content,
        ExprInner::BuildStruct(name, args) => {
            let args = args.into_iter().map(|(id, expr)| (id, rewrite_expr(expr, local_ctxt, map))).collect();
            match translate_id(&name, map, local_ctxt) {
                None => ExprInner::BuildStruct(name, args),
                Some(path) => ExprInner::BuildStructPath(path, args),
            }
        },
        ExprInner::BuildStructPath(path, args) => {
            let args = args.into_iter().map(|(id, expr)| (id, rewrite_expr(expr, local_ctxt, map))).collect();
            ExprInner::BuildStructPath(translate_path(path, map), args)
        },
        ExprInner::Coercion(expr, typ) => {
            ExprInner::Coercion(rewrite_expr(expr, local_ctxt, map), typ.map(|typ| translate_type(typ, map)))
        },
        ExprInner::Deref(expr) => ExprInner::Deref(rewrite_expr(expr, local_ctxt, map)),
        ExprInner::FunCall(free, name, exprs) => {
            let exprs = exprs.into_iter().map(|expr| rewrite_expr(expr, local_ctxt, map)).collect();
            match translate_id(&name, map, local_ctxt) {
                None => ExprInner::FunCall(free, name, exprs),
                Some(path) => ExprInner::FunCallPath(free, path, exprs),
            }
        },
        ExprInner::FunCallPath(free, path, exprs) => {
            let exprs = exprs.into_iter().map(|expr| rewrite_expr(expr, local_ctxt, map)).collect();
            ExprInner::FunCallPath(free, translate_path(path, map), exprs)
        },
        ExprInner::If(expr, bloc1, bloc2) =>
            ExprInner::If(rewrite_expr(expr, local_ctxt, map), rewrite_bloc(bloc1, local_ctxt, map), rewrite_bloc(bloc2, local_ctxt, map)),
        ExprInner::Index(expr1, expr2) =>
            ExprInner::Index(rewrite_expr(expr1, local_ctxt, map), rewrite_expr(expr2, local_ctxt, map)),
        ExprInner::MacroCall(id, args) if id.get_content() == "print" =>
            ExprInner::MacroCall(id, args),
        ExprInner::MacroCall(_, _) => panic!("ICE"),
        ExprInner::Method(expr, name, exprs) => {
            ExprInner::Method(rewrite_expr(expr, local_ctxt, map), name,
                exprs.into_iter().map(|expr| rewrite_expr(expr, local_ctxt, map)).collect())
        },
        ExprInner::Parenthesis(expr) => ExprInner::Parenthesis(rewrite_expr(expr, local_ctxt, map)),
        ExprInner::Proj(expr, proj) => 
            ExprInner::Proj(rewrite_expr(expr, local_ctxt, map), proj),
        ExprInner::Ref(b, expr) =>
            ExprInner::Ref(b, rewrite_expr(expr, local_ctxt, map)),
        ExprInner::Return(opt) => 
            ExprInner::Return(opt.map(|expr| rewrite_expr(expr, local_ctxt, map))),
        ExprInner::Tuple(exprs) =>
            ExprInner::Tuple(exprs.into_iter().map(|expr| rewrite_expr(expr, local_ctxt, map)).collect()),
        ExprInner::UnaryOp(op, expr) =>
            ExprInner::UnaryOp(op, rewrite_expr(expr, local_ctxt, map)),
        ExprInner::Var(id) => match translate_id(&id, map, local_ctxt) {
            None => ExprInner::Var(id),
            Some(path) => ExprInner::VarPath(path),
        },
        ExprInner::VarPath(path) => ExprInner::VarPath(translate_path(path, map)),
        ExprInner::While(expr, bloc) =>
            ExprInner::While(
                rewrite_expr(expr, local_ctxt, map), 
                rewrite_bloc(bloc, local_ctxt, map)),

    };
    let content = Box::new(content);
    let typed = expr.typed.map(|typ| translate_type(typ, map));
    Expr {
        content,
        typed,
        ..expr
    }
}

fn rewrite_bloc(bloc : Bloc, local_ctxt : &mut LocalContext, map : &HashMap<String, Path<()>>) -> Bloc {
    local_ctxt.add_layer();
    let mut content = Vec::new();
    for instr in bloc.content.into_iter() {
        let instr_content = match instr.content {
            InstrInner::Expr(d, e) => {
                InstrInner::Expr(d, rewrite_expr(e, local_ctxt, map))
            },
            InstrInner::Binding(mutable, id, typ, expr) => {
                let typ = typ.map(|typ| translate_type(typ, map));
                let expr = rewrite_expr(expr, local_ctxt, map);
                if map.contains_key(id.get_content()) {
                    local_ctxt.insert(id.get_content().to_string());
                }
                InstrInner::Binding(mutable, id, typ, expr)
            },
        };
        content.push(Instr { content: instr_content, ..instr })
    }
    local_ctxt.pop_layer();
    Bloc {
        content,
        ..bloc
    }
}

fn rewrite_decl(decl: Decl, map: &mut HashMap<String, Path<()>>) -> Decl {
    match decl {
        Decl::Fun(decl_fun) => {
            Decl::Fun(DeclFun {
                content : rewrite_bloc(decl_fun.content, &mut LocalContext::from_args(&decl_fun.args), map),
                args : decl_fun.args.into_iter().map(|(id, b, typ)| (id, b, translate_type(typ, map))).collect(),
                output : translate_type(decl_fun.output, map),
                ..decl_fun
            })

        },
        Decl::Struct(decl_struct) => {
            Decl::Struct(DeclStruct {
                args : decl_struct.args.into_iter().map(|(id, typ)| (id, translate_type(typ, map))).collect(),
                ..decl_struct
            })
        },
        Decl::Impl(decl_impl) => todo!(),
        Decl::Const(decl_const) => {
            Decl::Const(DeclConst {
                expr : rewrite_expr(decl_const.expr, &mut LocalContext::new(), map),
                typ : translate_type(decl_const.typ, map),
                ..decl_const
            })
        },
    }
}

pub fn rewrite_file(file: File) -> File {
    let mut map = HashMap::new();
    map.insert("Vec".to_string(), Path::from_vec(vec!["std", "vec", "Vec"]));

    for open_decl in file.dep.into_iter() {
        match open_decl {
            Open::Mod(_, module, None) => {
                map.insert(module.get_content().to_string(), module.to_path());
            }
            Open::Mod(_, module, Some(given_name)) => {
                map.insert(given_name.get_content().to_string(), module.to_path());
            }
            Open::Use(path, None) => {
                map.insert(path.last().unwrap().get_content().to_string(), path.clone());
            }
            Open::Use(path, Some(given_name)) => {
                map.insert(given_name.get_content().to_string(), path.clone());
            }
        }
    }

    let content = file
        .content
        .into_iter()
        .map(|d| rewrite_decl(d, &mut map))
        .collect();

    File {
        content,
        dep : Vec::new(),
        ..file
    }
}

pub fn rewrite(m : crate::frontend::Module<File>) -> crate::frontend::Module<File> {
    let content = rewrite_file(m.content);
    let submodules = m.submodules.into_iter().map(|(k, (b, m_inner))| (k, (b, rewrite(m_inner)))).collect();
    crate::frontend::Module::build(content, submodules)
}
