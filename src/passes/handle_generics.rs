use std::collections::HashMap;

use crate::ast::common::NamePath;
use crate::ast::typed_rust::*;
use crate::frontend::Module;
use crate::typing::types::substitute;
use crate::{ast::common::PathUL, typing::context::ModuleInterface};

fn rewrite_modint(mut modint: ModuleInterface) -> ModuleInterface {
    let mut to_handle = vec![
        (PathUL::from_vec(vec!["crate", "main"]), Vec::new()),
        (
            PathUL::from_vec(vec!["std", "allocator", "realloc"]),
            Vec::new(),
        ),
        (
            PathUL::from_vec(vec!["std", "allocator", "init"]),
            Vec::new(),
        ),
    ];

    for (path, _) in &to_handle {
        let info = modint.get_mut_fun(&path.add_loc()).unwrap();
        info.add_version(&Vec::new());
    }

    println!("start specialisation\n");
    while !to_handle.is_empty() {
        let (path, types) = to_handle.pop().unwrap();
        println!("{:?}", path);
        let infos = modint.get_fun(&path.add_loc()).unwrap().1;
        let mut hash_map = HashMap::new();
        assert_eq!(types.len(), infos.get_free().len());
        for (name, typ) in infos.get_free().iter().zip(types.into_iter()) {
            assert!(hash_map.insert(name.to_string(), typ).is_none())
        }
        for (path, set) in infos.get_dependancies().clone() {
            println!("- {:?}", path);
            for given_free in set {
                let given_free = given_free
                    .into_iter()
                    .map(|typ| substitute(typ, &hash_map))
                    .collect();
                println!("-- {:?} {:?}", path, given_free);
                let info = modint.get_mut_fun(&path.add_loc()).unwrap();
                if info.add_version(&given_free) {
                    to_handle.push((path.clone(), given_free))
                }
            }
        }
    }
    modint
}

pub fn new_path(path: &PathUL<()>, i: &usize) -> PathUL<()> {
    let mut path = path.clone();
    match path.pop() {
        Some(NamePath::Name(mut str)) => {
            str.push_str(&format!("_{}", i));
            path.push(NamePath::Name(str))
        }
        _ => panic!("ICE"),
    }
    path
}

fn rewrite_patt(
    patt: &Pattern,
    hashmap: &HashMap<String, &PostType>,
    modint: &ModuleInterface,
) -> Pattern {
    Pattern {
        constructor_id: patt.constructor_id,
        constructor: patt.constructor.clone(),
        arguments: patt
            .arguments
            .iter()
            .map(|(b, id, typ)| (*b, id.clone(), rewrite_type(typ, hashmap, modint)))
            .collect(),
        guard: patt
            .guard
            .as_ref()
            .map(|expr| rewrite_expr(expr, hashmap, modint)),
        bloc: rewrite_bloc(&patt.bloc, hashmap, modint),
    }
}

fn rewrite_expr(
    expr: &Expr,
    hashmap: &HashMap<String, &PostType>,
    modint: &ModuleInterface,
) -> Expr {
    let content = match &*expr.content {
        ExprInner::BinOp(op, expr1, expr2) => ExprInner::BinOp(
            *op,
            rewrite_expr(expr1, hashmap, modint),
            rewrite_expr(expr2, hashmap, modint),
        ),
        ExprInner::Bloc(bloc) => ExprInner::Bloc(rewrite_bloc(bloc, hashmap, modint)),
        ExprInner::Bool(b) => ExprInner::Bool(*b),
        ExprInner::BuildStruct(path, binds) => ExprInner::BuildStruct(
            path.clone(),
            binds
                .iter()
                .map(|(id, expr)| (id.clone(), rewrite_expr(expr, hashmap, modint)))
                .collect(),
        ),
        ExprInner::Coercion(expr, typ1, typ2) => {
            ExprInner::Coercion(rewrite_expr(expr, hashmap, modint), *typ1, *typ2)
        }
        ExprInner::Constructor(path, exprs) => ExprInner::Constructor(
            path.clone(),
            exprs
                .iter()
                .map(|expr| rewrite_expr(expr, hashmap, modint))
                .collect(),
        ),
        ExprInner::Deref(expr) => ExprInner::Deref(rewrite_expr(expr, hashmap, modint)),
        ExprInner::FunCall(name, exprs) => ExprInner::FunCall(
            name.clone(),
            exprs
                .iter()
                .map(|expr| rewrite_expr(expr, hashmap, modint))
                .collect(),
        ),
        ExprInner::FunCallPath(spec, path, exprs) => {
            let spec = spec
                .iter()
                .map(|typ| rewrite_type(typ, hashmap, modint))
                .collect();
            println!("{:?}", path);
            let i = modint.get_fun(&path.add_loc()).unwrap().1;
            println!("{:?} {:?}", i, spec);
            let i = i.get_id(&spec).unwrap();
            let path = new_path(path, i);
            ExprInner::FunCallPath(
                Vec::new(),
                path,
                exprs
                    .iter()
                    .map(|expr| rewrite_expr(expr, hashmap, modint))
                    .collect(),
            )
        }
        ExprInner::Int(i) => ExprInner::Int(*i),
        ExprInner::If(expr, bloc1, bloc2) => ExprInner::If(
            rewrite_expr(expr, hashmap, modint),
            rewrite_bloc(bloc1, hashmap, modint),
            rewrite_bloc(bloc2, hashmap, modint),
        ),
        ExprInner::PatternMatching(expr, patterns, fall) => ExprInner::PatternMatching(
            rewrite_expr(expr, hashmap, modint),
            patterns
                .iter()
                .map(|patt| rewrite_patt(patt, hashmap, modint))
                .collect(),
            fall.as_ref()
                .map(|(b, id, bloc)| (*b, id.clone(), rewrite_bloc(bloc, hashmap, modint))),
        ),
        ExprInner::Proj(expr, proj) => {
            ExprInner::Proj(rewrite_expr(expr, hashmap, modint), proj.clone())
        }
        ExprInner::Print(str) => ExprInner::Print(str.clone()),
        ExprInner::PrintPtr(expr) => ExprInner::PrintPtr(rewrite_expr(expr, hashmap, modint)),
        ExprInner::Ref(b, expr) => ExprInner::Ref(*b, rewrite_expr(expr, hashmap, modint)),
        ExprInner::Return(None) => ExprInner::Return(None),
        ExprInner::Return(Some(expr)) => {
            ExprInner::Return(Some(rewrite_expr(expr, hashmap, modint)))
        }
        ExprInner::Set(expr1, expr2) => ExprInner::Set(
            rewrite_expr(expr1, hashmap, modint),
            rewrite_expr(expr2, hashmap, modint),
        ),
        ExprInner::String(str) => ExprInner::String(str.clone()),
        ExprInner::Tuple(exprs, pad) => ExprInner::Tuple(
            exprs
                .iter()
                .map(|expr| rewrite_expr(expr, hashmap, modint))
                .collect(),
            *pad,
        ),
        ExprInner::UnaOp(op, expr) => ExprInner::UnaOp(*op, rewrite_expr(expr, hashmap, modint)),
        ExprInner::Var(name) => ExprInner::Var(name.clone()),
        ExprInner::VarPath(path) => match modint.get_fun(&path.add_loc()) {
            None => ExprInner::VarPath(path.clone()),
            Some((_, info)) => {
                let i = info.get_id(&Vec::new()).unwrap();
                ExprInner::VarPath(new_path(path, i))
            }
        },
        ExprInner::While(expr, bloc) => ExprInner::While(
            rewrite_expr(expr, hashmap, modint),
            rewrite_bloc(bloc, hashmap, modint),
        ),
    };
    Expr {
        content: Box::new(content),
        typed: rewrite_type(&expr.typed, hashmap, modint),
        loc: expr.loc,
    }
}

fn rewrite_bloc(
    bloc: &Bloc,
    hashmap: &HashMap<String, &PostType>,
    modint: &ModuleInterface,
) -> Bloc {
    let mut content = Vec::new();
    for instr in &bloc.content {
        match instr {
            Instr::Expr(keep, expr) => {
                content.push(Instr::Expr(*keep, rewrite_expr(expr, hashmap, modint)))
            }
            Instr::Binding(mutable, id, expr) => content.push(Instr::Binding(
                *mutable,
                id.clone(),
                rewrite_expr(expr, hashmap, modint),
            )),
        }
    }
    Bloc {
        content,
        last_type: rewrite_type(&bloc.last_type, hashmap, modint),
    }
}

fn rewrite_type(
    typ: &PostType,
    hashmap: &HashMap<String, &PostType>,
    modint: &ModuleInterface,
) -> PostType {
    let content = match &typ.content {
        PostTypeInner::BuiltIn(b) => PostTypeInner::BuiltIn(*b),
        PostTypeInner::Struct(path, types) => PostTypeInner::Struct(
            path.clone(),
            types
                .iter()
                .map(|typ| rewrite_type(typ, hashmap, modint))
                .collect(),
        ),
        PostTypeInner::Enum(path, types) => PostTypeInner::Enum(
            path.clone(),
            types
                .iter()
                .map(|typ| rewrite_type(typ, hashmap, modint))
                .collect(),
        ),
        PostTypeInner::Box(typ) => {
            PostTypeInner::Box(Box::new(rewrite_type(&**typ, hashmap, modint)))
        }
        PostTypeInner::Ref(b, typ) => {
            PostTypeInner::Ref(*b, Box::new(rewrite_type(&**typ, hashmap, modint)))
        }
        PostTypeInner::Tuple(types) => PostTypeInner::Tuple(
            types
                .iter()
                .map(|typ| rewrite_type(typ, hashmap, modint))
                .collect(),
        ),
        PostTypeInner::FreeType(t) => match hashmap.get(t) {
            None => panic!("ICE"),
            Some(t) => return (*t).clone(),
        },
        PostTypeInner::Fun(spec, args, out) if spec.is_empty() => PostTypeInner::Fun(
            Vec::new(),
            args.iter()
                .map(|typ| rewrite_type(typ, hashmap, modint))
                .collect(),
            Box::new(rewrite_type(&**out, hashmap, modint)),
        ),
        PostTypeInner::Fun(_, _, _) => panic!("ICE"),
        PostTypeInner::Diverge => PostTypeInner::Diverge,
        PostTypeInner::String => PostTypeInner::String,
    };
    PostType { content }
}

fn rewrite_code_inner(
    code: Module<File>,
    path: &mut PathUL<()>,
    modint: &ModuleInterface,
) -> Module<File> {
    let mut funs = Vec::new();
    for fun_decl in code.content.funs {
        let info = modint.get_fun(&fun_decl.name.add_loc()).unwrap().1;
        for (vec, id) in info.get_ids() {
            assert_eq!(vec.len(), fun_decl.free.len());
            let name = new_path(&fun_decl.name, id);
            let mut hashmap = HashMap::new();
            for (name, typ) in fun_decl.free.iter().zip(vec.iter()) {
                hashmap.insert(name.clone(), typ);
            }
            let content = rewrite_bloc(&fun_decl.content, &hashmap, modint);
            let output = rewrite_type(&fun_decl.output, &hashmap, modint);
            let args = fun_decl
                .args
                .iter()
                .map(|(id, b, typ)| (id.clone(), *b, rewrite_type(typ, &hashmap, modint)))
                .collect();
            funs.push(DeclFun {
                name,
                args,
                output,
                content,
                id_counter: fun_decl.id_counter.clone(),
                free: fun_decl.free.clone(),
            })
        }
    }
    let name = code.content.name;
    let content = File { name, funs };

    let mut submodules = HashMap::new();
    for (name, (b, sm)) in code.submodules.into_iter() {
        path.push(NamePath::Name(name.clone()));
        let sm = rewrite_code_inner(sm, path, modint);
        path.pop();
        submodules.insert(name, (b, sm));
    }
    Module::build(content, submodules)
}

fn rewrite_code(code: Module<File>, modint: &ModuleInterface) -> Module<File> {
    rewrite_code_inner(code, &mut PathUL::new(vec![]), modint)
}

pub fn rewrite(code: Module<File>, modint: ModuleInterface) -> (Module<File>, ModuleInterface) {
    let modint = rewrite_modint(modint);
    let code = rewrite_code(code, &modint);
    (code, modint)
}
