use super::context::GlobalContext;
use super::structs::Graph;
use crate::ast::common::{BinOperator, BuiltinType, Projector, Sizes, TypedBinop, PathUL, NamePath};
use crate::ast::rust as rr;
use crate::ast::typed_rust as tr;
use crate::frontend::Module;
use super::context::ModuleInterface;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Val {
    Bool(bool),
    Integer(i64, Sizes),
    Uinteger(u64, Sizes),
    String(String),
    Struct(PathUL<()>, HashMap<String, Val>),
    Tuple(Vec<Val>),
}

pub fn add_deps(expr: &rr::Expr, path: &PathUL<()>, graph: &mut Graph) {
    match &*expr.content {
        rr::ExprInner::Array(exprs) | rr::ExprInner::Tuple(exprs) => {
            for expr in exprs {
                add_deps(expr, path, graph)
            }
        }
        rr::ExprInner::BinaryOp(BinOperator::Set, _, _) => {}
        rr::ExprInner::BinaryOp(_, e1, e2) => {
            add_deps(e1, path, graph);
            add_deps(e2, path, graph)
        }
        rr::ExprInner::BuildStruct(_, _) => todo!(),
        rr::ExprInner::BuildStructPath(_, _) => todo!(),

        rr::ExprInner::Coercion(expr, _)
        | rr::ExprInner::Parenthesis(expr)
        | rr::ExprInner::Proj(expr, _)
        | rr::ExprInner::UnaryOp(_, expr) => add_deps(expr, path, graph),
        rr::ExprInner::Bool(_) | rr::ExprInner::Int(_, _) | rr::ExprInner::String(_) => (),

        rr::ExprInner::Bloc(_) => todo!(),
        rr::ExprInner::Deref(_) => todo!(),
        rr::ExprInner::FunCall(_, _, _) => todo!(),
        rr::ExprInner::FunCallPath(_, _, _) => todo!(),
        rr::ExprInner::If(_, _, _) => todo!(),
        rr::ExprInner::Index(_, _) => todo!(),
        rr::ExprInner::MacroCall(_, _) => panic!("ICE"),
        rr::ExprInner::Method(_, _, _) => todo!(),
        rr::ExprInner::Ref(_, _) => todo!(),
        rr::ExprInner::Return(_) => todo!(),
        rr::ExprInner::Var(v) => {
            let mut path2 = path.clone();
            path2.pop();
            path2.push(NamePath::Name(v.get_content().to_string()));
            graph.add_edge(path, &path2);
        },
        rr::ExprInner::VarPath(v) => todo!(),
        rr::ExprInner::While(_, _) => todo!(),
    }
}

fn compute_i64(bin: TypedBinop, i1: i64, i2: i64) -> Val {
    match bin {
        TypedBinop::Add(s) => Val::Integer(i1 + i2, s),
        TypedBinop::Mul(_, s) => Val::Integer(i1 * i2, s),
        TypedBinop::Div(_, s) => Val::Integer(i1 / i2, s),
        TypedBinop::Mod(_, s) => Val::Integer(i1 % i2, s),
        TypedBinop::Sub(s) => Val::Integer(i1 - i2, s),
        TypedBinop::Eq(_) => Val::Bool(i1 == i2),
        TypedBinop::Neq(_) => Val::Bool(i1 != i2),
        TypedBinop::Greater(_, _) => Val::Bool(i1 > i2),
        TypedBinop::GreaterEq(_, _) => Val::Bool(i1 >= i2),
        TypedBinop::Lower(_, _) => Val::Bool(i1 < i2),
        TypedBinop::LowerEq(_, _) => Val::Bool(i1 < i2),
        TypedBinop::And(s) => Val::Integer(i1 & i2, s),
        TypedBinop::Or(s) => Val::Integer(i1 | i2, s),
        TypedBinop::Shl(s) => Val::Integer(i1 << i2, s),
        TypedBinop::Shr(s) => todo!(),
    }
}

fn compute_u64(bin: TypedBinop, i1: u64, i2: u64) -> Val {
    match bin {
        TypedBinop::Add(s) => Val::Uinteger(i1 + i2, s),
        TypedBinop::Mul(_, s) => Val::Uinteger(i1 * i2, s),
        TypedBinop::Div(_, s) => Val::Uinteger(i1 / i2, s),
        TypedBinop::Mod(_, s) => Val::Uinteger(i1 % i2, s),
        TypedBinop::Sub(s) => Val::Uinteger(i1 - i2, s),
        TypedBinop::Eq(_) => Val::Bool(i1 == i2),
        TypedBinop::Neq(_) => Val::Bool(i1 != i2),
        TypedBinop::Greater(_, _) => Val::Bool(i1 > i2),
        TypedBinop::GreaterEq(_, _) => Val::Bool(i1 >= i2),
        TypedBinop::Lower(_, _) => Val::Bool(i1 < i2),
        TypedBinop::LowerEq(_, _) => Val::Bool(i1 < i2),
        TypedBinop::And(s) => Val::Uinteger(i1 & i2, s),
        TypedBinop::Or(s) => Val::Uinteger(i1 | i2, s),
        TypedBinop::Shl(s) => Val::Uinteger(i1 << i2, s),
        TypedBinop::Shr(s) => todo!(),
    }
}
fn compute_bool(bin: TypedBinop, i1: bool, i2: bool) -> Val {
    match bin {
        TypedBinop::Eq(_) => Val::Bool(i1 == i2),
        TypedBinop::Neq(_) => Val::Bool(i1 != i2),
        TypedBinop::Greater(_, _) => Val::Bool(i1 > i2),
        TypedBinop::GreaterEq(_, _) => Val::Bool(i1 >= i2),
        TypedBinop::Lower(_, _) => Val::Bool(i1 < i2),
        TypedBinop::LowerEq(_, _) => Val::Bool(i1 < i2),
        TypedBinop::And(_) => Val::Bool(i1 & i2),
        TypedBinop::Or(_) => Val::Bool(i1 | i2),
        _ => panic!("ICE, operation not handled {:?}", bin),
    }
}

pub fn compute_const(expr: tr::Expr, ctxt: &GlobalContext) -> Val {
    match *expr.content {
        tr::ExprInner::BinOp(bin, expr1, expr2) => {
            let val1 = compute_const(expr1, ctxt);
            let val2 = compute_const(expr2, ctxt);
            match (val1, val2) {
                (Val::Integer(i1, s1), Val::Integer(i2, s2)) => {
                    assert_eq!(s1, s2);
                    compute_i64(bin, i1, i2)
                }
                (Val::Uinteger(i1, s1), Val::Uinteger(i2, s2)) => {
                    assert_eq!(s1, s2);
                    compute_u64(bin, i1, i2)
                }
                (Val::Bool(i1), Val::Bool(i2)) => compute_bool(bin, i1, i2),
                (val1, val2) => panic!("Not handled {:?} {:?} {:?}", bin, val1, val2),
            }
        }
        tr::ExprInner::Bloc(_) => todo!(),
        tr::ExprInner::Bool(b) => Val::Bool(b),
        tr::ExprInner::Int(i) => match expr.typed.content {
            tr::PostTypeInner::BuiltIn(BuiltinType::Int(true, size)) => {
                Val::Integer(i as i64, size)
            }
            tr::PostTypeInner::BuiltIn(BuiltinType::Int(false, size)) => Val::Uinteger(i, size),
            _ => panic!("Should not happen"),
        },
        tr::ExprInner::BuildStruct(name, args) => {
            let mut map = HashMap::new();
            for (id, expr) in args {
                let val = compute_const(expr, ctxt);
                assert!(map.insert(id.content(), val).is_none());
            }
            Val::Struct(name, map)
        }
        tr::ExprInner::Coercion(_, _, _) => todo!(),
        tr::ExprInner::Deref(_) => todo!(),
        tr::ExprInner::FunCall(_, _) => todo!(),
        tr::ExprInner::FunCallPath(_, _) => todo!(),
        tr::ExprInner::If(_, _, _) => todo!(),
        tr::ExprInner::Print(_) | tr::ExprInner::PrintPtr(_) => panic!("ICE"),
        tr::ExprInner::Proj(expr, proj) => {
            let val = compute_const(expr, ctxt);
            match (proj, val) {
                (Projector::Int(id), Val::Tuple(mut t)) => t.swap_remove(id),
                (Projector::Name(id), Val::Struct(_, mut t)) => t.remove(id.get_content()).unwrap(),
                _ => panic!("ICE"),
            }
        }
        tr::ExprInner::Ref(_, _) => todo!(),
        tr::ExprInner::Return(_) => todo!(),
        tr::ExprInner::Set(_, _) => todo!(),
        tr::ExprInner::String(str) => Val::String(str),
        tr::ExprInner::Tuple(exprs) => Val::Tuple(
            exprs
                .into_iter()
                .map(|expr| compute_const(expr, ctxt))
                .collect(),
        ),
        tr::ExprInner::UnaOp(_, _) => todo!(),
        tr::ExprInner::Var(v) => todo!(), /*ctxt
            .get_const_val(v.get_content())
            .unwrap()
            .get_value()
            .clone(),*/
        tr::ExprInner::VarPath(v) => ctxt
            .get_const_val(&v)
            .unwrap()
            .get_value()
            .clone(),
        tr::ExprInner::While(_, _) => todo!(),
    }
}

/*
fn handle_constants(
    consts: Vec<rust::DeclConst>,
    ctxt: &mut GlobalContext,
    err_reporter: &ErrorReporter,
) {
    let mut graph = structs::Graph::new();
    for const_decl in consts.iter() {
        graph.add_node(const_decl.name.get_content().to_string());
    }
    for const_decl in consts.iter() {
        consts::add_deps(&const_decl.expr, const_decl.name.get_content(), &mut graph);
    }
    let consts = match topological_sort(consts, &graph) {
        Ok(c) => c,
        Err((id, mut consts)) => {
            let constant = consts.swap_remove(id);
            let err = TypeError::self_referencing_constant(constant.name);
            err_reporter.report(vec![err])
        }
    };
    for const_decl in consts.into_iter() {
        let expected_typ = match translate_typ(const_decl.typ, ctxt) {
            None => todo!(),
            Some(t) => t,
        };
        let (typing_info, expr) = match inferencer::type_const(const_decl.expr, ctxt, &expected_typ)
        {
            Ok(e) => e,
            Err(errs) => err_reporter.report(errs),
        };
        let expr = match expr::type_const(expr, ctxt, &expected_typ, &typing_info) {
            Ok(e) => e,
            Err(errs) => err_reporter.report(errs),
        };
        let value = consts::compute_const(expr, ctxt);
        ctxt.add_const(
            const_decl.name.get_content().to_string(),
            expected_typ,
            value,
        );
    }
}
*/

fn handle_file(file : &mut rr::File, path : &PathUL<()>, consts : &mut Vec<(PathUL<()>, rr::DeclConst)>) {
    let mut local_structs = Vec::new();
    local_structs.append(&mut file.content);
    for decl in local_structs.into_iter() {
        match decl {
            rr::Decl::Const(decl_const) => {
                let mut path = path.clone();
                path.push(NamePath::Name(decl_const.name.get_content().to_string()));
                consts.push((path.clone(), decl_const))
            },
            _ => file.content.push(decl),
        }
    }

}

pub fn handle_rec(module : &mut Module<rr::File>, path : PathUL<()>, consts : &mut Vec<(PathUL<()>, rr::DeclConst)>) {
    handle_file(&mut module.content, &path, consts);
    for (name, (_, submod)) in module.submodules.iter_mut() {
        let mut path = path.clone();
        path.push(NamePath::Name(name.clone()));
        handle_rec(submod, path, consts)
    }
}


pub fn handle(module : &mut Module<rr::File>, mut modint : ModuleInterface) -> ModuleInterface {
    let mut constants = Vec::new();
    handle_rec(module, PathUL::new(vec![NamePath::Name("crate".to_string())]), &mut constants);
    if constants.len() == 0 {
        return modint
    }

    let mut graph = super::structs::Graph::new();
    for (path, _) in constants.iter() {
        graph.add_node(path.clone())
    }

    for (path, const_decl) in constants.iter() {
        add_deps(&const_decl.expr, path, &mut graph);
    }

    let constants = match super::structs::topological_sort(constants, &graph) {
        Ok(c) => c,
        Err((id, mut consts)) => {
            let constant = consts.swap_remove(id);
            let err = super::errors::TypeError::self_referencing_constant(constant.1.name);
            todo!();
//            err_reporter.report(vec![err])
        }
    };

    for (mut path, const_decl) in constants.into_iter() {
        path.pop();
        let mut ctxt = GlobalContext::new(path, modint);
        let expected_typ = match super::types::translate_typ(const_decl.typ, &ctxt) {
            None => todo!(),
            Some(t) => t,
        };
        let (typing_info, expr) = match super::inferencer::type_const(const_decl.expr, &ctxt, &expected_typ)
        {
            Ok(e) => e,
            Err(errs) => todo!(),//err_reporter.report(errs),
        };
        let expr = match super::expr::type_const(expr, &ctxt, &expected_typ, &typing_info) {
            Ok(e) => e,
            Err(errs) => todo!(), //err_reporter.report(errs),
        };
        let value = compute_const(expr, &ctxt);
        ctxt.add_const(
            const_decl.name.get_content().to_string(),
            const_decl.public,
            expected_typ,
            value,
        );
        modint = ctxt.extract_module();
    }
    modint
}
