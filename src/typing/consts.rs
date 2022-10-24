use super::context::GlobalContext;
use super::structs::Graph;
use crate::ast::common::{BinOperator, BuiltinType, Projector, Sizes, TypedBinop};
use crate::ast::rust as rr;
use crate::ast::typed_rust as tr;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Val {
    Bool(bool),
    Integer(i64, Sizes),
    Uinteger(u64, Sizes),
    String(String),
    Struct(String, HashMap<String, Val>),
    Tuple(Vec<Val>),
}

pub fn add_deps(expr: &rr::Expr, name: &str, graph: &mut Graph) {
    match &*expr.content {
        rr::ExprInner::Array(exprs) | rr::ExprInner::Tuple(exprs) => {
            for expr in exprs {
                add_deps(expr, name, graph)
            }
        }
        rr::ExprInner::BinaryOp(BinOperator::Set, _, _) => {}
        rr::ExprInner::BinaryOp(_, e1, e2) => {
            add_deps(e1, name, graph);
            add_deps(e2, name, graph)
        }
        rr::ExprInner::BuildStruct(_, _) => todo!(),

        rr::ExprInner::Coercion(expr, _)
        | rr::ExprInner::Parenthesis(expr)
        | rr::ExprInner::Proj(expr, _)
        | rr::ExprInner::UnaryOp(_, expr) => add_deps(expr, name, graph),
        rr::ExprInner::Bool(_) | rr::ExprInner::Int(_, _) | rr::ExprInner::String(_) => (),

        rr::ExprInner::Bloc(b) => todo!(),
        rr::ExprInner::Deref(_) => todo!(),
        rr::ExprInner::FunCall(_, _, _) => todo!(),
        rr::ExprInner::If(_, _, _) => todo!(),
        rr::ExprInner::Index(_, _) => todo!(),
        rr::ExprInner::MacroCall(_, _) => panic!("ICE"),
        rr::ExprInner::Method(_, _, _) => todo!(),
        rr::ExprInner::Ref(_, _) => todo!(),
        rr::ExprInner::Var(v) => match graph.add_edge(name, v.get_content()) {
            None => todo!(),
            Some(_) => (),
        },
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
            Val::Struct(name.content(), map)
        }
        tr::ExprInner::Coercion(_, _, _) => todo!(),
        tr::ExprInner::Deref(_) => todo!(),
        tr::ExprInner::FunCall(_, _) => todo!(),
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
        tr::ExprInner::Set(_, _) => todo!(),
        tr::ExprInner::String(str) => Val::String(str),
        tr::ExprInner::Tuple(exprs) => Val::Tuple(
            exprs
                .into_iter()
                .map(|expr| compute_const(expr, ctxt))
                .collect(),
        ),
        tr::ExprInner::UnaOp(_, _) => todo!(),
        tr::ExprInner::Var(v) => ctxt
            .get_const_val(v.get_content())
            .unwrap()
            .get_value()
            .clone(),
    }
}
