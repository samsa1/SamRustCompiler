use super::context::GlobalContext;
use super::structs::Graph;
use crate::ast::rust as rr;
use crate::ast::typed_rust as tr;

pub fn add_deps(expr: &rr::Expr, name: &str, graph: &mut Graph) {
    match &*expr.content {
        rr::ExprInner::Array(exprs) | rr::ExprInner::Tuple(exprs) => {
            for expr in exprs {
                add_deps(expr, name, graph)
            }
        }
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
        rr::ExprInner::Var(v) => todo!(),
    }
}

pub fn compute_const(expr: tr::Expr, ctxt: &GlobalContext) -> tr::Expr {
    todo!()
}
