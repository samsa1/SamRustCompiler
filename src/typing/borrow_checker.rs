use super::context::{GlobalContext, Trait};
use crate::ast::typed_rust::*;
use std::collections::HashSet;

struct ScopeScanResult {
    moved_values: HashSet<String>,
    result_dependance: HashSet<String>,
    used_mut_ref: HashSet<String>,
}

impl ScopeScanResult {
    fn empty() -> Self {
        Self {
            moved_values: HashSet::new(),
            result_dependance: HashSet::new(),
            used_mut_ref: HashSet::new(),
        }
    }

    fn mark_moved(&mut self, id: &str) -> bool {
        if self.used_mut_ref.contains(id) {
            todo!()
        }
        self.moved_values.insert(id.to_string())
    }

    fn drop_mut_refs(&mut self) {
        self.used_mut_ref = HashSet::new()
    }
}

fn check_scopes(
    ctxt: &GlobalContext,
    expr: &Expr,
    ref_autorized: bool,
    scope_scan: &mut ScopeScanResult,
) {
    match &*expr.content {
        ExprInner::Bool(_) | ExprInner::Int(_) => (),
        ExprInner::Bloc(_bloc) => todo!(),
        ExprInner::Var(id) => {
            if ctxt
                .has_trait(&expr.typed, &Trait::Name("Copy".to_string()))
                .is_none()
                && !scope_scan.mark_moved(id.get_content())
            {
                todo!()
            }
        },
        ExprInner::FunCall(_, _) => todo!(),
        ExprInner::Constructor(_, _) => todo!(),
        ExprInner::Ref(_, _) => todo!(),
        ExprInner::Deref(_) => todo!(),
        ExprInner::Tuple(_) => todo!(),
        ExprInner::BuildStruct(_, _) => todo!(),
        ExprInner::Proj(_, _) => todo!(),
        ExprInner::Set(_, _) => todo!(),
        ExprInner::Print(_) => todo!(),
        ExprInner::String(_) => todo!(),
        ExprInner::Vec(_) => todo!(),
        ExprInner::If(_, _, _) => todo!(),
    }
}
