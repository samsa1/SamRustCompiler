use crate::ast::low_level_repr as llr;

mod asm;
use asm::*;
mod context;
/*
#[derive(Debug, Clone)]
enum Target {
    Op(reg::Operand<reg::RegQ>),
    Stack(usize),
}

impl Target {
    fn to_op(self) -> reg::Operand<reg::RegQ> {
        match self {
            Self::Op(op) => op,
            Self::Stack(_) => todo!(),
        }
    }

    fn push(self, added: usize) -> Self {
        match self {
            Self::Stack(original) => Self::Stack(original + added),
            _ => self,
        }
    }

    fn offset(self, added: usize) -> Self {
        match self {
            Self::Stack(original) => Self::Stack(original + added),
            Self::Op(op) => Self::Op(op.offset(added)),
        }
    }
}

fn compile_expr_pointer(ctxt: &mut context::Context, expr: Expr, target: Target) -> Asm {
    match *expr.content {
        ExprInner::Bloc(_)
        | ExprInner::BuildStruct(_, _)
        | ExprInner::Bool(_)
        | ExprInner::Deref(_)
        | ExprInner::FunCall(_, _)
        | ExprInner::If(_, _, _)
        | ExprInner::Int(_)
        | ExprInner::Print(_)
        | ExprInner::Ref(_, _)
        | ExprInner::Set(_, _)
        | ExprInner::Tuple(_) => {
            panic!("ICE : case should have been ruled out by passes/move_refs")
        }
        ExprInner::Proj(expr, proj) => {
            let offset = ctxt.get_proj_offset(&expr.typed, &proj);
            let asm1 = compile_expr_pointer(ctxt, expr, target.clone());
            asm1 + addq(immq(offset as u64), target.to_op())
        }
        ExprInner::String(string) => {
            let label = ctxt.get_label_for_string(string);
            movq(reg::Operand::Label(label), target.to_op())
        }
        ExprInner::Var(name) => movq(ctxt.get_pos_var(name.get_content()), target.to_op()),
        ExprInner::BinOp(_, _, _) => todo!(),
    }
}

fn compile_expr_val(ctxt: &mut context::Context, expr: Expr, target: Target) -> Asm {
    match *expr.content {
        ExprInner::Bloc(bloc) => compile_bloc(ctxt, bloc, target),
        ExprInner::Int(id) => movq(immq(id), target.to_op()),
        ExprInner::Bool(b) => movq(immq(if b { 1 } else { 0 }), target.to_op()),
        ExprInner::BuildStruct(_, _) => todo!(),
        ExprInner::Deref(_) => todo!(),
        ExprInner::FunCall(_, _) => todo!(),
        ExprInner::If(_, _, _) => todo!(),
        ExprInner::Print(string) => {
            let label = ctxt.get_label_for_string(string);
            movq(reg::Operand::Label(label), regq(reg::RegQ::Rdi)) + call(ctxt.get_print_fun())
        }
        ExprInner::Proj(_, _) => todo!(),
        ExprInner::Ref(_, expr) => compile_expr_pointer(ctxt, expr, target),
        ExprInner::Set(_, _) => todo!(),
        ExprInner::String(_) => todo!(),
        ExprInner::Tuple(_) => todo!(),
        ExprInner::Var(_) => todo!(),
        _ => todo!(),
    }
}

fn compile_bloc(ctxt: &mut context::Context, bloc: Bloc, target: Target) -> Asm {
    todo!()
}*/

fn to_asm(file: llr::File) -> asm::file::File {
    todo!()
}
