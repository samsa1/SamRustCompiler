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
}*/

fn compile_expr_pointer(ctxt: &mut context::Context, expr: llr::Expr) -> Asm {
    match *expr.content {
        llr::ExprInner::BinOp(_, _, _)
        | llr::ExprInner::Bloc(_)
        | llr::ExprInner::Bool(_)
        | llr::ExprInner::BuildStruct(_, _)
        | llr::ExprInner::FunCall(_, _)
        | llr::ExprInner::FunCallVar(_, _)
        | llr::ExprInner::If(_, _, _)
        | llr::ExprInner::Int(_, _)
        | llr::ExprInner::Print(_)
        | llr::ExprInner::Ref(_)
        | llr::ExprInner::Set(_, _, _)
        | llr::ExprInner::Tuple(_, _)
        | llr::ExprInner::Deref(_) => panic!("ICE"),
        llr::ExprInner::Constant(str) | llr::ExprInner::VarGlobal(str) => {
            todo!()
        }
        llr::ExprInner::Proj(expr, proj) => {
            let expr = if expr.typed.is_ref() {
                compile_expr_val(ctxt, expr)
            } else {
                compile_expr_pointer(ctxt, expr)
            };
            expr + addq(
                reg::Operand::Imm(proj as u64),
                reg::Operand::Reg(reg::RegQ::Rax),
            )
        }
        llr::ExprInner::VarId(id) => {
            movq(
                reg::Operand::Reg(reg::RegQ::Rbp),
                reg::Operand::Reg(reg::RegQ::Rax),
            ) + addq(
                reg::Operand::Imm(ctxt.find(id)),
                reg::Operand::Reg(reg::RegQ::Rax),
            )
        }
    }
}

fn compile_expr_val(ctxt: &mut context::Context, expr: llr::Expr) -> Asm {
    match *expr.content {
        llr::ExprInner::BinOp(_, _, _) => todo!(),
        llr::ExprInner::Bloc(bloc) => compile_bloc(ctxt, bloc),
        llr::ExprInner::Bool(b) => {
            if b {
                movq(reg::Operand::Imm(1), reg::Operand::Reg(reg::RegQ::Rax))
            } else {
                movq(reg::Operand::Imm(0), reg::Operand::Reg(reg::RegQ::Rax))
            }
        }
        llr::ExprInner::BuildStruct(id, exprs) => todo!(),
        llr::ExprInner::Constant(str) => todo!(),
        llr::ExprInner::Deref(_) => todo!(),
        llr::ExprInner::FunCall(_, _) => todo!(),
        llr::ExprInner::FunCallVar(_, _) => todo!(),
        llr::ExprInner::If(_, _, _) => todo!(),
        llr::ExprInner::Int(_, _) => todo!(),
        llr::ExprInner::Print(_) => todo!(),
        llr::ExprInner::Proj(expr, id) => {
            todo!()
        }
        llr::ExprInner::Ref(expr) => compile_expr_pointer(ctxt, expr),
        llr::ExprInner::Ref(_) => todo!(),
        llr::ExprInner::Set(_, _, _) => todo!(),
        llr::ExprInner::Tuple(_, _) => todo!(),
        llr::ExprInner::VarGlobal(_) => todo!(),
        llr::ExprInner::VarId(_) => todo!(),
    }
}

fn compile_bloc(ctxt: &mut context::Context, bloc: llr::Bloc) -> Asm {
    todo!()
}

pub fn to_asm(file: llr::File) -> asm::file::File {
    todo!()
}
