use std::collections::HashMap;

use crate::ast::asm::{ImmOrReg, Registers};
use crate::ast::common::{BuiltinType, ComputedValue, Sizes};
use crate::ast::low_level_repr as llr;
use crate::ast::operators::{HArithDesc, Logic, TBinop, TUnaop};
use crate::ast::typed_rust::PostType;
use llr::{Pos, Value};

use super::utils::{mov_struct, remove_pad};
use write_x86_64::*;

use super::base;
mod cond;
use super::context::Context;

/// Value stored at RSP + below, with above empty spaces above
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Pad {
    pub above: u64,
    pub below: u64,
}

impl Pad {
    fn addr<R: traits::Reg>(&self) -> reg::Operand<R> {
        addr!(self.below as i64, RSP)
    }

    const fn empty() -> Self {
        Self { above: 0, below: 0 }
    }

    fn total(&self) -> u64 {
        self.above + self.below
    }

    fn to_reg(&self, size: usize, reg: Registers) -> Segment<instr::Instr> {
        match size {
            0 => remove_pad(self.total()),
            1 => movb(self.addr(), reg!(reg.b())) + remove_pad(1 + self.total()),
            2 => movw(self.addr(), reg!(reg.w())) + remove_pad(2 + self.total()),
            4 => movl(self.addr(), reg!(reg.l())) + remove_pad(4 + self.total()),
            8 => remove_pad(self.below) + popq(reg.q()) + remove_pad(self.above),
            _ => panic!("No handled"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Location {
    StackWithPadding(Pad),
    Rax,
    Never,
}

impl Location {
    fn to_reg(&self, size: usize, reg: Registers) -> Text {
        match self {
            Self::Rax if matches!(reg, Registers::RegA) => Text::empty(),
            Self::Rax => match size {
                1 => movb(reg!(AL), reg!(reg.b())),
                2 => movw(reg!(AX), reg!(reg.w())),
                4 => movl(reg!(EAX), reg!(reg.l())),
                8 => movq(reg!(RAX), reg!(reg.q())),
                _ => panic!("ICE"),
            },
            Self::Never => Text::empty(),
            Self::StackWithPadding(pad) => pad.to_reg(size, reg),
        }
    }
}

fn compile_expr_pointer(
    ctxt: &mut Context,
    expr: llr::Expr,
    stack_offset: u64,
) -> Segment<instr::Instr> {
    match *expr.content {
        llr::ExprInner::BinOp(_, _, _)
        | llr::ExprInner::Bloc(_)
        | llr::ExprInner::BuildStruct(_, _)
        | llr::ExprInner::Coercion(_, _, _)
        | llr::ExprInner::FunCall(_, _)
        | llr::ExprInner::FunCallVar(_, _)
        | llr::ExprInner::If(_, _, _)
        | llr::ExprInner::Print(_)
        | llr::ExprInner::Ref(_)
        | llr::ExprInner::Set(_, _, _)
        | llr::ExprInner::Tuple(_, _)
        | llr::ExprInner::Return(_)
        | llr::ExprInner::Value(_)
        | llr::ExprInner::While(_, _)
        | llr::ExprInner::UnaOp(_, _) => panic!("ICE"),
        llr::ExprInner::Deref(expr) => {
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset);
            expr + loc.to_reg(8, Registers::RegA)
        }
        llr::ExprInner::Constant(_str) | llr::ExprInner::FunVar(_str) => {
            todo!()
        }
        llr::ExprInner::Proj(expr, proj) => {
            let expr = if expr.typed.is_ref() {
                compile_expr_val(ctxt, expr, stack_offset).1
            } else {
                compile_expr_pointer(ctxt, expr, stack_offset)
            };
            expr + addq(immq(proj as i64), reg!(RAX))
        }
        llr::ExprInner::VarId(id) => leaq(addr!(ctxt.find(id), RBP), RAX),
    }
}

fn compile_div(size: Sizes, signed: bool, v: Value, pos: Pos) -> Segment<instr::Instr> {
    match (size, pos) {
        (Sizes::S8, _) => todo!(),
        (Sizes::S16, _) => todo!(),
        (Sizes::S32, Pos::Left) => {
            leaq(
                reg::Operand::LabRelAddr(reg::Label::from_str("division_by_zero_str".to_string())),
                R12,
            ) + testl(reg!(EAX), reg!(EAX))
                + jz(reg::Label::panic())
                + movl(reg!(EAX), reg!(ECX))
                + movl(imml(v.imm() as i32), reg!(EAX))
                + if signed {
                    cltd() + idivl(reg!(ECX))
                } else {
                    xorl(reg!(EDX), reg!(EDX)) + divl(reg!(ECX))
                }
        }
        (Sizes::S32, Pos::Right) => {
            let imm = v.imm();
            if imm == 0 {
                leaq(
                    reg::Operand::LabRelAddr(reg::Label::from_str(
                        "division_by_zero_str".to_string(),
                    )),
                    R12,
                ) + jmp(reg::Label::panic())
            } else {
                movl(imml(imm as i32), reg!(ECX))
                    + if signed {
                        cltd() + idivl(reg!(ECX))
                    } else {
                        xorl(reg!(EDX), reg!(EDX)) + divl(reg!(ECX))
                    }
            }
        }
        (Sizes::S64 | Sizes::SUsize, Pos::Left) => {
            leaq(
                reg::Operand::LabRelAddr(reg::Label::from_str("division_by_zero_str".to_string())),
                R12,
            ) + testq(reg!(RAX), reg!(RAX))
                + jz(reg::Label::panic())
                + movq(reg!(RAX), reg!(RCX))
                + movq(immq(v.imm()), reg!(RAX))
                + if signed {
                    cqto() + idivq(reg!(RCX))
                } else {
                    xorq(reg!(RDX), reg!(RDX)) + divq(reg!(RCX))
                }
        }
        (Sizes::S64 | Sizes::SUsize, Pos::Right) => {
            let imm = v.imm();
            if imm == 0 {
                leaq(
                    reg::Operand::LabRelAddr(reg::Label::from_str(
                        "division_by_zero_str".to_string(),
                    )),
                    R12,
                ) + jmp(reg::Label::panic())
            } else {
                movq(immq(imm), reg!(RCX))
                    + if signed {
                        cqto() + idivq(reg!(RCX))
                    } else {
                        xorq(reg!(RDX), reg!(RDX)) + divq(reg!(RCX))
                    }
            }
        }
    }
}

fn compile_op(op: TBinop, v: Value, pos: Pos) -> Segment<instr::Instr> {
    match (op, pos) {
        (TBinop::Logic(_), _) => panic!("ICE"),
        (TBinop::LArith(aop), Pos::Right) => aop.to_bin(ImmOrReg::V(v), Registers::RegA),

        (TBinop::LArith(aop), Pos::Left) if aop.commutes() => {
            aop.to_bin(ImmOrReg::V(v), Registers::RegA)
        }
        (TBinop::LArith(aop), Pos::Left) => {
            v.size().mov(Registers::RegA, Registers::RegC)
                + v.to_reg(Registers::RegA)
                + aop.to_bin(ImmOrReg::R(Registers::RegC), Registers::RegA)
        }

        (TBinop::HArith(dm), _) => {
            match dm.dm {
                HArithDesc::Div => compile_div(dm.size, dm.signed, v, pos),
                HArithDesc::Mod => {
                    compile_div(dm.size, dm.signed, v, pos) + movq(reg!(RDX), reg!(RAX))
                }
                HArithDesc::Mul => match dm.size {
                    Sizes::S8 | Sizes::S16 => todo!(),
                    Sizes::S32 => {
                        if dm.signed {
                            imull(imml(v.imm() as i32), reg!(EAX))
                        } else {
                            // TODO : use unsigned multiply
                            imull(imml(v.imm() as i32), reg!(EAX))
                        }
                    }
                    Sizes::S64 | Sizes::SUsize => {
                        if dm.signed {
                            imulq(immq(v.imm()), reg!(RAX))
                        } else {
                            // TODO : use unsigned multiply
                            imulq(immq(v.imm()), reg!(RAX))
                        }
                    }
                },
            }
        }

        (TBinop::Shl(s), Pos::Right) => v.to_reg(Registers::RegC) + s.shl_reg(Registers::RegA),
        (TBinop::Shr(s), Pos::Right) => v.to_reg(Registers::RegC) + s.shr_reg(Registers::RegA),

        (TBinop::Shl(s), Pos::Left) => {
            s.mov(Registers::RegA, Registers::RegC)
                + v.to_reg(Registers::RegA)
                + s.shl_reg(Registers::RegA)
        }
        (TBinop::Shr(s), Pos::Left) => {
            s.mov(Registers::RegA, Registers::RegC)
                + v.to_reg(Registers::RegA)
                + s.shr_reg(Registers::RegA)
        }

        (TBinop::Cmp(cmp), _) => {
            cmp.cond_rev(pos.is_left())
                .to_bin(ImmOrReg::V(v), Registers::RegA, Registers::RegA)
        }
    }
}

fn compile_expr_val(
    ctxt: &mut Context,
    expr: llr::Expr,
    stack_offset: u64,
) -> (Location, Segment<instr::Instr>) {
    let size = expr.size;
    match *expr.content {
        llr::ExprInner::UnaOp(op, expr) => {
            let size = expr.size;
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset);
            let expr = expr + loc.to_reg(size, Registers::RegA);
            let op = match op {
                llr::UnaOp::Unary(TUnaop::Neg(Sizes::S8)) => negb(reg!(AL)),
                llr::UnaOp::Unary(TUnaop::Neg(Sizes::S16)) => negw(reg!(AX)),
                llr::UnaOp::Unary(TUnaop::Neg(Sizes::S32)) => negl(reg!(EAX)),
                llr::UnaOp::Unary(TUnaop::Neg(Sizes::S64)) => negq(reg!(RAX)),
                llr::UnaOp::Unary(TUnaop::Neg(Sizes::SUsize)) => negq(reg!(RAX)),
                llr::UnaOp::Unary(TUnaop::Not(Sizes::S8)) => xorb(immb(1), reg!(AL)),
                llr::UnaOp::Unary(TUnaop::Not(Sizes::S16)) => xorw(immw(1), reg!(AX)),
                llr::UnaOp::Unary(TUnaop::Not(Sizes::S32)) => xorl(imml(1), reg!(EAX)),
                llr::UnaOp::Unary(TUnaop::Not(Sizes::S64)) => xorq(immq(1), reg!(RAX)),
                llr::UnaOp::Unary(TUnaop::Not(Sizes::SUsize)) => xorq(immq(1), reg!(RAX)),
                llr::UnaOp::Binary(op, v, pos) => compile_op(op, v, pos),
            };
            (Location::Rax, expr + op)
        }

        llr::ExprInner::BinOp(TBinop::Logic(Logic::LAnd), expr1, expr2) => {
            let (loc, expr2) = compile_expr_val(ctxt, expr2, stack_offset);
            let expr2 = expr2 + loc.to_reg(1, Registers::RegA);
            let (label_failed, label2) = ctxt.gen_if_labels();
            (
                Location::Rax,
                cond::compile_cond(ctxt, expr1, stack_offset, None, label_failed.clone())
                    + expr2
                    + jmp(label2.clone())
                    + Segment::label(label_failed)
                    + movb(immb(0), reg!(AL))
                    + Segment::label(label2),
            )
        }

        llr::ExprInner::BinOp(TBinop::Logic(Logic::LOr), expr1, expr2) => {
            let (loc, expr2) = compile_expr_val(ctxt, expr2, stack_offset);
            let expr2 = expr2 + loc.to_reg(1, Registers::RegA);
            let (label_success, label_fail) = ctxt.gen_if_labels();
            (
                Location::Rax,
                cond::compile_cond(ctxt, expr1, stack_offset, None, label_fail.clone())
                    + movb(immb(1), reg!(AL))
                    + jmp(label_success.clone())
                    + Segment::label(label_fail)
                    + expr2
                    + Segment::label(label_success),
            )
        }

        llr::ExprInner::BinOp(op, expr1, expr2) => {
            let size = expr2.size;
            let (loc, expr2) = compile_expr_val(ctxt, expr2, stack_offset);
            let expr2 = match loc {
                Location::Never => expr2,
                Location::Rax => match size {
                    0 => expr2,
                    1 => expr2 + subq(immq(1), reg!(RSP)) + movb(reg!(AL), addr!(RSP)),
                    2 => expr2 + subq(immq(2), reg!(RSP)) + movw(reg!(AX), addr!(RSP)),
                    4 => expr2 + subq(immq(4), reg!(RSP)) + movl(reg!(EAX), addr!(RSP)),
                    8 => expr2 + pushq(reg!(RAX)),
                    _ => panic!("ICE"),
                },
                Location::StackWithPadding(pad) => match size {
                    0 => expr2,
                    1 => {
                        expr2
                            + movb(pad.addr(), reg!(AL))
                            + remove_pad(pad.total())
                            + movb(reg!(AL), addr!(RSP))
                    }
                    2 => {
                        expr2
                            + movw(pad.addr(), reg!(AX))
                            + remove_pad(pad.total())
                            + movw(reg!(AX), addr!(RSP))
                    }
                    4 => {
                        expr2
                            + movl(pad.addr(), reg!(EAX))
                            + remove_pad(pad.total())
                            + movl(reg!(EAX), addr!(RSP))
                    }
                    8 => {
                        expr2
                            + movq(pad.addr(), reg!(RAX))
                            + remove_pad(pad.total())
                            + movq(reg!(RAX), addr!(RSP))
                    }
                    _ => panic!("ICE"),
                },
            };
            let (loc, expr1) = compile_expr_val(ctxt, expr1, stack_offset + size as u64);
            let expr1 = expr1 + loc.to_reg(size, Registers::RegA);
            let mov = match size {
                0 => nop(),
                1 => movb(addr!(RSP), reg!(CL)) + remove_pad(1),
                2 => movw(addr!(RSP), reg!(CX)) + remove_pad(2),
                4 => movl(addr!(RSP), reg!(ECX)) + remove_pad(4),
                8 => popq(RCX),
                _ => panic!("ICE"),
            };
            let op = match op {
                TBinop::Logic(_) => panic!("ICE"),
                TBinop::LArith(aop) => aop.to_bin(ImmOrReg::R(Registers::RegC), Registers::RegA),

                TBinop::HArith(haop) => {
                    match (haop.dm, haop.size) {
                        (_, Sizes::S8) => todo!(),
                        (_, Sizes::S16) => todo!(),
                        (HArithDesc::Mul, Sizes::S32) => {
                            if haop.signed {
                                imull(reg!(ECX), reg!(EAX))
                            } else {
                                // TODO use unsigned multiply
                                imull(reg!(ECX), reg!(EAX))
                            }
                        }
                        (HArithDesc::Mul, Sizes::S64 | Sizes::SUsize) => {
                            if haop.signed {
                                imulq(reg!(RCX), reg!(RAX))
                            } else {
                                // TODO use unsigned multiply
                                imulq(reg!(RCX), reg!(RAX))
                            }
                        }
                        (HArithDesc::Div, Sizes::S32) => {
                            leaq(
                                reg::Operand::LabRelAddr(reg::Label::from_str(
                                    "division_by_zero_str".to_string(),
                                )),
                                R12,
                            ) + testl(reg!(ECX), reg!(ECX))
                                + jz(reg::Label::panic())
                                + if haop.signed {
                                    cltd() + idivl(reg!(ECX))
                                } else {
                                    xorl(reg!(EDX), reg!(EDX)) + divl(reg!(ECX))
                                }
                        }
                        (HArithDesc::Div, Sizes::S64 | Sizes::SUsize) => {
                            leaq(
                                reg::Operand::LabRelAddr(reg::Label::from_str(
                                    "division_by_zero_str".to_string(),
                                )),
                                R12,
                            ) + testq(reg!(RCX), reg!(RCX))
                                + jz(reg::Label::panic())
                                + if haop.signed {
                                    cqto() + idivq(reg!(RCX))
                                } else {
                                    xorq(reg!(RDX), reg!(RDX)) + divq(reg!(RCX))
                                }
                        }
                        (HArithDesc::Mod, Sizes::S32) => {
                            leaq(
                                reg::Operand::LabRelAddr(reg::Label::from_str(
                                    "division_by_zero_str".to_string(),
                                )),
                                R12,
                            ) + testl(reg!(ECX), reg!(ECX))
                                + jz(reg::Label::panic())
                                + if haop.signed {
                                    cltd() + idivl(reg!(ECX))
                                } else {
                                    xorl(reg!(EDX), reg!(EDX)) + divl(reg!(ECX))
                                }
                                + movl(reg!(EDX), reg!(EAX))
                        }
                        (HArithDesc::Mod, Sizes::S64 | Sizes::SUsize) => {
                            leaq(
                                reg::Operand::LabRelAddr(reg::Label::from_str(
                                    "division_by_zero_str".to_string(),
                                )),
                                R12,
                            ) + testq(reg!(RCX), reg!(RCX))
                                + jz(reg::Label::panic())
                                + if haop.signed {
                                    cqto() + idivq(reg!(RCX))
                                } else {
                                    xorq(reg!(RDX), reg!(RDX)) + divq(reg!(RCX))
                                }
                                + movq(reg!(RDX), reg!(RAX))
                        }
                    }
                }

                TBinop::Shl(s) => s.shl_reg(Registers::RegA),
                TBinop::Shr(s) => s.shr_reg(Registers::RegA),

                TBinop::Cmp(cmp) => cmp.to_bin(
                    ImmOrReg::R(Registers::RegC),
                    Registers::RegA,
                    Registers::RegA,
                ),
            };
            (Location::Rax, expr2 + expr1 + mov + op)
        }

        llr::ExprInner::Coercion(expr, typ1, typ2) => {
            let size_in = expr.size;
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset);
            let expr = expr + loc.to_reg(size_in, Registers::RegA);
            let conversion = match (typ1, typ2) {
                (t1, t2) if t1 == t2 => Text::empty(),
                (BuiltinType::Int(true, s1), BuiltinType::Int(_, s2)) => match (s1, s2) {
                    (Sizes::S8, Sizes::S8)
                    | (Sizes::S16, Sizes::S16)
                    | (Sizes::S32, Sizes::S32)
                    | (Sizes::S64, Sizes::S64)
                    | (Sizes::S64, Sizes::SUsize)
                    | (Sizes::SUsize, Sizes::S64)
                    | (Sizes::SUsize, Sizes::SUsize) => nop(),
                    (Sizes::S8, Sizes::S16) => movsbw(reg!(AL), AX),
                    (Sizes::S8, Sizes::S32) => movsbl(reg!(AL), EAX),
                    (Sizes::S8, Sizes::S64) | (Sizes::S8, Sizes::SUsize) => movsbq(reg!(AL), RAX),
                    (Sizes::S16, Sizes::S32) => movswl(reg!(AX), EAX),
                    (Sizes::S16, Sizes::S64) | (Sizes::S16, Sizes::SUsize) => movswq(reg!(AX), RAX),
                    (Sizes::S32, Sizes::S64) | (Sizes::S32, Sizes::SUsize) => {
                        movslq(reg!(EAX), RAX)
                    }

                    _ => {
                        assert!(s1.to_byte_size() > s2.to_byte_size());
                        nop()
                    }
                },
                (BuiltinType::Int(false, s1), BuiltinType::Int(_, s2)) => {
                    match (s1, s2) {
                        (Sizes::S8, Sizes::S8)
                        | (Sizes::S16, Sizes::S16)
                        | (Sizes::S32, Sizes::S32)
                        | (Sizes::S64, Sizes::S64)
                        | (Sizes::S64, Sizes::SUsize)
                        | (Sizes::SUsize, Sizes::S64)
                        | (Sizes::SUsize, Sizes::SUsize) => nop(),
                        (Sizes::S8, Sizes::S16) => movzbw(reg!(AL), AX),
                        (Sizes::S8, Sizes::S32) => movzbl(reg!(AL), EAX),
                        (Sizes::S8, Sizes::S64) | (Sizes::S8, Sizes::SUsize) => {
                            movzbq(reg!(AL), RAX)
                        }
                        (Sizes::S16, Sizes::S32) => movzwl(reg!(AX), EAX),
                        (Sizes::S16, Sizes::S64) | (Sizes::S16, Sizes::SUsize) => {
                            movzwq(reg!(AX), RAX)
                        }
                        (Sizes::S32, Sizes::S64) | (Sizes::S32, Sizes::SUsize) => {
                            // this instruction should fill top bytes of RAX with zeros
                            movl(reg!(EAX), reg!(EAX))
                        }
                        _ => {
                            println!("{s1:?} {s2:?}");
                            assert!(s1.to_byte_size() > s2.to_byte_size());
                            nop()
                        }
                    }
                }
                (t1, t2) => {
                    println!("Coercion {:?} {:?} to do", t1, t2);
                    todo!()
                }
            };
            (Location::Rax, expr + conversion)
        }

        llr::ExprInner::Bloc(bloc) => compile_bloc(ctxt, bloc, stack_offset),
        llr::ExprInner::BuildStruct(struct_size, exprs) => {
            let mut asm = subq(immq(struct_size as i64), reg!(RSP));
            for (offset, expr) in exprs {
                let size = expr.size;
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset + struct_size as u64);
                asm = asm + expr;
                asm = asm
                    + match loc {
                        Location::Rax => {
                            if size == 1 {
                                movb(
                                    reg!(AL),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        RBP
                                    ),
                                )
                            } else if size == 2 {
                                movw(
                                    reg!(AX),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        RBP
                                    ),
                                )
                            } else if size == 4 {
                                movl(
                                    reg!(EAX),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        RBP
                                    ),
                                )
                            } else if size == 8 {
                                movq(
                                    reg!(RAX),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        RBP
                                    ),
                                )
                            } else {
                                panic!("ICE")
                            }
                        }
                        Location::Never => todo!(),
                        Location::StackWithPadding(pad) => {
                            mov_struct(
                                RSP,
                                pad.below as i64,
                                RBP,
                                -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                size as u64,
                                Registers::RegA,
                            ) + remove_pad(pad.total() + size as u64)
                        }
                    }
            }
            (Location::StackWithPadding(Pad::empty()), asm)
        }
        llr::ExprInner::Constant(_) => todo!(),
        llr::ExprInner::Deref(expr) => {
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset);
            let expr = expr + loc.to_reg(8, Registers::RegA);
            (
                Location::StackWithPadding(Pad::empty()),
                expr + subq(immq(size as i64), reg!(RSP))
                    + mov_struct(RAX, 0, RSP, 0, size as u64, Registers::RegC),
            )
        }
        llr::ExprInner::FunCall(name, args) => {
            let label = ctxt.fun_label(&name);
            let mut total_size = 0;
            for arg in &args {
                total_size += arg.size as u64
            }
            let offset = (total_size + expr.size as u64 + stack_offset) % 16;
            let missing = if offset == 0 { 0 } else { 16 - offset };
            let mut asm = subq(
                immq(missing as i64 + expr.size as i64 + total_size as i64),
                reg!(RSP),
            );
            let mut current_offset = stack_offset + missing + expr.size as u64;
            let stack_offset = stack_offset + missing + total_size + expr.size as u64;
            for arg in args {
                let size = arg.size;
                current_offset += size as u64;
                let (loc, arg) = compile_expr_val(ctxt, arg, stack_offset);
                let asm2 = match loc {
                    Location::Rax => match size {
                        1 => movb(reg!(AL), addr!(-(current_offset as i64), RBP)),
                        2 => movw(reg!(AX), addr!(-(current_offset as i64), RBP)),
                        4 => movl(reg!(EAX), addr!(-(current_offset as i64), RBP)),
                        8 => movq(reg!(RAX), addr!(-(current_offset as i64), RBP)),
                        _ => panic!("ICE"),
                    },
                    Location::Never => panic!("ICE"),
                    Location::StackWithPadding(pad) => {
                        mov_struct(
                            RSP,
                            pad.below as i64,
                            RBP,
                            -(current_offset as i64),
                            size as u64,
                            Registers::RegA,
                        ) + remove_pad(pad.total() + size as u64)
                    }
                };
                asm = asm + arg + asm2;
            }
            assert_eq!(current_offset, stack_offset);
            asm = asm + call(label);
            //  + addq(immq(total_size as i64), reg::Operand::Reg(RSP));
            (
                Location::StackWithPadding(Pad {
                    above: missing,
                    below: total_size,
                }),
                asm,
            )
        }
        llr::ExprInner::FunCallVar(fun_var_id, args) => {
            let mut total_size = 0;
            for arg in &args {
                total_size += arg.size as u64
            }
            let offset = (total_size + expr.size as u64 + stack_offset) % 16;
            let missing = if offset == 0 { 0 } else { 16 - offset };
            let mut asm = subq(
                immq(missing as i64 + expr.size as i64 + total_size as i64),
                reg!(RSP),
            );
            let mut current_offset = stack_offset + missing + expr.size as u64;
            let stack_offset = stack_offset + missing + total_size + expr.size as u64;
            for arg in args {
                let size = arg.size;
                current_offset += size as u64;
                let (loc, arg) = compile_expr_val(ctxt, arg, stack_offset);
                let asm2 = match loc {
                    Location::Rax => match size {
                        1 => movb(reg!(AL), addr!(-(current_offset as i64), RBP)),
                        2 => movw(reg!(AX), addr!(-(current_offset as i64), RBP)),
                        4 => movl(reg!(EAX), addr!(-(current_offset as i64), RBP)),
                        8 => movq(reg!(RAX), addr!(-(current_offset as i64), RBP)),
                        _ => panic!("ICE"),
                    },
                    Location::Never => panic!("ICE"),
                    Location::StackWithPadding(pad) => {
                        mov_struct(
                            RSP,
                            pad.below as i64,
                            RBP,
                            -(current_offset as i64),
                            size as u64,
                            Registers::RegA,
                        ) + remove_pad(pad.total() + size as u64)
                    }
                };
                asm = asm + arg + asm2;
            }
            assert_eq!(current_offset, stack_offset);
            asm = asm + call_star(addr!(ctxt.find(fun_var_id), RBP));
            (
                Location::StackWithPadding(Pad {
                    above: missing,
                    below: total_size,
                }),
                asm,
            )
        }
        llr::ExprInner::If(expr, bloc1, bloc2) => {
            let (loc1, bloc1) = compile_bloc(ctxt, bloc1, stack_offset);
            let (loc2, bloc2) = compile_bloc(ctxt, bloc2, stack_offset);
            let (else_label, end_label) = ctxt.gen_if_labels();
            let (loc, bloc1, bloc2) = match (loc1, loc2) {
                (Location::Never, loc) => (loc, bloc1, bloc2),
                (loc, Location::Never) => (loc, bloc1, bloc2),
                (Location::Rax, Location::Rax) => (Location::Rax, bloc1, bloc2),
                (Location::Rax, Location::StackWithPadding(pad)) => (
                    Location::Rax,
                    bloc1,
                    bloc2 + pad.to_reg(size, Registers::RegA),
                ),
                (Location::StackWithPadding(pad), Location::Rax) => (
                    Location::Rax,
                    bloc1 + pad.to_reg(size, Registers::RegA),
                    bloc2,
                ),
                (Location::StackWithPadding(pad1), Location::StackWithPadding(pad2))
                    if pad1 == pad2 =>
                {
                    (Location::StackWithPadding(pad1), bloc1, bloc2)
                }
                (Location::StackWithPadding(pad1), Location::StackWithPadding(pad2)) => {
                    let pad3 = if pad1.total() >= pad2.total() {
                        pad1.total() as u64
                    } else {
                        pad2.total() as u64
                    };
                    let bloc1 = {
                        bloc1
                            + subq(immq(size as i64 + (pad3 - pad1.total()) as i64), reg!(RSP))
                            + mov_struct(
                                RSP,
                                (pad3 - pad1.above) as i64 + size as i64,
                                RSP,
                                0,
                                size as u64,
                                Registers::RegA,
                            )
                    };
                    let bloc2 = {
                        bloc2
                            + subq(immq(size as i64 + (pad3 - pad2.total()) as i64), reg!(RSP))
                            + mov_struct(
                                RSP,
                                (pad3 - pad2.above) as i64 + size as i64,
                                RSP,
                                0,
                                size as u64,
                                Registers::RegA,
                            )
                    };

                    (
                        Location::StackWithPadding(Pad {
                            below: 0,
                            above: pad3 + size as u64,
                        }),
                        bloc1,
                        bloc2,
                    )
                }
            };
            (
                loc,
                cond::compile_cond(ctxt, expr, stack_offset, None, else_label.clone())
                    + bloc1
                    + jmp(end_label.clone())
                    + Segment::label(else_label)
                    + bloc2
                    + Segment::label(end_label),
            )
        }
        llr::ExprInner::Print(label_name) => {
            let missing = if stack_offset % 16 == 0 {
                0
            } else {
                16 - (stack_offset % 16)
            };
            (
                Location::Rax,
                leaq(
                    reg::Operand::LabRelAddr(ctxt.string_label(&label_name)),
                    RDI,
                ) + movq(immq(0), reg!(RAX))
                    + subq(immq(missing as i64), reg!(RSP))
                    + call(reg::Label::printf())
                    + remove_pad(missing),
            )
        }
        llr::ExprInner::Proj(sub_expr, offset) => {
            println!("type of proj {:?}", sub_expr.typed);
            let sub_expr = if sub_expr.typed.is_ref() {
                let (loc, sub_expr) = compile_expr_val(ctxt, sub_expr, stack_offset);
                sub_expr + loc.to_reg(8, Registers::RegA)
            } else {
                compile_expr_pointer(ctxt, sub_expr, stack_offset)
            };
            match expr.size {
                1 => (
                    Location::Rax,
                    sub_expr + movb(addr!(offset as i64, RAX), reg!(AL)),
                ),
                2 => (
                    Location::Rax,
                    sub_expr + movw(addr!(offset as i64, RAX), reg!(AX)),
                ),
                4 => (
                    Location::Rax,
                    sub_expr + movl(addr!(offset as i64, RAX), reg!(EAX)),
                ),
                8 => (
                    Location::Rax,
                    sub_expr + movq(addr!(offset as i64, RAX), reg!(RAX)),
                ),
                size => (
                    Location::StackWithPadding(Pad::empty()),
                    sub_expr
                        + subq(immq(size as i64), reg!(RSP))
                        + mov_struct(RAX, offset as i64, RSP, 0, size as u64, Registers::RegC),
                ),
            }
        }
        llr::ExprInner::Ref(expr) => (
            Location::Rax,
            compile_expr_pointer(ctxt, expr, stack_offset),
        ),
        llr::ExprInner::Set(size, addr, expr) if addr.is_ref_var() => {
            assert_eq!(size, expr.size);
            let var_id = addr.get_ref_var().unwrap();
            let offset_from_rbp = ctxt.find(var_id);
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset);
            let asm = match loc {
                Location::Never => expr,
                Location::Rax => match size {
                    0 => expr,
                    1 => expr + movb(reg!(AL), addr!(offset_from_rbp, RBP)),
                    2 => expr + movw(reg!(AX), addr!(offset_from_rbp, RBP)),
                    4 => expr + movl(reg!(EAX), addr!(offset_from_rbp, RBP)),
                    8 => expr + movq(reg!(RAX), addr!(offset_from_rbp, RBP)),
                    _ => panic!("ICE"),
                },
                Location::StackWithPadding(pad) => {
                    expr + mov_struct(
                        RSP,
                        pad.below as i64,
                        RBP,
                        offset_from_rbp,
                        size as u64,
                        Registers::RegA,
                    ) + remove_pad(pad.total() + size as u64)
                }
            };
            (Location::Rax, asm)
        }
        llr::ExprInner::Set(size, addr, expr) => {
            assert_eq!(size, expr.size);
            let (loc, addr) = compile_expr_val(ctxt, addr, stack_offset);
            let addr = addr + loc.to_reg(8, Registers::RegA);
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset + 8);
            let mut asm = addr + pushq(reg!(RAX)) + expr;
            asm = asm
                + match loc {
                    Location::Rax => {
                        popq(RCX)
                            + if size == 1 {
                                movb(reg!(AL), addr!(RCX))
                            } else if size == 2 {
                                movw(reg!(AX), addr!(RCX))
                            } else if size == 4 {
                                movl(reg!(EAX), addr!(RCX))
                            } else if size == 8 {
                                movq(reg!(RAX), addr!(RCX))
                            } else {
                                panic!("ICE")
                            }
                    }
                    Location::Never => nop(),
                    Location::StackWithPadding(pad) => {
                        movq(addr!(-(stack_offset as i64 + 8), RBP), reg!(RCX))
                            + mov_struct(
                                RSP,
                                pad.below as i64,
                                RCX,
                                0,
                                size as u64,
                                Registers::RegA,
                            )
                            + remove_pad(pad.total() + size as u64 + 8)
                    }
                };
            (Location::Rax, asm)
        }
        llr::ExprInner::Tuple(tuple_size, exprs) => {
            println!("Build tuple : {tuple_size}",);
            let mut asm = subq(immq(tuple_size as i64), reg!(RSP));
            let mut current_offset = 0;
            for expr in exprs {
                let size = expr.size;
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset + tuple_size as u64);
                asm = asm + expr;
                asm = asm
                    + match loc {
                        Location::Never => nop(),
                        Location::Rax => {
                            if size == 0 {
                                nop()
                            } else if size == 1 {
                                movb(reg!(AL), addr!(current_offset, RSP))
                            } else if size == 4 {
                                movl(reg!(EAX), addr!(current_offset, RSP))
                            } else if size == 8 {
                                movq(reg!(RAX), addr!(current_offset, RSP))
                            } else {
                                panic!("ICE")
                            }
                        }
                        Location::StackWithPadding(pad) => {
                            mov_struct(
                                RSP,
                                pad.below as i64,
                                RSP,
                                size as i64 + pad.total() as i64 + current_offset,
                                size as u64,
                                Registers::RegA,
                            ) + remove_pad(pad.total() + size as u64)
                        }
                    };
                current_offset += size as i64;
            }
            (Location::StackWithPadding(Pad::empty()), asm)
        }
        llr::ExprInner::FunVar(str) => (Location::Rax, leaq(lab!(ctxt.fun_label(&str)), RAX)),
        llr::ExprInner::Value(v) => (Location::Rax, v.to_reg(Registers::RegA)),
        llr::ExprInner::VarId(id) => {
            let offset_from_rbp = ctxt.find(id);
            if expr.size == 1 {
                (Location::Rax, movb(addr!(offset_from_rbp, RBP), reg!(AL)))
            } else if expr.size == 2 {
                (Location::Rax, movw(addr!(offset_from_rbp, RBP), reg!(AX)))
            } else if expr.size == 4 {
                (Location::Rax, movl(addr!(offset_from_rbp, RBP), reg!(EAX)))
            } else if expr.size == 8 {
                (Location::Rax, movq(addr!(offset_from_rbp, RBP), reg!(RAX)))
            } else {
                let size = expr.size;
                (
                    Location::StackWithPadding(Pad::empty()),
                    subq(immq(size as i64), reg!(RSP))
                        + mov_struct(RBP, offset_from_rbp, RSP, 0, size as u64, Registers::RegA),
                )
            }
        }
        llr::ExprInner::Return(None) => (
            Location::Never,
            xorq(reg!(RAX), reg!(RAX)) + movq(reg!(RBP), reg!(RSP)) + popq(RBP) + ret(),
        ),
        llr::ExprInner::Return(Some(expr)) => {
            let size = expr.size as u64;
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset);
            (
                Location::Never,
                expr + match loc {
                    Location::Rax => match size {
                        1 => movb(reg!(AL), addr!(ctxt.get_return_offset(), RBP)),
                        2 => movw(reg!(AX), addr!(ctxt.get_return_offset(), RBP)),
                        4 => movl(reg!(EAX), addr!(ctxt.get_return_offset(), RBP)),
                        8 => movq(reg!(RAX), addr!(ctxt.get_return_offset(), RBP)),
                        _ => panic!("ICE"),
                    },
                    Location::Never => todo!(),
                    Location::StackWithPadding(pad) => mov_struct(
                        RSP,
                        pad.below as i64,
                        RBP,
                        ctxt.get_return_offset(),
                        size,
                        Registers::RegA,
                    ),
                } + movq(reg!(RBP), reg!(RSP))
                    + popq(RBP)
                    + ret(),
            )
        }

        llr::ExprInner::While(expr, bloc) => {
            let (loc2, bloc) = compile_bloc(ctxt, bloc, stack_offset);
            let bloc = match loc2 {
                Location::StackWithPadding(_) => panic!("ICE"),
                Location::Never | Location::Rax => bloc,
            };
            let (in_label, out_label) = ctxt.gen_while_labels();
            (
                Location::Rax,
                Segment::label(in_label.clone())
                    + cond::compile_cond(ctxt, expr, stack_offset, None, out_label.clone())
                    + bloc
                    + jmp(in_label)
                    + Segment::label(out_label),
            )
        }
    }
}

fn compile_bloc(
    ctxt: &mut Context,
    bloc: llr::Bloc,
    mut stack_offset: u64,
) -> (Location, Segment<instr::Instr>) {
    let initial_stack_offset = stack_offset;
    ctxt.add_layer();
    for instr in &bloc.content {
        if let llr::Instr::Binding(id, expr) = instr {
            stack_offset = ctxt.insert(*id, expr.size, stack_offset)
        }
    }

    let mut asm = subq(
        immq((stack_offset - initial_stack_offset) as i64),
        reg!(RSP),
    );
    let mut last_loc = Location::Rax;
    for instr in bloc.content {
        match instr {
            llr::Instr::Binding(id, expr) => {
                let expr = llr::Expr {
                    loc: expr.loc,
                    content: Box::new(llr::ExprInner::Set(
                        expr.size,
                        llr::Expr {
                            content: Box::new(llr::ExprInner::VarId(id)),
                            typed: expr.typed.clone(),
                            loc: expr.loc,
                            size: expr.size,
                        }
                        .to_ref(8),
                        expr,
                    )),
                    size: 0,
                    typed: PostType::unit(),
                };
                asm = asm + compile_expr_val(ctxt, expr, stack_offset).1;
                last_loc = Location::Rax;
            }
            llr::Instr::Expr(ComputedValue::Drop, expr) => {
                let size = expr.size;
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset);
                asm = asm
                    + match loc {
                        Location::StackWithPadding(pad) => {
                            expr + remove_pad(size as u64 + pad.total())
                        }
                        _ => expr,
                    };
                last_loc = Location::Rax;
            }

            llr::Instr::Expr(ComputedValue::Keep, expr) => {
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset);
                last_loc = loc;
                asm = asm + expr;
            }
        }
    }
    match last_loc {
        Location::StackWithPadding(pad) => {
            // assert_eq!(pad.below, 0);
            last_loc = Location::StackWithPadding(Pad {
                above: pad.above + stack_offset - initial_stack_offset,
                below: pad.below,
            })
        }
        Location::Never => (),
        Location::Rax => asm = asm + remove_pad(stack_offset - initial_stack_offset),
    }
    ctxt.drop_layer();
    (last_loc, asm)
}

fn compile_fun(fun_decl: llr::DeclFun, ctxt: &mut Context) -> Segment<instr::Instr> {
    let size = fun_decl.output;
    ctxt.init(fun_decl.args);
    let (loc, bloc) = compile_bloc(ctxt, fun_decl.content, 0);
    let mut asm = Segment::label(ctxt.fun_label(&fun_decl.name))
        + pushq(reg!(RBP))
        + movq(reg!(RSP), reg!(RBP))
        + bloc;
    match loc {
        Location::StackWithPadding(pad) => {
            asm = asm
                + mov_struct(
                    RSP,
                    pad.below as i64,
                    RBP,
                    ctxt.get_return_offset(),
                    size as u64,
                    Registers::RegA,
                )
        }
        Location::Rax => {
            asm = match size {
                0 => asm,
                1 => asm + movb(reg!(AL), addr!(ctxt.get_return_offset(), RBP)),
                2 => asm + movw(reg!(AX), addr!(ctxt.get_return_offset(), RBP)),
                4 => asm + movl(reg!(EAX), addr!(ctxt.get_return_offset(), RBP)),
                8 => asm + movq(reg!(RAX), addr!(ctxt.get_return_offset(), RBP)),
                _ => panic!("ICE"),
            }
        }
        Location::Never => (),
    }
    asm + movq(reg!(RBP), reg!(RSP)) + popq(RBP) + ret()
}

fn to_asm(file: llr::File, strings: HashMap<String, String>, ctxt: &mut Context) -> file::File {
    let mut text_ss = Segment::empty();
    for fun_decl in file.funs {
        text_ss = text_ss + compile_fun(fun_decl, ctxt)
    }

    text_ss = text_ss;

    let mut data_ss = Data::empty();
    for (str1, str2) in strings {
        data_ss += Segment::label(new_label(&str2)) + data::dasciz(str1)
    }
    data_ss += Segment::label(new_label("heap")) + data::space(base::HEAP_SIZE + 32);

    file::File {
        globl: None,
        text_ss,
        data_ss,
    }
}

pub fn compile(
    file: llr::File,
    strings: HashMap<String, String>,
    vec_info: crate::to_llr::VecInfo,
) -> file::File {
    let mut ctxt = Context::new();
    let file = to_asm(file, strings, &mut ctxt);
    let base = base::base(&mut ctxt, vec_info);
    if file.globl.is_some() {
        panic!("ICE {:?} {:?}", base.globl, file.globl);
    }

    file::File {
        globl: base.globl,
        text_ss: base.text_ss + file.text_ss,
        data_ss: base.data_ss + file.data_ss,
    }
}
