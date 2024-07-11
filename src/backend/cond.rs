use super::{compile_expr_val, context::Context, Location};
use crate::ast::{
    common::{Sizes, TypedBinop, TypedUnaop},
    low_level_repr::*,
};
use write_x86_64::{instr::Cond, *};

pub fn neg_cond(cond: Cond) -> Cond {
    match cond {
        Cond::E => Cond::NE,
        Cond::Z => Cond::NZ,
        Cond::NE => Cond::E,
        Cond::NZ => Cond::Z,
        Cond::S => Cond::NS,
        Cond::NS => Cond::S,
        Cond::G => Cond::LE,
        Cond::GE => Cond::L,
        Cond::L => Cond::GE,
        Cond::LE => Cond::G,
        Cond::A => Cond::BE,
        Cond::AE => Cond::B,
        Cond::B => Cond::AE,
        Cond::BE => Cond::A,
    }
}

pub fn compile_cond(
    ctxt: &mut Context,
    expr: Expr,
    stack_offset: u64,
    label_true: Option<reg::Label>,
    label_false: reg::Label,
) -> Segment<instr::Instr> {
    match *expr.content {
        ExprInner::UnaOp(UnaOp::Unary(TypedUnaop::Not(size)), expr) => {
            assert_eq!(size, Sizes::S8);
            match label_true {
                Some(label_true) => {
                    compile_cond(ctxt, expr, stack_offset, Some(label_false), label_true)
                }
                None => {
                    let (label, _) = ctxt.gen_if_labels();
                    compile_cond(ctxt, expr, stack_offset, Some(label_false), label.clone())
                        + Segment::label(label)
                }
            }
        }
        ExprInner::BinOp(TypedBinop::LAnd, expr1, expr2) => {
            compile_cond(ctxt, expr1, stack_offset, None, label_false.clone())
                + compile_cond(ctxt, expr2, stack_offset, label_true, label_false)
        }
        ExprInner::BinOp(TypedBinop::LOr, expr1, expr2) => match label_true {
            Some(label) => {
                compile_cond(
                    ctxt,
                    expr1,
                    stack_offset,
                    Some(label.clone()),
                    label_false.clone(),
                ) + compile_cond(ctxt, expr2, stack_offset, Some(label), label_false)
            }
            None => {
                let label = ctxt.gen_if_labels().0;
                compile_cond(
                    ctxt,
                    expr1,
                    stack_offset,
                    Some(label.clone()),
                    label_false.clone(),
                ) + compile_cond(ctxt, expr2, stack_offset, None, label_false)
                    + Segment::label(label)
            }
        },
        ExprInner::BinOp(op, expr1, expr2) if super::get_cond(false, op).is_some() => {
            let cond = super::get_cond(false, op).unwrap();
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
                            + movb(addr!(RSP), reg!(AL))
                            + addq(immq(pad as i64), reg!(RSP))
                            + movb(reg!(AL), addr!(RSP))
                    }
                    2 => {
                        expr2
                            + movw(addr!(RSP), reg!(AX))
                            + addq(immq(pad as i64), reg!(RSP))
                            + movw(reg!(AX), addr!(RSP))
                    }
                    4 => {
                        expr2
                            + movl(addr!(RSP), reg!(EAX))
                            + addq(immq(pad as i64), reg!(RSP))
                            + movl(reg!(EAX), addr!(RSP))
                    }
                    8 => {
                        expr2
                            + movq(addr!(RSP), reg!(RAX))
                            + addq(immq(pad as i64), reg!(RSP))
                            + movq(reg!(RAX), addr!(RSP))
                    }
                    _ => panic!("ICE"),
                },
            };
            let (loc, expr1) = compile_expr_val(ctxt, expr1, stack_offset + size as u64);
            let expr1 = match loc {
                Location::Rax | Location::Never => expr1,
                Location::StackWithPadding(pad) => expr1 + super::move_stack_to_rax(pad, size),
            };
            let mov = match size {
                0 => nop(),
                1 => movb(addr!(RSP), reg!(CL)) + addq(immq(1), reg!(RSP)),
                2 => movw(addr!(RSP), reg!(CX)) + addq(immq(2), reg!(RSP)),
                4 => movl(addr!(RSP), reg!(ECX)) + addq(immq(4), reg!(RSP)),
                8 => popq(RCX),
                _ => panic!("ICE"),
            };

            let op = match size {
                1 => cmpb(reg!(CL), reg!(AL)),
                2 => cmpw(reg!(CX), reg!(AX)),
                4 => cmpl(reg!(ECX), reg!(EAX)),
                8 => cmpq(reg!(RCX), reg!(RAX)),
                _ => panic!("ICE"),
            };
            let mut asm = expr2 + expr1 + mov + op + jcc(neg_cond(cond), label_false);
            if let Some(label) = label_true {
                asm += jmp(label)
            }
            asm
        }
        _ => {
            assert_eq!(expr.size, 1);
            let (loc, mut asm) = compile_expr_val(ctxt, expr, stack_offset);
            match loc {
                Location::Rax | Location::Never => (),
                Location::StackWithPadding(pad) => asm += super::move_stack_to_rax(pad, 1),
            };

            asm += testb(reg!(AL), reg!(AL)) + jz(label_false);
            match label_true {
                None => (),
                Some(label) => asm += jmp(label),
            }
            asm
        }
    }
}
