use super::{compile_expr_val, context::Context, utils::remove_pad, Location};
use crate::ast::{
    asm::{
        ImmOrReg,
        Registers::{RegA, RegC},
    },
    common::Sizes,
    low_level_repr::*,
    operators::{Logic, TBinop, TUnaop},
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
        ExprInner::UnaOp(UnaOp::Unary(TUnaop::Not(size)), expr) => {
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
        ExprInner::BinOp(TBinop::Logic(Logic::LAnd), expr1, expr2) => {
            compile_cond(ctxt, expr1, stack_offset, None, label_false.clone())
                + compile_cond(ctxt, expr2, stack_offset, label_true, label_false)
        }
        ExprInner::BinOp(TBinop::Logic(Logic::LOr), expr1, expr2) => match label_true {
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
        ExprInner::UnaOp(UnaOp::Binary(TBinop::Cmp(cmp), v, pos), expr) => {
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset);
            expr + loc.move_to_reg(cmp.size.to_byte_size(), RegA)
                + cmp.size.cmp(ImmOrReg::V(v), RegA)
                + jcc(
                    neg_cond(cmp.cond_rev(pos.is_left()).get_cond()),
                    label_false,
                )
                + match label_true {
                    Some(label) => jmp(label),
                    None => Text::empty(),
                }
        }
        ExprInner::BinOp(TBinop::Cmp(cmp), expr1, expr2) => {
            let (loc, expr2) = compile_expr_val(ctxt, expr2, stack_offset);
            let size = cmp.size.to_byte_size();
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
                            + remove_pad(pad)
                            + movb(reg!(AL), addr!(RSP))
                    }
                    2 => {
                        expr2
                            + movw(addr!(RSP), reg!(AX))
                            + remove_pad(pad)
                            + movw(reg!(AX), addr!(RSP))
                    }
                    4 => {
                        expr2
                            + movl(addr!(RSP), reg!(EAX))
                            + remove_pad(pad)
                            + movl(reg!(EAX), addr!(RSP))
                    }
                    8 => {
                        expr2
                            + movq(addr!(RSP), reg!(RAX))
                            + remove_pad(pad)
                            + movq(reg!(RAX), addr!(RSP))
                    }
                    _ => panic!("ICE"),
                },
            };
            let (loc, expr1) = compile_expr_val(ctxt, expr1, stack_offset + size as u64);
            let expr1 = expr1 + loc.move_to_reg(size, RegA);
            let mov = match size {
                0 => nop(),
                1 => movb(addr!(RSP), reg!(CL)) + remove_pad(1),
                2 => movw(addr!(RSP), reg!(CX)) + remove_pad(2),
                4 => movl(addr!(RSP), reg!(ECX)) + remove_pad(4),
                8 => popq(RCX),
                _ => panic!("ICE"),
            };

            let op = cmp.size.cmp(ImmOrReg::R(RegC), RegA);
            let mut asm = expr2 + expr1 + mov + op + jcc(neg_cond(cmp.get_cond()), label_false);
            if let Some(label) = label_true {
                asm += jmp(label)
            }
            asm
        }
        _ => {
            assert_eq!(expr.size, 1);
            let (loc, mut asm) = compile_expr_val(ctxt, expr, stack_offset);
            asm += loc.move_to_reg(1, RegA);
            asm += testb(reg!(AL), reg!(AL)) + jz(label_false);
            match label_true {
                None => (),
                Some(label) => asm += jmp(label),
            }
            asm
        }
    }
}
