use crate::ast::common::{ComputedValue, Sizes, TypedBinop, TypedUnaop};
use crate::ast::low_level_repr as llr;
use crate::ast::typed_rust::PostType;

#[macro_use]
use write_x86_64::*;

mod context;

#[derive(Debug, PartialEq, Eq)]
enum Location {
    StackWithPadding(u64),
    Rax,
    Never,
}

fn mov_struct(
    reg_in: reg::RegQ,
    offset_in: i64,
    reg_out: reg::RegQ,
    offset_out: i64,
    mut size: u64,
    free_regq: reg::RegQ,
    free_regl: reg::RegL,
    free_regw: reg::RegW,
    free_regb: reg::RegB,
) -> Text {
    let mut offset = 0;
    let mut asm = Text::Concat(Vec::new());
    while size >= 8 {
        asm =
            asm + movq(
                addr!(offset_in + offset, reg_in),
                reg::Operand::Reg(free_regq),
            ) + movq(
                reg::Operand::Reg(free_regq),
                addr!(offset_out + offset, reg_out),
            );
        size -= 8;
        offset += 8;
    }

    while size >= 4 {
        asm = asm
            + movl(addr!(offset_in + offset, reg_in), reg!(free_regl))
            + movl(reg!(free_regl), addr!(offset_out + offset, reg_out));
        size -= 4;
        offset += 4;
    }

    while size >= 2 {
        asm = asm
            + movw(addr!(offset_in + offset, reg_in), reg!(free_regw))
            + movw(reg!(free_regw), addr!(offset_out + offset, reg_out));
        size -= 2;
        offset += 2;
    }

    while size >= 1 {
        asm = asm
            + movb(addr!(offset_in + offset, reg_in), reg!(free_regb))
            + movb(reg!(free_regb), addr!(offset_out + offset, reg_out));
        size -= 1;
        offset += 1;
    }
    asm
}

fn compile_expr_pointer(
    ctxt: &mut context::Context,
    expr: llr::Expr,
    stack_offset: u64,
    is_main: bool,
) -> Text {
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
        | llr::ExprInner::UnaOp(_, _) => panic!("ICE"),
        llr::ExprInner::Deref(expr) => {
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
            match loc {
                Location::Rax | Location::Never => expr,
                Location::StackWithPadding(pad) => {
                    expr + popq(RAX) + addq(immq(pad as i64), reg!(RSP))
                }
            }
        }
        llr::ExprInner::Constant(_str) | llr::ExprInner::VarGlobal(_str) => {
            todo!()
        }
        llr::ExprInner::Proj(expr, proj) => {
            let expr = if expr.typed.is_ref() {
                compile_expr_val(ctxt, expr, stack_offset, is_main).1
            } else {
                compile_expr_pointer(ctxt, expr, stack_offset, is_main)
            };
            expr + addq(immq(proj as i64), reg!(RAX))
        }
        llr::ExprInner::VarId(id) => leaq(addr!(ctxt.find(id), RBP), RAX),
    }
}

fn get_cond(op: TypedBinop) -> Option<instr::Cond> {
    match op {
        TypedBinop::And(_)
        | TypedBinop::Or(_)
        | TypedBinop::Add(_)
        | TypedBinop::Sub(_)
        | TypedBinop::Mod(_, _)
        | TypedBinop::Mul(_, _)
        | TypedBinop::Div(_, _) => None,

        TypedBinop::Eq(_) => Some(instr::Cond::Z),
        TypedBinop::Neq(_) => Some(instr::Cond::NZ),
        TypedBinop::Lower(false, _) => Some(instr::Cond::B),
        TypedBinop::Lower(true, _) => Some(instr::Cond::L),
        TypedBinop::LowerEq(false, _) => Some(instr::Cond::BE),
        TypedBinop::LowerEq(true, _) => Some(instr::Cond::LE),
        TypedBinop::Greater(false, _) => Some(instr::Cond::A),
        TypedBinop::Greater(true, _) => Some(instr::Cond::G),
        TypedBinop::GreaterEq(false, _) => Some(instr::Cond::AE),
        TypedBinop::GreaterEq(true, _) => Some(instr::Cond::GE),
    }
}

fn compile_expr_val(
    ctxt: &mut context::Context,
    expr: llr::Expr,
    stack_offset: u64,
    is_main: bool,
) -> (Location, Text) {
    let size = expr.size;
    match *expr.content {
        llr::ExprInner::UnaOp(op, expr) => {
            let size = expr.size;
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
            let expr = match loc {
                Location::Never | Location::Rax => expr,
                Location::StackWithPadding(pad) => {
                    expr + match size {
                        1 => {
                            movb(addr!(RSP), reg!(AH))
                                + addq(immq(pad as i64 + size as i64), reg!(RSP))
                        }
                        2 => {
                            movw(addr!(RSP), reg!(AX))
                                + addq(immq(pad as i64 + size as i64), reg!(RSP))
                        }
                        4 => {
                            movl(addr!(RSP), reg!(EAX))
                                + addq(immq(pad as i64 + size as i64), reg!(RSP))
                        }
                        8 => popq(RAX) + addq(immq(pad as i64), reg!(RSP)),
                        _ => panic!("ICE"),
                    }
                }
            };
            let op = match op {
                TypedUnaop::Neg(Sizes::S8) => negb(reg!(AH)),
                TypedUnaop::Neg(Sizes::S16) => negw(reg!(AX)),
                TypedUnaop::Neg(Sizes::S32) => negl(reg!(EAX)),
                TypedUnaop::Neg(Sizes::S64) => negq(reg!(RAX)),
                TypedUnaop::Neg(Sizes::SUsize) => negq(reg!(RAX)),
                TypedUnaop::Not(Sizes::S8) => xorb(immb(1), reg!(AH)),
                TypedUnaop::Not(Sizes::S16) => xorw(immw(1), reg!(AX)),
                TypedUnaop::Not(Sizes::S32) => xorl(imml(1), reg!(EAX)),
                TypedUnaop::Not(Sizes::S64) => xorq(immq(1), reg!(RAX)),
                TypedUnaop::Not(Sizes::SUsize) => xorq(immq(1), reg!(RAX)),
            };
            (Location::Rax, expr + op)
        }

        llr::ExprInner::BinOp(op, expr1, expr2) => {
            println!("{:?} {:?} {:?}", op, expr1, expr2);
            let size = expr2.size;
            let (loc, expr2) = compile_expr_val(ctxt, expr2, stack_offset, is_main);
            println!("{:?}", loc);
            let expr2 = match loc {
                Location::Never => expr2,
                Location::Rax => match size {
                    0 => expr2,
                    1 => expr2 + subq(immq(1), reg!(RSP)) + movb(reg!(AH), addr!(RSP)),
                    2 => expr2 + subq(immq(2), reg!(RSP)) + movw(reg!(AX), addr!(RSP)),
                    4 => expr2 + subq(immq(4), reg!(RSP)) + movl(reg!(EAX), addr!(RSP)),
                    8 => expr2 + pushq(reg!(RAX)),
                    _ => panic!("ICE"),
                },
                Location::StackWithPadding(pad) => match size {
                    0 => expr2,
                    1 => {
                        expr2
                            + movb(addr!(RSP), reg!(AH))
                            + addq(immq(pad as i64), reg!(RSP))
                            + movb(reg!(AH), addr!(RSP))
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
            let (loc, expr1) = compile_expr_val(ctxt, expr1, stack_offset + size as u64, is_main);
            println!("{:?}", loc);
            let expr1 = match loc {
                Location::Rax | Location::Never => expr1,
                Location::StackWithPadding(pad) => {
                    expr1
                        + match size {
                            0 => nop(),
                            1 => movb(addr!(RSP), reg!(AH)) + addq(immq(1 + pad as i64), reg!(RSP)),
                            2 => movw(addr!(RSP), reg!(AX)) + addq(immq(2 + pad as i64), reg!(RSP)),
                            4 => {
                                movl(addr!(RSP), reg!(EAX)) + addq(immq(4 + pad as i64), reg!(RSP))
                            }
                            8 => popq(RAX) + addq(immq(pad as i64), reg!(RSP)),
                            _ => panic!("No handled"),
                        }
                }
            };
            let mov = match size {
                0 => nop(),
                1 => movb(addr!(RSP), reg!(CH)) + addq(immq(1), reg!(RSP)),
                2 => movw(addr!(RSP), reg!(CX)) + addq(immq(2), reg!(RSP)),
                4 => movl(addr!(RSP), reg!(ECX)) + addq(immq(4), reg!(RSP)),
                8 => popq(RCX),
                _ => todo!(),
            };
            let op = match op {
                TypedBinop::Add(Sizes::S8) => addb(reg!(CH), reg!(AH)),
                TypedBinop::Add(Sizes::S16) => addw(reg!(CX), reg!(AX)),
                TypedBinop::Add(Sizes::S32) => addl(reg!(ECX), reg!(EAX)),
                TypedBinop::Add(Sizes::S64) | TypedBinop::Add(Sizes::SUsize) => {
                    addq(reg!(RCX), reg!(RAX))
                }

                TypedBinop::Sub(Sizes::S8) => subb(reg!(CH), reg!(AH)),
                TypedBinop::Sub(Sizes::S16) => subw(reg!(CX), reg!(AX)),
                TypedBinop::Sub(Sizes::S32) => subl(reg!(ECX), reg!(EAX)),
                TypedBinop::Sub(Sizes::S64) | TypedBinop::Sub(Sizes::SUsize) => {
                    subq(reg!(RCX), reg!(RAX))
                }

                TypedBinop::Mul(true, Sizes::S8) => todo!(),
                TypedBinop::Mul(true, Sizes::S32) => imull(reg!(ECX), reg!(EAX)),
                TypedBinop::Mul(true, Sizes::S64) | TypedBinop::Mul(true, Sizes::SUsize) => {
                    imulq(reg!(RCX), reg!(RAX))
                }

                TypedBinop::Div(_, Sizes::S8) => todo!(),
                TypedBinop::Div(_, Sizes::S16) => todo!(),
                TypedBinop::Div(b, Sizes::S32) => {
                    testl(reg!(ECX), reg!(ECX))
                        + jz(reg::Label::panic())
                        + if b {
                            cltd() + idivl(reg!(ECX))
                        } else {
                            xorl(reg!(EDX), reg!(EDX)) + divl(reg!(ECX))
                        }
                }
                TypedBinop::Div(b, Sizes::S64) | TypedBinop::Div(b, Sizes::SUsize) => {
                    testq(reg!(RCX), reg!(RCX))
                        + jz(reg::Label::panic())
                        + if b {
                            cqto() + idivq(reg!(RCX))
                        } else {
                            xorq(reg!(RDX), reg!(RDX)) + divq(reg!(RCX))
                        }
                }

                TypedBinop::Mod(_, Sizes::S8) => todo!(),
                TypedBinop::Mod(_, Sizes::S16) => todo!(),
                TypedBinop::Mod(b, Sizes::S32) => {
                    testl(reg!(ECX), reg!(ECX))
                        + jz(reg::Label::panic())
                        + if b {
                            cltd() + idivl(reg!(ECX))
                        } else {
                            xorl(reg!(EDX), reg!(EDX)) + divl(reg!(ECX))
                        }
                        + movl(reg!(EDX), reg!(EAX))
                }

                TypedBinop::Mod(b, Sizes::S64) | TypedBinop::Mod(b, Sizes::SUsize) => {
                    testq(reg!(RCX), reg!(RCX))
                        + jz(reg::Label::panic())
                        + if b {
                            cqto() + idivq(reg!(RCX))
                        } else {
                            xorq(reg!(RDX), reg!(RDX)) + divq(reg!(RCX))
                        }
                        + movq(reg!(RDX), reg!(RAX))
                }

                TypedBinop::And(Sizes::S8) => andb(reg!(CH), reg!(AH)),
                TypedBinop::And(Sizes::S16) => andw(reg!(CX), reg!(AX)),
                TypedBinop::And(Sizes::S32) => andl(reg!(ECX), reg!(EAX)),
                TypedBinop::And(Sizes::S64) | TypedBinop::And(Sizes::SUsize) => {
                    andq(reg!(RCX), reg!(RAX))
                }

                TypedBinop::Or(Sizes::S8) => orb(reg!(CH), reg!(AH)),
                TypedBinop::Or(Sizes::S16) => orw(reg!(CX), reg!(AX)),
                TypedBinop::Or(Sizes::S32) => orl(reg!(ECX), reg!(EAX)),
                TypedBinop::Or(Sizes::S64) | TypedBinop::Or(Sizes::SUsize) => {
                    orq(reg!(RCX), reg!(RAX))
                }

                TypedBinop::Eq(Sizes::S8)
                | TypedBinop::Neq(Sizes::S8)
                | TypedBinop::Lower(_, Sizes::S8)
                | TypedBinop::LowerEq(_, Sizes::S8)
                | TypedBinop::Greater(_, Sizes::S8)
                | TypedBinop::GreaterEq(_, Sizes::S8) => {
                    cmpb(reg!(CH), reg!(AH)) + set(get_cond(op).unwrap(), reg!(AH))
                }
                TypedBinop::Eq(Sizes::S16)
                | TypedBinop::Neq(Sizes::S16)
                | TypedBinop::Lower(_, Sizes::S16)
                | TypedBinop::LowerEq(_, Sizes::S16)
                | TypedBinop::Greater(_, Sizes::S16)
                | TypedBinop::GreaterEq(_, Sizes::S16) => {
                    cmpw(reg!(CX), reg!(AX)) + set(get_cond(op).unwrap(), reg!(AH))
                }

                TypedBinop::Eq(Sizes::S32)
                | TypedBinop::Neq(Sizes::S32)
                | TypedBinop::Lower(_, Sizes::S32)
                | TypedBinop::LowerEq(_, Sizes::S32)
                | TypedBinop::Greater(_, Sizes::S32)
                | TypedBinop::GreaterEq(_, Sizes::S32) => {
                    cmpl(reg!(ECX), reg!(EAX)) + set(get_cond(op).unwrap(), reg!(AH))
                }

                TypedBinop::Eq(Sizes::S64)
                | TypedBinop::Neq(Sizes::S64)
                | TypedBinop::Lower(_, Sizes::S64)
                | TypedBinop::LowerEq(_, Sizes::S64)
                | TypedBinop::Greater(_, Sizes::S64)
                | TypedBinop::GreaterEq(_, Sizes::S64) => {
                    cmpq(reg!(RCX), reg!(RAX)) + set(get_cond(op).unwrap(), reg!(AH))
                }

                TypedBinop::Eq(Sizes::SUsize)
                | TypedBinop::Neq(Sizes::SUsize)
                | TypedBinop::Lower(_, Sizes::SUsize)
                | TypedBinop::LowerEq(_, Sizes::SUsize)
                | TypedBinop::Greater(_, Sizes::SUsize)
                | TypedBinop::GreaterEq(_, Sizes::SUsize) => {
                    cmpq(reg!(RCX), reg!(RAX)) + set(get_cond(op).unwrap(), reg!(AH))
                }

                op => {
                    println!("{:?}", op);
                    todo!()
                }
            };
            (Location::Rax, expr2 + expr1 + mov + op)
        }

        llr::ExprInner::Bloc(bloc) => compile_bloc(ctxt, bloc, stack_offset, is_main),
        llr::ExprInner::Bool(b) => {
            if b {
                (Location::Rax, movb(immb(1), reg::Operand::Reg(AH)))
            } else {
                (Location::Rax, movb(immb(0), reg::Operand::Reg(AH)))
            }
        }
        llr::ExprInner::BuildStruct(struct_size, exprs) => {
            let mut asm = subq(immq(struct_size as i64), reg::Operand::Reg(RSP));
            for (offset, expr) in exprs {
                let size = expr.size;
                let (loc, expr) =
                    compile_expr_val(ctxt, expr, stack_offset + struct_size as u64, is_main);
                asm = asm + expr;
                asm = asm
                    + match loc {
                        Location::Rax => {
                            if size == 1 {
                                movb(
                                    reg::Operand::Reg(AH),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        RBP
                                    ),
                                )
                            } else if size == 2 {
                                movw(
                                    reg::Operand::Reg(AX),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        RBP
                                    ),
                                )
                            } else if size == 4 {
                                movl(
                                    reg::Operand::Reg(EAX),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        RBP
                                    ),
                                )
                            } else if size == 8 {
                                movq(
                                    reg::Operand::Reg(RAX),
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
                                0,
                                RBP,
                                -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                size as u64,
                                RAX,
                                EAX,
                                AX,
                                AH,
                            ) + addq(immq(pad as i64 + size as i64), reg!(RSP))
                        }
                    }
            }
            (Location::StackWithPadding(0), asm)
        }
        llr::ExprInner::Constant(_) => todo!(),
        llr::ExprInner::Deref(expr) => {
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
            let expr = match loc {
                Location::Never | Location::Rax => expr,
                Location::StackWithPadding(pad) => {
                    expr + popq(RAX) + addq(immq(pad as i64), reg!(RSP))
                }
            };

            (
                Location::StackWithPadding(0),
                expr + subq(immq(size as i64), reg!(RSP))
                    + mov_struct(RAX, 0, RSP, 0, size as u64, RCX, ECX, CX, CH),
            )
        }
        llr::ExprInner::FunCall(name, args) => {
            //            println!("{} {:?}", name, args);
            let label = ctxt.fun_label(&name);
            let mut total_size = 0;
            for arg in &args {
                total_size += arg.size as u64
            }
            let offset = (total_size + expr.size as u64 + stack_offset) % 16;
            let missing = if offset == 0 { 0 } else { 16 - offset };
            let mut asm = subq(
                immq(missing as i64 + expr.size as i64 + total_size as i64),
                reg::Operand::Reg(RSP),
            );
            //            println!("{} {} {} {} {}", stack_offset, offset, missing, total_size, expr.size);
            let mut current_offset = stack_offset + missing + expr.size as u64;
            let stack_offset = stack_offset + missing + total_size + expr.size as u64;
            //            println!("{} {}", current_offset, stack_offset);
            for arg in args {
                let size = arg.size;
                current_offset += size as u64;
                let (loc, arg) = compile_expr_val(ctxt, arg, stack_offset, is_main);
                let asm2 = match loc {
                    Location::Rax => match size {
                        1 => movb(reg::Operand::Reg(AH), addr!(-(current_offset as i64), RBP)),
                        2 => movw(reg::Operand::Reg(AX), addr!(-(current_offset as i64), RBP)),
                        4 => movl(reg::Operand::Reg(EAX), addr!(-(current_offset as i64), RBP)),
                        8 => movq(reg::Operand::Reg(RAX), addr!(-(current_offset as i64), RBP)),
                        _ => panic!("ICE"),
                    },
                    Location::Never => panic!("ICE"),
                    Location::StackWithPadding(pad) => {
                        mov_struct(
                            RSP,
                            0,
                            RBP,
                            -(current_offset as i64),
                            size as u64,
                            RAX,
                            EAX,
                            AX,
                            AH,
                        ) + addq(immq(pad as i64 + size as i64), reg!(RSP))
                    }
                };
                //                println!("{current_offset} {size}");
                asm = asm + arg + asm2;
            }
            assert_eq!(current_offset, stack_offset);
            asm = asm + call(label) + addq(immq(total_size as i64), reg::Operand::Reg(RSP));
            (Location::StackWithPadding(missing), asm)
        }
        llr::ExprInner::FunCallVar(_, _) => todo!(),
        llr::ExprInner::If(expr, bloc1, bloc2) => {
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
            let expr = match loc {
                Location::Never => todo!(),
                Location::Rax => expr,
                Location::StackWithPadding(pad) => {
                    expr + movb(addr!(0, RSP), reg::Operand::Reg(AH))
                        + addq(immq(pad as i64 + 1), reg!(RSP))
                }
            };
            let (loc1, bloc1) = compile_bloc(ctxt, bloc1, stack_offset, is_main);
            let (loc2, bloc2) = compile_bloc(ctxt, bloc2, stack_offset, is_main);
            let (else_label, end_label) = ctxt.gen_if_labels();
            let (loc, bloc1, bloc2) = match (loc1, loc2) {
                (Location::Never, loc) => (loc, bloc1, bloc2),
                (loc, Location::Never) => (loc, bloc1, bloc2),
                (Location::Rax, Location::Rax) => (Location::Rax, bloc1, bloc2),
                (Location::Rax, Location::StackWithPadding(pad)) => {
                    let mov = match size {
                        0 => Text::Concat(Vec::new()),
                        1 => movb(addr!(RSP), reg::Operand::Reg(AH)),
                        2 => movw(addr!(RSP), reg::Operand::Reg(AX)),
                        4 => movl(addr!(RSP), reg::Operand::Reg(EAX)),
                        8 => movq(addr!(RSP), reg::Operand::Reg(RAX)),
                        _ => panic!("ICE"),
                    };
                    (
                        Location::Rax,
                        bloc1,
                        bloc2 + mov + addq(immq(pad as i64 + size as i64), reg!(RSP)),
                    )
                }
                (Location::StackWithPadding(pad), Location::Rax) => {
                    let mov = match size {
                        0 => Text::Concat(Vec::new()),
                        1 => movb(addr!(RSP), reg::Operand::Reg(AH)),
                        2 => movw(addr!(RSP), reg::Operand::Reg(AX)),
                        4 => movl(addr!(RSP), reg::Operand::Reg(EAX)),
                        8 => movq(addr!(RSP), reg::Operand::Reg(RAX)),
                        _ => panic!("ICE move of size {size}"),
                    };
                    (
                        Location::Rax,
                        bloc1 + mov + addq(immq(pad as i64 + size as i64), reg!(RSP)),
                        bloc2,
                    )
                }
                (Location::StackWithPadding(pad1), Location::StackWithPadding(pad2))
                    if pad1 == pad2 =>
                {
                    (Location::StackWithPadding(pad1), bloc1, bloc2)
                }
                (Location::StackWithPadding(pad1), Location::StackWithPadding(pad2)) => todo!(),
            };
            (
                loc,
                expr
                + testb(reg::Operand::Reg(AH), reg::Operand::Reg(AH))
                + jz(else_label.clone())
                + bloc1
//                + todo!()
                + jmp(end_label.clone())
                + label(else_label)
                + bloc2
//                + todo!()
                + label(end_label),
            )
        }
        llr::ExprInner::Int(i, size) => (
            Location::Rax,
            match size {
                Sizes::S8 => movb(immb(i as i8), reg::Operand::Reg(AH)),
                Sizes::S16 => movw(immw(i as i16), reg::Operand::Reg(AX)),
                Sizes::S32 => movl(imml(i as i32), reg::Operand::Reg(EAX)),
                Sizes::S64 | Sizes::SUsize => movq(immq(i as i64), reg!(RAX)),
            },
        ),
        llr::ExprInner::Print(label_name) => {
            let missing = if stack_offset % 16 == 0 {
                0
            } else {
                16 - (stack_offset % 16) as i64
            };
            (
                Location::Rax,
                deplq(reg::Label::from_str(label_name), RDI)
                    + movq(immq(0), reg!(RAX))
                    + subq(immq(missing), reg!(RSP))
                    + call(reg::Label::printf())
                    + addq(immq(missing), reg!(RSP)),
            )
        }
        llr::ExprInner::Proj(sub_expr, offset) => {
            let sub_expr = if sub_expr.typed.is_ref() {
                compile_expr_val(ctxt, sub_expr, stack_offset, is_main).1
            } else {
                compile_expr_pointer(ctxt, sub_expr, stack_offset, is_main)
            };
            match expr.size {
                1 => (
                    Location::Rax,
                    sub_expr + movb(addr!(offset as i64, RAX), reg::Operand::Reg(AH)),
                ),
                2 => (
                    Location::Rax,
                    sub_expr + movw(addr!(offset as i64, RAX), reg::Operand::Reg(AX)),
                ),
                4 => (
                    Location::Rax,
                    sub_expr + movl(addr!(offset as i64, RAX), reg::Operand::Reg(EAX)),
                ),
                8 => (
                    Location::Rax,
                    sub_expr + movq(addr!(offset as i64, RAX), reg::Operand::Reg(RAX)),
                ),
                size => (
                    Location::StackWithPadding(0),
                    sub_expr
                        + subq(immq(size as i64), reg::Operand::Reg(RSP))
                        + mov_struct(RAX, offset as i64, RSP, 0, size as u64, RCX, ECX, CX, CH),
                ),
            }
        }
        llr::ExprInner::Ref(expr) => (
            Location::Rax,
            compile_expr_pointer(ctxt, expr, stack_offset, is_main),
        ),
        llr::ExprInner::Set(size, addr, expr) if addr.is_ref_var() => {
            assert_eq!(size, expr.size);
            let var_id = addr.get_ref_var().unwrap();
            let offset_from_rbp = ctxt.find(var_id);
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
            let asm = match loc {
                Location::Never => expr,
                Location::Rax => match size {
                    0 => expr,
                    1 => (expr + movb(reg!(AH), addr!(offset_from_rbp, RBP))),
                    2 => (expr + movw(reg!(AX), addr!(offset_from_rbp, RBP))),
                    4 => (expr + movl(reg!(EAX), addr!(offset_from_rbp, RBP))),
                    8 => (expr + movq(reg!(RAX), addr!(offset_from_rbp, RBP))),
                    _ => panic!("ICE"),
                },
                Location::StackWithPadding(pad) => {
                    expr + mov_struct(RSP, 0, RBP, offset_from_rbp, size as u64, RAX, EAX, AX, AH)
                        + addq(immq(pad as i64 + size as i64), reg!(RSP))
                }
            };
            (Location::Rax, asm)
        }
        llr::ExprInner::Set(size, addr, expr) => {
            assert_eq!(size, expr.size);
            let (loc, addr) = compile_expr_val(ctxt, addr, stack_offset, is_main);
            let addr = match loc {
                Location::Rax | Location::Never => addr,
                Location::StackWithPadding(pad) => {
                    addr + popq(RAX) + addq(immq(pad as i64), reg!(RSP))
                }
            };
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset + 8, is_main);
            let mut asm = addr + pushq(reg::Operand::Reg(RAX)) + expr;
            asm = asm
                + match loc {
                    Location::Rax => {
                        popq(RCX)
                            + if size == 1 {
                                movb(reg::Operand::Reg(AH), addr!(RCX))
                            } else if size == 2 {
                                movw(reg::Operand::Reg(AX), addr!(RCX))
                            } else if size == 4 {
                                movl(reg::Operand::Reg(EAX), addr!(RCX))
                            } else if size == 8 {
                                movq(reg::Operand::Reg(RAX), addr!(RCX))
                            } else {
                                panic!("ICE")
                            }
                    }
                    Location::Never => nop(),
                    Location::StackWithPadding(pad) => {
                        movq(addr!(-(stack_offset as i64 + 8), RBP), reg!(RCX))
                            + mov_struct(RSP, 0, RCX, 0, size as u64, RAX, EAX, AX, AH)
                            + addq(immq(pad as i64 + size as i64 + 8), reg!(RSP))
                    }
                    _ => todo!(),
                };
            (Location::Rax, asm)
        }
        llr::ExprInner::Tuple(tuple_size, exprs) => {
            let mut asm = subq(immq(tuple_size as i64), reg!(RSP));
            let mut current_offset = 0;
            for expr in exprs {
                let size = expr.size;
                let (loc, expr) =
                    compile_expr_val(ctxt, expr, stack_offset + tuple_size as u64, is_main);
                asm = asm + expr;
                asm = asm
                    + match loc {
                        Location::Never => nop(),
                        Location::Rax => {
                            if size == 0 {
                                nop()
                            } else if size == 1 {
                                movb(reg::Operand::Reg(AH), addr!(current_offset, RSP))
                            } else if size == 4 {
                                movl(reg::Operand::Reg(EAX), addr!(current_offset, RSP))
                            } else if size == 8 {
                                movq(reg::Operand::Reg(RAX), addr!(current_offset, RSP))
                            } else {
                                panic!("ICE")
                            }
                        }
                        Location::StackWithPadding(pad) => {
                            mov_struct(
                                RSP,
                                0,
                                RSP,
                                size as i64 + pad as i64,
                                size as u64,
                                RAX,
                                EAX,
                                AX,
                                AH,
                            ) + addq(immq(pad as i64 + size as i64), reg!(RSP))
                        }
                    };
                current_offset += size as i64;
            }
            (Location::StackWithPadding(0), asm)
        }
        llr::ExprInner::VarGlobal(_) => todo!(),
        llr::ExprInner::VarId(id) => {
            let offset_from_rbp = ctxt.find(id);
            if expr.size == 1 {
                (
                    Location::Rax,
                    movb(addr!(offset_from_rbp, RBP), reg::Operand::Reg(AH)),
                )
            } else if expr.size == 2 {
                (
                    Location::Rax,
                    movw(addr!(offset_from_rbp, RBP), reg::Operand::Reg(AX)),
                )
            } else if expr.size == 4 {
                (
                    Location::Rax,
                    movl(addr!(offset_from_rbp, RBP), reg::Operand::Reg(EAX)),
                )
            } else if expr.size == 8 {
                (
                    Location::Rax,
                    movq(addr!(offset_from_rbp, RBP), reg::Operand::Reg(RAX)),
                )
            } else {
                let size = expr.size;
                (
                    Location::StackWithPadding(0),
                    subq(immq(size as i64), reg!(RSP))
                        + mov_struct(RBP, offset_from_rbp, RSP, 0, size as u64, RAX, EAX, AX, AH),
                )
            }
        }
    }
}

fn compile_bloc(
    ctxt: &mut context::Context,
    bloc: llr::Bloc,
    mut stack_offset: u64,
    is_main: bool,
) -> (Location, Text) {
    let initial_stack_offset = stack_offset;
    ctxt.add_layer();
    for instr in &bloc.content {
        match instr {
            llr::Instr::Binding(id, expr) => {
                stack_offset = ctxt.insert(*id, expr.size, stack_offset)
            }
            _ => (),
        }
    }
    while stack_offset % 16 != 0 {
        stack_offset += 1
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
                asm = asm + compile_expr_val(ctxt, expr, stack_offset, is_main).1;
                last_loc = Location::Rax;
            }
            llr::Instr::While(expr, bloc) => {
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
                assert_eq!(matches!(loc, Location::StackWithPadding(_)), false);
                let (loc2, bloc) = compile_bloc(ctxt, bloc, stack_offset, is_main);
                let bloc = match loc2 {
                    Location::StackWithPadding(_) => todo!(),
                    Location::Never | Location::Rax => bloc,
                };
                let (in_label, out_label) = ctxt.gen_while_labels();
                asm = asm
                    + label(in_label.clone())
                    + expr
                    + testb(reg!(AH), reg!(AH))
                    + jz(out_label.clone())
                    + bloc
                    + jmp(in_label)
                    + label(out_label);
                last_loc = Location::Rax;
            }
            llr::Instr::Return(None) => {
                last_loc = Location::Never;
                asm = asm
                    + xorq(reg!(RAX), reg!(RAX))
                    + movq(reg!(RBP), reg!(RSP))
                    + if is_main {
                        popq(R13) + popq(R12)
                    } else {
                        nop()
                    }
                    + popq(RBP)
                    + ret();
            }
            llr::Instr::Return(Some(expr)) => {
                let size = expr.size as u64;
                last_loc = Location::Never;
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
                asm = asm
                    + expr
                    + match loc {
                        Location::Rax => match size {
                            1 => movb(reg::Operand::Reg(AH), addr!(ctxt.get_return_offset(), RBP)),
                            2 => movw(reg::Operand::Reg(AX), addr!(ctxt.get_return_offset(), RBP)),
                            4 => movl(reg::Operand::Reg(EAX), addr!(ctxt.get_return_offset(), RBP)),
                            8 => movq(reg::Operand::Reg(RAX), addr!(ctxt.get_return_offset(), RBP)),
                            _ => panic!("ICE"),
                        },
                        Location::Never => todo!(),
                        Location::StackWithPadding(_) => mov_struct(
                            RSP,
                            0,
                            RBP,
                            ctxt.get_return_offset(),
                            size,
                            RAX,
                            EAX,
                            AX,
                            AH,
                        ),
                    }
                    + movq(reg!(RBP), reg!(RSP))
                    + popq(RBP)
                    + ret();
            }
            llr::Instr::Expr(ComputedValue::Drop, expr) => {
                let size = expr.size;
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
                asm = asm
                    + match loc {
                        Location::StackWithPadding(pad) => {
                            expr + addq(immq(size as i64 + pad as i64), reg!(RSP))
                        }
                        _ => expr,
                    };
                last_loc = Location::Rax;
            }

            llr::Instr::Expr(ComputedValue::Keep, expr) => {
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
                last_loc = loc;
                asm = asm + expr;
            }
        }
    }
    match last_loc {
        Location::StackWithPadding(pad) => {
            last_loc = Location::StackWithPadding(pad + stack_offset - initial_stack_offset)
        }
        Location::Never => (),
        Location::Rax => {
            asm = asm
                + addq(
                    immq((stack_offset - initial_stack_offset) as i64),
                    reg!(RSP),
                )
        }
    }
    ctxt.drop_layer();
    (last_loc, asm)
}

fn default_vec_function(ctxt: &context::Context) -> Text {
    label(ctxt.fun_label("print_ptr"))
        + pushq(reg!(RBP))
        + movq(reg!(RSP), reg!(RBP))
        + movq(addr!(16, RBP), reg!(RSI))
        + deplq(reg::Label::from_str("my_string".to_string()), RDI)
        + call(reg::Label::printf())
        + movq(reg!(RBP), reg!(RSP))
        + popq(RBP)
        + ret()

/*
A vector is a pointer to a tuple of 4 elements in the stack :
- Pointer
- Length
- Capacity
- Size_of_elements
*/
    + label(ctxt.fun_label("std::vec::Vec::new"))
        + pushq(reg!(RBP)) /* bit align for malloc */
        + movq(immq(32), reg!(RDI)) /* Allocate chunk for 4 numbers */
        + call(reg::Label::malloc())
        + movq(reg!(RAX), addr!(24, RSP)) /* Store the pointer to return it */
        + movq(reg!(RAX), reg!(RBP)) /* Store the pointer also in Rbp */
        + movq(addr!(16, RSP), reg!(RDI)) /* Get size of elements */
        + movq(reg!(RDI), addr!(24, RBP)) /* Initialize quadri-vector with a capacity of 1 */
        + call(reg::Label::malloc())
        + movq(reg!(RAX), addr!(RBP))
        + movq(immq(0), reg!(RAX))
        + movq(reg!(RAX), addr!(8, RBP))
        + movq(immq(1), reg!(RAX))
        + movq(reg!(RAX), addr!(16, RBP))
        + popq(RBP) /* returns */
        + ret()
    + label(ctxt.fun_label("std::vec::Vec::len"))
        + movq(addr!(8, RSP), reg!(RAX)) /* get pointer to vec */
        + movq(addr!(RAX), reg!(RAX))    /* get pointer to 4-vector */
        + movq(addr!(8, RAX), reg!(RAX)) /* put length in Rax */
        + movq(reg!(RAX), addr!(16, RSP)) /* Store the result in stack */
        + ret()
    + label(ctxt.fun_label("std::vec::Vec::get"))
        + movq(addr!(16, RSP), reg!(RCX)) /* get pointer to vec */
        + movq(addr!(RCX), reg!(RCX)) /* get pointer to 4-vector */
        + movq(addr!(8, RSP), reg!(RAX)) /* put index in Rax */
        + movq(addr!(8, RCX), reg!(RDX))
        + cmpq(reg!(RDX), reg!(RAX))
        + jae(reg::Label::panic())
        + imulq(addr!(24, RCX), reg!(RAX)) /* multiply by size of elements */
        + addq(addr!(RCX), reg!(RAX)) /* add base of vector to offset */
        + movq(reg!(RAX), addr!(24, RSP)) /* store result */
        + ret()

/*
Called with pointer to pointer to quadri vector as second argument
and then arg
*/
    + label(ctxt.fun_label("std::vec::Vec::push"))
        + pushq(reg!(RBP))
        + movq(addr!(16, RSP), reg!(RBP)) /* get pointer to vec */
        + movq(addr!(RBP), reg!(RBP)) /* get pointer to 4-vector */
        + movq(addr!(8, RBP), reg!(RAX)) /* get length */
        + movq(addr!(16, RBP), reg!(RSI)) /* get capacity */
        + cmpq(reg!(RAX), reg!(RSI))
        + jnz(reg::Label::from_str("push_has_capacity".to_string()))
        + addq(reg!(RSI), reg!(RSI)) /* double capacity */
        + imulq(addr!(24, RBP), reg!(RSI))
        + movq(addr!(RBP), reg!(RDI))
        + call(reg::Label::realloc())
        + movq(reg!(RAX), addr!(RBP)) // store new pointer
        + movq(addr!(8, RBP), reg!(RAX)) /* get length */
        + movq(addr!(16, RBP), reg!(RSI)) /* get capacity */
        + addq(reg!(RSI), reg!(RSI))
        + label(reg::Label::from_str("push_has_capacity".to_string()))
        //    Rbp pointer to 4-vector
        //    Rsi current capacity
        //    Rax length
        + movq(addr!(24, RBP), reg!(RDI)) /* size_of elements */
        + imulq(reg!(RDI), reg!(RAX))
        + addq(addr!(RBP), reg!(RAX)) /* target of move */
        + leaq(addr!(24, RSP), RCX) /* origin of move */
        + label(reg::Label::from_str("push_copy_while_start".to_string()))
        + testq(reg!(RDI), reg!(RDI))
        + jz(reg::Label::from_str("push_copy_while_end".to_string()))
        + movb(addr!(RCX), reg!(DH))
        + movb(reg!(DH), addr!(RAX))
        + decq(reg!(RDI))
        + incq(reg!(RAX))
        + incq(reg!(RCX))
        + jmp(reg::Label::from_str("push_copy_while_start".to_string()))
        + label(reg::Label::from_str("push_copy_while_end".to_string()))
        + incq(addr!(8, RBP))
        + popq(RBP)
        + ret()
    + label(reg::Label::panic())
        + movq(reg!(R13), reg!(RSP))
        + popq(R13)
        + popq(R12)
        + popq(RBP)
        + movq(immq(1), reg!(RAX))
        + ret()
}

fn compile_fun(fun_decl: llr::DeclFun, ctxt: &mut context::Context) -> Text {
    let is_main = fun_decl.name.get_content() == "main";
    let size = fun_decl.output;
    ctxt.init(fun_decl.args);
    let (loc, bloc) = compile_bloc(ctxt, fun_decl.content, 0, is_main);
    let mut asm = label(ctxt.fun_label(fun_decl.name.get_content()))
        + pushq(reg!(RBP))
        + if is_main {
            pushq(reg!(R12)) + pushq(reg!(R13)) + movq(reg!(RSP), reg!(R13))
        } else {
            nop()
        }
        + movq(reg!(RSP), reg!(RBP))
        + bloc;
    if is_main {
        if size == 0 {
            asm = asm + xorq(reg::Operand::Reg(RAX), reg::Operand::Reg(RAX))
        } else {
            todo!()
        }
    } else {
        match loc {
            Location::StackWithPadding(_) => {
                asm = asm
                    + mov_struct(
                        RSP,
                        0,
                        RBP,
                        ctxt.get_return_offset(),
                        size as u64,
                        RAX,
                        EAX,
                        AX,
                        AH,
                    )
            }
            Location::Rax => {
                asm = match size {
                    0 => asm,
                    1 => asm + movb(reg!(AH), addr!(ctxt.get_return_offset(), RBP)),
                    2 => asm + movw(reg!(AX), addr!(ctxt.get_return_offset(), RBP)),
                    4 => asm + movl(reg!(EAX), addr!(ctxt.get_return_offset(), RBP)),
                    8 => asm + movq(reg!(RAX), addr!(ctxt.get_return_offset(), RBP)),
                    _ => panic!("ICE"),
                }
            }
            Location::Never => (),
        }
    }
    asm + movq(reg!(RBP), reg!(RSP))
        + if is_main {
            popq(R13) + popq(R12)
        } else {
            nop()
        }
        + popq(RBP)
        + ret()
}

pub fn to_asm(file: llr::File) -> file::File {
    let mut text_ss = Text::Concat(Vec::new());
    let mut ctxt = context::Context::new();
    let mut funs = Vec::new();
    for fun_decl in file.funs {
        if fun_decl.name.get_content() == "main" {
            text_ss = text_ss + compile_fun(fun_decl, &mut ctxt)
        } else {
            funs.push(fun_decl)
        }
    }
    for fun_decl in funs {
        text_ss = text_ss + compile_fun(fun_decl, &mut ctxt)
    }

    text_ss = text_ss + default_vec_function(&ctxt);

    let data_ss = data::Data::from_strings(file.strings)
        + data::dstring("my_string".to_string(), "%zd\n".to_string());

    file::File { text_ss, data_ss }
}
