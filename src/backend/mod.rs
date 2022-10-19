use crate::ast::common::{ComputedValue, Sizes, TypedBinop, TypedUnaop};
use crate::ast::low_level_repr as llr;
use crate::ast::typed_rust::PostType;

#[macro_use]
mod asm;
use asm::*;

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
) -> Asm {
    let mut offset = 0;
    let mut asm = Asm::Concat(Vec::new());
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
        asm =
            asm + movl(
                addr!(offset_in + offset, reg_in),
                reg::Operand::Reg(free_regl),
            ) + movl(
                reg::Operand::Reg(free_regl),
                addr!(offset_out + offset, reg_out),
            );
        size -= 4;
        offset += 4;
    }

    while size >= 2 {
        asm =
            asm + movw(
                addr!(offset_in + offset, reg_in),
                reg::Operand::Reg(free_regw),
            ) + movw(
                reg::Operand::Reg(free_regw),
                addr!(offset_out + offset, reg_out),
            );
        size -= 2;
        offset += 2;
    }

    while size >= 1 {
        asm =
            asm + movb(
                addr!(offset_in + offset, reg_in),
                reg::Operand::Reg(free_regb),
            ) + movb(
                reg::Operand::Reg(free_regb),
                addr!(offset_out + offset, reg_out),
            );
        size -= 1;
        offset += 1;
    }
    asm
}

fn compile_expr_pointer(ctxt: &mut context::Context, expr: llr::Expr, stack_offset: u64, is_main: bool) -> Asm {
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
                    expr + popq(regq!(Rax)) + addq(immq(pad as i64), regq!(Rsp))
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
            expr + addq(immq(proj as i64), reg::Operand::Reg(reg::RegQ::Rax))
        }
        llr::ExprInner::VarId(id) => leaq(addr!(ctxt.find(id), reg::RegQ::Rbp), regq!(Rax)),
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

        TypedBinop::Eq(_) => Some(instr::Cond::JZ),
        TypedBinop::Neq(_) => Some(instr::Cond::JNZ),
        TypedBinop::Lower(false, _) => Some(instr::Cond::JB),
        TypedBinop::Lower(true, _) => Some(instr::Cond::JL),
        TypedBinop::LowerEq(false, _) => Some(instr::Cond::JBE),
        TypedBinop::LowerEq(true, _) => Some(instr::Cond::JLE),
        TypedBinop::Greater(false, _) => Some(instr::Cond::JA),
        TypedBinop::Greater(true, _) => Some(instr::Cond::JG),
        TypedBinop::GreaterEq(false, _) => Some(instr::Cond::JAE),
        TypedBinop::GreaterEq(true, _) => Some(instr::Cond::JGE),
    }
}

fn compile_expr_val(
    ctxt: &mut context::Context,
    expr: llr::Expr,
    stack_offset: u64,
    is_main: bool,
) -> (Location, Asm) {
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
                            movb(addr!(reg::RegQ::Rsp), regb!(Ah))
                                + addq(immq(pad as i64 + size as i64), regq!(Rsp))
                        }
                        2 => {
                            movw(addr!(reg::RegQ::Rsp), regw!(Ax))
                                + addq(immq(pad as i64 + size as i64), regq!(Rsp))
                        }
                        4 => {
                            movl(addr!(reg::RegQ::Rsp), regl!(Eax))
                                + addq(immq(pad as i64 + size as i64), regq!(Rsp))
                        }
                        8 => popq(regq!(Rax)) + addq(immq(pad as i64), regq!(Rsp)),
                        _ => panic!("ICE"),
                    }
                }
            };
            let op = match op {
                TypedUnaop::Neg(Sizes::S8) => negb(regb!(Ah)),
                TypedUnaop::Neg(Sizes::S16) => negw(regw!(Ax)),
                TypedUnaop::Neg(Sizes::S32) => negl(regl!(Eax)),
                TypedUnaop::Neg(Sizes::S64) => negq(regq!(Rax)),
                TypedUnaop::Neg(Sizes::SUsize) => negq(regq!(Rax)),
                TypedUnaop::Not(Sizes::S8) => notb(regb!(Ah)),
                TypedUnaop::Not(Sizes::S16) => notw(regw!(Ax)),
                TypedUnaop::Not(Sizes::S32) => notl(regl!(Eax)),
                TypedUnaop::Not(Sizes::S64) => notq(regq!(Rax)),
                TypedUnaop::Not(Sizes::SUsize) => notq(regq!(Rax)),
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
                    1 => expr2 + subq(immq(1), regq!(Rsp)) + movb(regb!(Ah), addr!(reg::RegQ::Rsp)),
                    2 => expr2 + subq(immq(2), regq!(Rsp)) + movw(regw!(Ax), addr!(reg::RegQ::Rsp)),
                    4 => {
                        expr2 + subq(immq(4), regq!(Rsp)) + movl(regl!(Eax), addr!(reg::RegQ::Rsp))
                    }
                    8 => expr2 + pushq(regq!(Rax)),
                    _ => panic!("ICE"),
                },
                Location::StackWithPadding(pad) => match size {
                    0 => expr2,
                    1 => {
                        expr2
                            + movb(addr!(reg::RegQ::Rsp), regb!(Ah))
                            + addq(immq(pad as i64), regq!(Rsp))
                            + movb(regb!(Ah), addr!(reg::RegQ::Rsp))
                    }
                    2 => {
                        expr2
                            + movw(addr!(reg::RegQ::Rsp), regw!(Ax))
                            + addq(immq(pad as i64), regq!(Rsp))
                            + movw(regw!(Ax), addr!(reg::RegQ::Rsp))
                    }
                    4 => {
                        expr2
                            + movl(addr!(reg::RegQ::Rsp), regl!(Eax))
                            + addq(immq(pad as i64), regq!(Rsp))
                            + movl(regl!(Eax), addr!(reg::RegQ::Rsp))
                    }
                    8 => {
                        expr2
                            + movq(addr!(reg::RegQ::Rsp), regq!(Rax))
                            + addq(immq(pad as i64), regq!(Rsp))
                            + movq(regq!(Rax), addr!(reg::RegQ::Rsp))
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
                            1 => {
                                movb(addr!(reg::RegQ::Rsp), regb!(Ah))
                                    + addq(immq(1 + pad as i64), regq!(Rsp))
                            }
                            2 => {
                                movw(addr!(reg::RegQ::Rsp), regw!(Ax))
                                    + addq(immq(2 + pad as i64), regq!(Rsp))
                            }
                            4 => {
                                movl(addr!(reg::RegQ::Rsp), regl!(Eax))
                                    + addq(immq(4 + pad as i64), regq!(Rsp))
                            }
                            8 => popq(regq!(Rax)) + addq(immq(pad as i64), regq!(Rsp)),
                            _ => panic!("No handled"),
                        }
                }
            };
            let mov = match size {
                0 => nop(),
                1 => movb(addr!(reg::RegQ::Rsp), regb!(Ch)) + addq(immq(1), regq!(Rsp)),
                2 => movw(addr!(reg::RegQ::Rsp), regw!(Cx)) + addq(immq(2), regq!(Rsp)),
                4 => movl(addr!(reg::RegQ::Rsp), regl!(Ecx)) + addq(immq(4), regq!(Rsp)),
                8 => popq(regq!(Rcx)),
                _ => todo!(),
            };
            let op = match op {
                TypedBinop::Add(Sizes::S8) => addb(regb!(Ch), regb!(Ah)),
                TypedBinop::Add(Sizes::S16) => addw(regw!(Cx), regw!(Ax)),
                TypedBinop::Add(Sizes::S32) => addl(regl!(Ecx), regl!(Eax)),
                TypedBinop::Add(Sizes::S64) | TypedBinop::Add(Sizes::SUsize) => {
                    addq(regq!(Rcx), regq!(Rax))
                }

                TypedBinop::Sub(Sizes::S8) => subb(regb!(Ch), regb!(Ah)),
                TypedBinop::Sub(Sizes::S16) => subw(regw!(Cx), regw!(Ax)),
                TypedBinop::Sub(Sizes::S32) => subl(regl!(Ecx), regl!(Eax)),
                TypedBinop::Sub(Sizes::S64) | TypedBinop::Sub(Sizes::SUsize) => {
                    subq(regq!(Rcx), regq!(Rax))
                }

                TypedBinop::Mul(true, Sizes::S8) => imulb(regb!(Ch), regb!(Ah)),
                TypedBinop::Mul(true, Sizes::S32) => imull(regl!(Ecx), regl!(Eax)),
                TypedBinop::Mul(true, Sizes::S64) | TypedBinop::Mul(true, Sizes::SUsize) => {
                    imulq(regq!(Rcx), regq!(Rax))
                }

                TypedBinop::Div(_, Sizes::S8) => todo!(),
                TypedBinop::Div(_, Sizes::S16) => todo!(),
                TypedBinop::Div(b, Sizes::S32) => {
                    testl(regl!(Ecx), regl!(Ecx))
                    + jz(reg::Label::panic())
                    + if b {
                        cltd() + idivl(regl!(Ecx))
                    } else {
                        xorl(regl!(Edx), regl!(Edx))
                        + divl(regl!(Ecx))    
                    }
                },
                TypedBinop::Div(b, Sizes::S64) | TypedBinop::Div(b, Sizes::SUsize) => {
                    testq(regq!(Rcx), regq!(Rcx))
                    + jz(reg::Label::panic())
                    + if b {
                        cqto()
                        + idivq(regq!(Rcx))
                    } else {
                        xorq(regq!(Rdx), regq!(Rdx))
                        + divq(regq!(Rcx))
                    }
                }

                TypedBinop::Mod(_, Sizes::S8) => todo!(),
                TypedBinop::Mod(_, Sizes::S16) => todo!(),
                TypedBinop::Mod(b, Sizes::S32) => {
                    testl(regl!(Ecx), regl!(Ecx))
                    + jz(reg::Label::panic())
                    + if b { cltd() + idivl(regl!(Ecx)) }
                      else { xorl(regl!(Edx), regl!(Edx)) + divl(regl!(Ecx)) }
                    + movl(regl!(Edx), regl!(Eax))
                }

                TypedBinop::Mod(b, Sizes::S64) | TypedBinop::Mod(b, Sizes::SUsize) => {
                    testq(regq!(Rcx), regq!(Rcx))
                    + jz(reg::Label::panic())
                    + if b { cqto() + idivq(regq!(Rcx)) }
                      else { xorq(regq!(Rdx), regq!(Rdx)) + divq(regq!(Rcx)) }
                    + movq(regq!(Rdx), regq!(Rax))
                }

                TypedBinop::And(Sizes::S8) => andb(regb!(Ch), regb!(Ah)),
                TypedBinop::And(Sizes::S16) => andw(regw!(Cx), regw!(Ax)),
                TypedBinop::And(Sizes::S32) => andl(regl!(Ecx), regl!(Eax)),
                TypedBinop::And(Sizes::S64) | TypedBinop::And(Sizes::SUsize) => {
                    andq(regq!(Rcx), regq!(Rax))
                }

                TypedBinop::Or(Sizes::S8) => orb(regb!(Ch), regb!(Ah)),
                TypedBinop::Or(Sizes::S16) => orw(regw!(Cx), regw!(Ax)),
                TypedBinop::Or(Sizes::S32) => orl(regl!(Ecx), regl!(Eax)),
                TypedBinop::Or(Sizes::S64) | TypedBinop::Or(Sizes::SUsize) => {
                    orq(regq!(Rcx), regq!(Rax))
                }

                TypedBinop::Eq(Sizes::S8)
                | TypedBinop::Neq(Sizes::S8)
                | TypedBinop::Lower(_, Sizes::S8)
                | TypedBinop::LowerEq(_, Sizes::S8)
                | TypedBinop::Greater(_, Sizes::S8)
                | TypedBinop::GreaterEq(_, Sizes::S8) => {
                    xorq(regq!(Rdx), regq!(Rdx))
                        + movq(immq(-1), regq!(Rdi))
                        + cmpb(regb!(Ch), regb!(Ah))
                        + cmovq(get_cond(op).unwrap(), regq!(Rdi), regq!(Rdx))
                        + movb(regb!(Dh), regb!(Ah))
                }
                TypedBinop::Eq(Sizes::S16)
                | TypedBinop::Neq(Sizes::S16)
                | TypedBinop::Lower(_, Sizes::S16)
                | TypedBinop::LowerEq(_, Sizes::S16)
                | TypedBinop::Greater(_, Sizes::S16)
                | TypedBinop::GreaterEq(_, Sizes::S16) => {
                    xorq(regq!(Rdx), regq!(Rdx))
                        + movq(immq(-1), regq!(Rdi))
                        + cmpw(regw!(Cx), regw!(Ax))
                        + cmovq(get_cond(op).unwrap(), regq!(Rdi), regq!(Rdx))
                        + movb(regb!(Dh), regb!(Ah))
                }
                
                TypedBinop::Eq(Sizes::S32)
                | TypedBinop::Neq(Sizes::S32)
                | TypedBinop::Lower(_, Sizes::S32)
                | TypedBinop::LowerEq(_, Sizes::S32)
                | TypedBinop::Greater(_, Sizes::S32)
                | TypedBinop::GreaterEq(_, Sizes::S32) => {
                    xorq(regq!(Rdx), regq!(Rdx))
                        + movq(immq(-1), regq!(Rdi))
                        + cmpl(regl!(Ecx), regl!(Eax))
                        + cmovq(get_cond(op).unwrap(), regq!(Rdi), regq!(Rdx))
                        + movb(regb!(Dh), regb!(Ah))
                }

                TypedBinop::Eq(Sizes::S64)
                | TypedBinop::Neq(Sizes::S64)
                | TypedBinop::Lower(_, Sizes::S64)
                | TypedBinop::LowerEq(_, Sizes::S64)
                | TypedBinop::Greater(_, Sizes::S64)
                | TypedBinop::GreaterEq(_, Sizes::S64) => {
                    xorq(regq!(Rdx), regq!(Rdx))
                        + movq(immq(-1), regq!(Rdi))
                        + cmpq(regq!(Rcx), regq!(Rax))
                        + cmovq(get_cond(op).unwrap(), regq!(Rdi), regq!(Rdx))
                        + movb(regb!(Dh), regb!(Ah))
                }

                TypedBinop::Eq(Sizes::SUsize)
                | TypedBinop::Neq(Sizes::SUsize)
                | TypedBinop::Lower(_, Sizes::SUsize)
                | TypedBinop::LowerEq(_, Sizes::SUsize)
                | TypedBinop::Greater(_, Sizes::SUsize)
                | TypedBinop::GreaterEq(_, Sizes::SUsize) => {
                    xorq(regq!(Rdx), regq!(Rdx))
                        + movq(immq(-1), regq!(Rdi))
                        + cmpq(regq!(Rcx), regq!(Rax))
                        + cmovq(get_cond(op).unwrap(), regq!(Rdi), regq!(Rdx))
                        + movb(regb!(Dh), regb!(Ah))
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
                (
                    Location::Rax,
                    movb(immb(-1), reg::Operand::Reg(reg::RegB::Ah)),
                )
            } else {
                (
                    Location::Rax,
                    movb(immb(0), reg::Operand::Reg(reg::RegB::Ah)),
                )
            }
        }
        llr::ExprInner::BuildStruct(struct_size, exprs) => {
            let mut asm = subq(immq(struct_size as i64), reg::Operand::Reg(reg::RegQ::Rsp));
            for (offset, expr) in exprs {
                let size = expr.size;
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset + struct_size as u64, is_main);
                asm = asm + expr;
                asm = asm
                    + match loc {
                        Location::Rax => {
                            if size == 1 {
                                movb(
                                    reg::Operand::Reg(reg::RegB::Ah),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        reg::RegQ::Rbp
                                    ),
                                )
                            } else if size == 2 {
                                movw(
                                    reg::Operand::Reg(reg::RegW::Ax),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        reg::RegQ::Rbp
                                    ),
                                )
                            } else if size == 4 {
                                movl(
                                    reg::Operand::Reg(reg::RegL::Eax),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        reg::RegQ::Rbp
                                    ),
                                )
                            } else if size == 8 {
                                movq(
                                    reg::Operand::Reg(reg::RegQ::Rax),
                                    addr!(
                                        -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                        reg::RegQ::Rbp
                                    ),
                                )
                            } else {
                                panic!("ICE")
                            }
                        }
                        Location::Never => todo!(),
                        Location::StackWithPadding(pad) => {
                            mov_struct(
                                reg::RegQ::Rsp,
                                0,
                                reg::RegQ::Rbp,
                                -(stack_offset as i64 + struct_size as i64) + offset as i64,
                                size as u64,
                                reg::RegQ::Rax,
                                reg::RegL::Eax,
                                reg::RegW::Ax,
                                reg::RegB::Ah,
                            ) + addq(immq(pad as i64 + size as i64), regq!(Rsp))
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
                    expr + popq(regq!(Rax)) + addq(immq(pad as i64), regq!(Rsp))
                }
            };

            (
                Location::StackWithPadding(0),
                expr + subq(immq(size as i64), regq!(Rsp))
                    + mov_struct(
                        reg::RegQ::Rax,
                        0,
                        reg::RegQ::Rsp,
                        0,
                        size as u64,
                        reg::RegQ::Rcx,
                        reg::RegL::Ecx,
                        reg::RegW::Cx,
                        reg::RegB::Ch,
                    ),
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
                reg::Operand::Reg(reg::RegQ::Rsp),
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
                        1 => movb(
                            reg::Operand::Reg(reg::RegB::Ah),
                            addr!(-(current_offset as i64), reg::RegQ::Rbp),
                        ),
                        2 => movw(
                            reg::Operand::Reg(reg::RegW::Ax),
                            addr!(-(current_offset as i64), reg::RegQ::Rbp),
                        ),
                        4 => movl(
                            reg::Operand::Reg(reg::RegL::Eax),
                            addr!(-(current_offset as i64), reg::RegQ::Rbp),
                        ),
                        8 => movq(
                            reg::Operand::Reg(reg::RegQ::Rax),
                            addr!(-(current_offset as i64), reg::RegQ::Rbp),
                        ),
                        _ => panic!("ICE"),
                    },
                    Location::Never => panic!("ICE"),
                    Location::StackWithPadding(pad) => {
                        mov_struct(
                            reg::RegQ::Rsp,
                            0,
                            reg::RegQ::Rbp,
                            -(current_offset as i64),
                            size as u64,
                            reg::RegQ::Rax,
                            reg::RegL::Eax,
                            reg::RegW::Ax,
                            reg::RegB::Ah,
                        ) + addq(immq(pad as i64 + size as i64), regq!(Rsp))
                    }
                };
                //                println!("{current_offset} {size}");
                asm = asm + arg + asm2;
            }
            assert_eq!(current_offset, stack_offset);
            asm = asm
                + call(label)
                + addq(immq(total_size as i64), reg::Operand::Reg(reg::RegQ::Rsp));
            (Location::StackWithPadding(missing), asm)
        }
        llr::ExprInner::FunCallVar(_, _) => todo!(),
        llr::ExprInner::If(expr, bloc1, bloc2) => {
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
            let expr = match loc {
                Location::Never => todo!(),
                Location::Rax => expr,
                Location::StackWithPadding(pad) => {
                    expr + movb(addr!(0, reg::RegQ::Rsp), reg::Operand::Reg(reg::RegB::Ah))
                        + addq(immq(pad as i64 + 1), regq!(Rsp))
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
                        0 => Asm::Concat(Vec::new()),
                        1 => movb(addr!(reg::RegQ::Rsp), reg::Operand::Reg(reg::RegB::Ah)),
                        2 => movw(addr!(reg::RegQ::Rsp), reg::Operand::Reg(reg::RegW::Ax)),
                        4 => movl(addr!(reg::RegQ::Rsp), reg::Operand::Reg(reg::RegL::Eax)),
                        8 => movq(addr!(reg::RegQ::Rsp), reg::Operand::Reg(reg::RegQ::Rax)),
                        _ => panic!("ICE"),
                    };
                    (
                        Location::Rax,
                        bloc1,
                        bloc2 + mov + addq(immq(pad as i64 + size as i64), regq!(Rsp)),
                    )
                }
                (Location::StackWithPadding(pad), Location::Rax) => {
                    let mov = match size {
                        0 => Asm::Concat(Vec::new()),
                        1 => movb(addr!(reg::RegQ::Rsp), reg::Operand::Reg(reg::RegB::Ah)),
                        2 => movw(addr!(reg::RegQ::Rsp), reg::Operand::Reg(reg::RegW::Ax)),
                        4 => movl(addr!(reg::RegQ::Rsp), reg::Operand::Reg(reg::RegL::Eax)),
                        8 => movq(addr!(reg::RegQ::Rsp), reg::Operand::Reg(reg::RegQ::Rax)),
                        _ => panic!("ICE move of size {size}"),
                    };
                    (
                        Location::Rax,
                        bloc1 + mov + addq(immq(pad as i64 + size as i64), regq!(Rsp)),
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
                + testb(reg::Operand::Reg(reg::RegB::Ah), reg::Operand::Reg(reg::RegB::Ah))
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
                Sizes::S8 => movb(immb(i as i8), reg::Operand::Reg(reg::RegB::Ah)),
                Sizes::S16 => movw(immw(i as i16), reg::Operand::Reg(reg::RegW::Ax)),
                Sizes::S32 => movl(imml(i as i32), reg::Operand::Reg(reg::RegL::Eax)),
                Sizes::S64 | Sizes::SUsize => movq(immq(i as i64), regq!(Rax)),
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
                deplq(reg::Label::from_str(label_name), regq!(Rdi))
                    + movq(immq(0), regq!(Rax))
                    + subq(immq(missing), regq!(Rsp))
                    + call(reg::Label::printf())
                    + addq(immq(missing), regq!(Rsp)),
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
                    sub_expr
                        + movb(
                            addr!(offset as i64, reg::RegQ::Rax),
                            reg::Operand::Reg(reg::RegB::Ah),
                        ),
                ),
                2 => (
                    Location::Rax,
                    sub_expr
                        + movw(
                            addr!(offset as i64, reg::RegQ::Rax),
                            reg::Operand::Reg(reg::RegW::Ax),
                        ),
                ),
                4 => (
                    Location::Rax,
                    sub_expr
                        + movl(
                            addr!(offset as i64, reg::RegQ::Rax),
                            reg::Operand::Reg(reg::RegL::Eax),
                        ),
                ),
                8 => (
                    Location::Rax,
                    sub_expr
                        + movq(
                            addr!(offset as i64, reg::RegQ::Rax),
                            reg::Operand::Reg(reg::RegQ::Rax),
                        ),
                ),
                size => (
                    Location::StackWithPadding(0),
                    sub_expr
                        + subq(immq(size as i64), reg::Operand::Reg(reg::RegQ::Rsp))
                        + mov_struct(
                            reg::RegQ::Rax,
                            offset as i64,
                            reg::RegQ::Rsp,
                            0,
                            size as u64,
                            reg::RegQ::Rcx,
                            reg::RegL::Ecx,
                            reg::RegW::Cx,
                            reg::RegB::Ch,
                        ),
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
                Location::Rax => {
                    match size {
                        0 => expr,
                        1 => (expr + movb(regb!(Ah), addr!(offset_from_rbp, reg::RegQ::Rbp))),
                        2 => (expr + movw(regw!(Ax), addr!(offset_from_rbp, reg::RegQ::Rbp))),
                        4 => (expr + movl(regl!(Eax), addr!(offset_from_rbp, reg::RegQ::Rbp))),
                        8 => (expr + movq(regq!(Rax), addr!(offset_from_rbp, reg::RegQ::Rbp))),
                        _ => panic!("ICE")
                    }
                }
                Location::StackWithPadding(pad) => {
                    expr
                    + mov_struct(reg::RegQ::Rsp, 0, reg::RegQ::Rbp, offset_from_rbp, size as u64,
                            reg::RegQ::Rax, reg::RegL::Eax, reg::RegW::Ax, reg::RegB::Ah)
                    + addq(immq(pad as i64 + size as i64), regq!(Rsp))
                },
            };
            (Location::Rax, asm)
        },
        llr::ExprInner::Set(size, addr, expr) => {
            assert_eq!(size, expr.size);
            let (loc, addr) = compile_expr_val(ctxt, addr, stack_offset, is_main);
            let addr = match loc {
                Location::Rax | Location::Never => addr,
                Location::StackWithPadding(pad) => {
                    addr + popq(regq!(Rax)) + addq(immq(pad as i64), regq!(Rsp))
                }
            };
            let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset + 8, is_main);
            let mut asm = addr + pushq(reg::Operand::Reg(reg::RegQ::Rax)) + expr;
            asm = asm
                + match loc {
                    Location::Rax => {
                        popq(regq!(Rcx))
                            + if size == 1 {
                                movb(reg::Operand::Reg(reg::RegB::Ah), addr!(reg::RegQ::Rcx))
                            } else if size == 2 {
                                movw(reg::Operand::Reg(reg::RegW::Ax), addr!(reg::RegQ::Rcx))
                            } else if size == 4 {
                                movl(reg::Operand::Reg(reg::RegL::Eax), addr!(reg::RegQ::Rcx))
                            } else if size == 8 {
                                movq(reg::Operand::Reg(reg::RegQ::Rax), addr!(reg::RegQ::Rcx))
                            } else {
                                panic!("ICE")
                            }
                    }
                    Location::Never => nop(),
                    Location::StackWithPadding(pad) => {
                        movq(
                            addr!(-(stack_offset as i64 + 8), reg::RegQ::Rbp),
                            regq!(Rcx),
                        ) + mov_struct(
                            reg::RegQ::Rsp,
                            0,
                            reg::RegQ::Rcx,
                            0,
                            size as u64,
                            reg::RegQ::Rax,
                            reg::RegL::Eax,
                            reg::RegW::Ax,
                            reg::RegB::Ah,
                        ) + addq(immq(pad as i64 + size as i64 + 8), regq!(Rsp))
                    }
                    _ => todo!(),
                };
            (Location::Rax, asm)
        }
        llr::ExprInner::Tuple(tuple_size, exprs) => {
            let mut asm = subq(immq(tuple_size as i64), regq!(Rsp));
            let mut current_offset = 0;
            for expr in exprs {
                let size = expr.size;
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset + tuple_size as u64, is_main);
                asm = asm
                    + expr
                    + movq(regq!(Rbp), regq!(Rcx))
                    + subq(
                        immq(stack_offset as i64 + current_offset as i64),
                        regq!(Rcx),
                    );
                asm = asm
                    + match loc {
                        Location::Rax => {
                            /*                            if size == 1 {
                                movb(reg::Operand::Reg(reg::RegB::Ah),
                                    reg::Operand::Addr)
                            } else if size == 4 {
                                movl(reg::Operand::Reg(reg::RegL::Eax),
                                    reg::Operand::Addr)
                            } else if size == 8 {
                                movq(reg::Operand::Reg(reg::RegQ::Rax),
                                    reg::Operand::Addr)
                            } else {
                                panic!("ICE")
                            }*/
                            todo!()
                        }
                        _ => todo!(),
                    }
            }
            (Location::StackWithPadding(0), asm)
        }
        llr::ExprInner::VarGlobal(_) => todo!(),
        llr::ExprInner::VarId(id) => {
            let offset_from_rbp = ctxt.find(id);
            if expr.size == 1 {
                (
                    Location::Rax,
                    movb(
                        addr!(offset_from_rbp, reg::RegQ::Rbp),
                        reg::Operand::Reg(reg::RegB::Ah),
                    ),
                )
            } else if expr.size == 2 {
                (
                    Location::Rax,
                    movw(
                        addr!(offset_from_rbp, reg::RegQ::Rbp),
                        reg::Operand::Reg(reg::RegW::Ax),
                    ),
                )
            } else if expr.size == 4 {
                (
                    Location::Rax,
                    movl(
                        addr!(offset_from_rbp, reg::RegQ::Rbp),
                        reg::Operand::Reg(reg::RegL::Eax),
                    ),
                )
            } else if expr.size == 8 {
                (
                    Location::Rax,
                    movq(
                        addr!(offset_from_rbp, reg::RegQ::Rbp),
                        reg::Operand::Reg(reg::RegQ::Rax),
                    ),
                )
            } else {
                let size = expr.size;
                (
                    Location::StackWithPadding(0),
                    subq(immq(size as i64), regq!(Rsp))
                        + mov_struct(
                            reg::RegQ::Rbp,
                            offset_from_rbp,
                            reg::RegQ::Rsp,
                            0,
                            size as u64,
                            reg::RegQ::Rax,
                            reg::RegL::Eax,
                            reg::RegW::Ax,
                            reg::RegB::Ah,
                        ),
                )
            }
        }
    }
}

fn compile_bloc(
    ctxt: &mut context::Context,
    bloc: llr::Bloc,
    mut stack_offset: u64,
    is_main : bool,
) -> (Location, Asm) {
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
        regq!(Rsp),
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
                    + testb(regb!(Ah), regb!(Ah))
                    + jz(out_label.clone())
                    + bloc
                    + jmp(in_label)
                    + label(out_label);
                last_loc = Location::Rax;
            }
            llr::Instr::Return(None) => {
                last_loc = Location::Never;
                asm = asm
                    + xorq(regq!(Rax), regq!(Rax))
                    + movq(regq!(Rbp), regq!(Rsp))
                    + if is_main { popq(regq!(R13)) + popq(regq!(R12)) } else { nop() }
                    + popq(regq!(Rbp))
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
                            1 => movb(
                                reg::Operand::Reg(reg::RegB::Ah),
                                addr!(ctxt.get_return_offset(), reg::RegQ::Rbp),
                            ),
                            2 => movw(
                                reg::Operand::Reg(reg::RegW::Ax),
                                addr!(ctxt.get_return_offset(), reg::RegQ::Rbp),
                            ),
                            4 => movl(
                                reg::Operand::Reg(reg::RegL::Eax),
                                addr!(ctxt.get_return_offset(), reg::RegQ::Rbp),
                            ),
                            8 => movq(
                                reg::Operand::Reg(reg::RegQ::Rax),
                                addr!(ctxt.get_return_offset(), reg::RegQ::Rbp),
                            ),
                            _ => panic!("ICE"),
                        },
                        Location::Never => todo!(),
                        Location::StackWithPadding(_) => mov_struct(
                            reg::RegQ::Rsp,
                            0,
                            reg::RegQ::Rbp,
                            ctxt.get_return_offset(),
                            size,
                            reg::RegQ::Rax,
                            reg::RegL::Eax,
                            reg::RegW::Ax,
                            reg::RegB::Ah,
                        ),
                    }
                    + movq(regq!(Rbp), regq!(Rsp))
                    + popq(regq!(Rbp))
                    + ret();
            }
            llr::Instr::Expr(ComputedValue::Drop, expr) => {
                let size = expr.size;
                let (loc, expr) = compile_expr_val(ctxt, expr, stack_offset, is_main);
                asm = asm
                    + match loc {
                        Location::StackWithPadding(pad) => {
                            expr + addq(immq(size as i64 + pad as i64), regq!(Rsp))
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
                    regq!(Rsp),
                )
        }
    }
    ctxt.drop_layer();
    (last_loc, asm)
}

fn default_vec_function(ctxt: &context::Context) -> Asm {
    label(ctxt.fun_label("print_ptr"))
        + pushq(regq!(Rbp))
        + movq(regq!(Rsp), regq!(Rbp))
        + movq(addr!(16, reg::RegQ::Rbp), regq!(Rsi))
        + deplq(reg::Label::from_str("my_string".to_string()), regq!(Rdi))
        + call(reg::Label::printf())
        + movq(regq!(Rbp), regq!(Rsp))
        + popq(regq!(Rbp))
        + ret()

/*
A vector is a pointer to a tuple of 4 elements in the stack :
- Pointer
- Length
- Capacity
- Size_of_elements
*/
    + label(ctxt.fun_label("std::vec::Vec::new"))
        + pushq(regq!(Rbp)) /* bit align for malloc */
        + movq(immq(32), regq!(Rdi)) /* Allocate chunk for 4 numbers */
        + call(reg::Label::malloc())
        + movq(regq!(Rax), addr!(24, reg::RegQ::Rsp)) /* Store the pointer to return it */
        + movq(regq!(Rax), regq!(Rbp)) /* Store the pointer also in Rbp */
        + movq(addr!(16, reg::RegQ::Rsp), regq!(Rdi)) /* Get size of elements */
        + movq(regq!(Rdi), addr!(24, reg::RegQ::Rbp)) /* Initialize quadri-vector with a capacity of 1 */
        + call(reg::Label::malloc())
        + movq(regq!(Rax), addr!(reg::RegQ::Rbp))
        + movq(immq(0), regq!(Rax))
        + movq(regq!(Rax), addr!(8, reg::RegQ::Rbp))
        + movq(immq(1), regq!(Rax))
        + movq(regq!(Rax), addr!(16, reg::RegQ::Rbp))
        + popq(regq!(Rbp)) /* returns */
        + ret()
    + label(ctxt.fun_label("std::vec::Vec::len"))
        + movq(addr!(8, reg::RegQ::Rsp), regq!(Rax)) /* get pointer to vec */
        + movq(addr!(reg::RegQ::Rax), regq!(Rax))    /* get pointer to 4-vector */
        + movq(addr!(8, reg::RegQ::Rax), regq!(Rax)) /* put length in Rax */
        + movq(regq!(Rax), addr!(16, reg::RegQ::Rsp)) /* Store the result in stack */
        + ret()
    + label(ctxt.fun_label("std::vec::Vec::get"))
        + movq(addr!(16, reg::RegQ::Rsp), regq!(Rcx)) /* get pointer to vec */
        + movq(addr!(reg::RegQ::Rcx), regq!(Rcx)) /* get pointer to 4-vector */
        + movq(addr!(8, reg::RegQ::Rsp), regq!(Rax)) /* put index in Rax */
        + movq(addr!(8, reg::RegQ::Rcx), regq!(Rdx))
        + cmpq(regq!(Rdx), regq!(Rax))
        + jae(reg::Label::panic())
        + imulq(addr!(24, reg::RegQ::Rcx), regq!(Rax)) /* multiply by size of elements */
        + addq(addr!(reg::RegQ::Rcx), regq!(Rax)) /* add base of vector to offset */
        + movq(regq!(Rax), addr!(24, reg::RegQ::Rsp)) /* store result */
        + ret()

/*
Called with pointer to pointer to quadri vector as second argument
and then arg
*/
    + label(ctxt.fun_label("std::vec::Vec::push"))
        + pushq(regq!(Rbp))
        + movq(addr!(16, reg::RegQ::Rsp), regq!(Rbp)) /* get pointer to vec */
        + movq(addr!(reg::RegQ::Rbp), regq!(Rbp)) /* get pointer to 4-vector */
        + movq(addr!(8, reg::RegQ::Rbp), regq!(Rax)) /* get length */
        + movq(addr!(16, reg::RegQ::Rbp), regq!(Rsi)) /* get capacity */
        + cmpq(regq!(Rax), regq!(Rsi))
        + jnz(reg::Label::from_str("push_has_capacity".to_string()))
        + addq(regq!(Rsi), regq!(Rsi)) /* double capacity */
        + imulq(addr!(24, reg::RegQ::Rbp), regq!(Rsi))
        + movq(addr!(reg::RegQ::Rbp), regq!(Rdi))
        + call(reg::Label::realloc())
        + movq(regq!(Rax), addr!(reg::RegQ::Rbp)) // store new pointer
        + movq(addr!(8, reg::RegQ::Rbp), regq!(Rax)) /* get length */
        + movq(addr!(16, reg::RegQ::Rbp), regq!(Rsi)) /* get capacity */
        + addq(regq!(Rsi), regq!(Rsi))
        + label(reg::Label::from_str("push_has_capacity".to_string()))
        //    Rbp pointer to 4-vector
        //    Rsi current capacity
        //    Rax length
        + movq(addr!(24, reg::RegQ::Rbp), regq!(Rdi)) /* size_of elements */
        + imulq(regq!(Rdi), regq!(Rax))
        + addq(addr!(reg::RegQ::Rbp), regq!(Rax)) /* target of move */
        + leaq(addr!(24, reg::RegQ::Rsp), regq!(Rcx)) /* origin of move */
        + label(reg::Label::from_str("push_copy_while_start".to_string()))
        + testq(regq!(Rdi), regq!(Rdi))
        + jz(reg::Label::from_str("push_copy_while_end".to_string()))
        + movb(addr!(reg::RegQ::Rcx), regb!(Dh))
        + movb(regb!(Dh), addr!(reg::RegQ::Rax))
        + decq(regq!(Rdi))
        + incq(regq!(Rax))
        + incq(regq!(Rcx))
        + jmp(reg::Label::from_str("push_copy_while_start".to_string()))
        + label(reg::Label::from_str("push_copy_while_end".to_string()))
        + incq(addr!(8, reg::RegQ::Rbp))
        + popq(regq!(Rbp))
        + ret()
    + label(reg::Label::panic())
        + movq(regq!(R13), regq!(Rsp))
        + popq(regq!(R13))
        + popq(regq!(R12))
        + popq(regq!(Rbp))
        + movq(immq(1), regq!(Rax))
        + ret()
}

fn compile_fun(fun_decl: llr::DeclFun, ctxt: &mut context::Context) -> Asm {
    let is_main = fun_decl.name.get_content() == "main";
    let size = fun_decl.output;
    ctxt.init(fun_decl.args);
    let (loc, bloc) = compile_bloc(ctxt, fun_decl.content, 0, is_main);
    let mut asm = label(ctxt.fun_label(fun_decl.name.get_content()))
        + pushq(regq!(Rbp))
        + if is_main { pushq(regq!(R12)) + pushq(regq!(R13)) + movq(regq!(Rsp), regq!(R13))} else { nop() }
        + movq(regq!(Rsp), regq!(Rbp))
        + bloc;
    if is_main {
        if size == 0 {
            asm = asm
                + xorq(
                    reg::Operand::Reg(reg::RegQ::Rax),
                    reg::Operand::Reg(reg::RegQ::Rax),
                )
        } else {
            todo!()
        }
    } else {
        match loc {
            Location::StackWithPadding(_) => {
                asm = asm
                    + mov_struct(
                        reg::RegQ::Rsp,
                        0,
                        reg::RegQ::Rbp,
                        ctxt.get_return_offset(),
                        size as u64,
                        reg::RegQ::Rax,
                        reg::RegL::Eax,
                        reg::RegW::Ax,
                        reg::RegB::Ah,
                    )
            }
            Location::Rax => {
                asm = match size {
                    0 => asm,
                    1 => asm + movb(regb!(Ah), addr!(ctxt.get_return_offset(), reg::RegQ::Rbp)),
                    2 => asm + movw(regw!(Ax), addr!(ctxt.get_return_offset(), reg::RegQ::Rbp)),
                    4 => asm + movl(regl!(Eax), addr!(ctxt.get_return_offset(), reg::RegQ::Rbp)),
                    8 => asm + movq(regq!(Rax), addr!(ctxt.get_return_offset(), reg::RegQ::Rbp)),
                    _ => panic!("ICE"),
                }
            }
            Location::Never => (),
        }
    }
    asm + movq(regq!(Rbp), regq!(Rsp))
        + if is_main { popq(regq!(R13)) + popq(regq!(R12)) } else { nop() }
        + popq(regq!(Rbp)) + ret()
}

pub fn to_asm(file: llr::File) -> asm::file::File {
    let mut asm = Asm::Concat(Vec::new());
    let mut ctxt = context::Context::new();
    let mut funs = Vec::new();
    for fun_decl in file.funs {
        if fun_decl.name.get_content() == "main" {
            asm = asm + compile_fun(fun_decl, &mut ctxt)
        } else {
            funs.push(fun_decl)
        }
    }
    for fun_decl in funs {
        asm = asm + compile_fun(fun_decl, &mut ctxt)
    }

    asm = asm + default_vec_function(&ctxt);
    asm::file::File::new(asm, file.strings)
}
