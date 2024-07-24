use crate::ast::asm::{ImmOrReg, Registers};
use crate::ast::common::Sizes;
use crate::backend::utils::{coercion, mov_struct};
use reg::Label;
use std::collections::{HashMap, HashSet};
use write_x86_64::*;

use super::super::base;
use super::super::context::Context;
use super::pre_asm::{self as pasm, InstructionInner, LoR};

fn find_pos(map: &HashMap<pasm::Reg, i64>, r: &pasm::Reg) -> i64 {
    match map.get(r) {
        Some(o) => *o,
        None => {
            assert_eq!(r.size, 0);
            0
        }
    }
}

fn insert_lor_l<R>(s: &mut HashSet<pasm::Reg>, l: &LoR<pasm::Reg, R>) {
    match l {
        LoR::L(r) => {
            s.insert(*r);
        }
        LoR::R(_) => (),
    }
}
fn insert_lor_r<L>(s: &mut HashSet<pasm::Reg>, r: &LoR<L, pasm::Reg>) {
    match r {
        LoR::R(r) => {
            s.insert(*r);
        }
        LoR::L(_) => (),
    }
}

fn reg_set(f: &pasm::DeclFun) -> HashSet<pasm::Reg> {
    let mut s = HashSet::new();
    s.insert(f.content.out_reg);
    for r in &f.args {
        s.insert(*r);
    }

    for (_, b) in &f.content.blocs {
        match &b.cond {
            Some((r, _)) => {
                s.insert(*r);
            }
            None => (),
        }
        for (_, instr) in &b.content {
            match &instr.instr {
                InstructionInner::CallP(right, args, out) => {
                    args.iter().for_each(|r| {
                        s.insert(r.clone());
                    });
                    insert_lor_r(&mut s, right);
                    s.insert(out.clone());
                }
                InstructionInner::BuildStruct(args, out) => {
                    args.iter().for_each(|(_, r)| {
                        s.insert(r.clone());
                    });
                    s.insert(out.clone());
                }
                InstructionInner::LArith(_, r1, l, r3) => {
                    s.insert(r1.clone());
                    insert_lor_l(&mut s, l);
                    s.insert(r3.clone());
                }
                InstructionInner::Shift(_, r1, l, r3) => {
                    s.insert(r1.clone());
                    insert_lor_l(&mut s, l);
                    s.insert(r3.clone());
                }
                InstructionInner::Leaq(right, r) => {
                    insert_lor_r(&mut s, right);
                    s.insert(r.clone());
                }
                InstructionInner::Update(r1, r2)
                | InstructionInner::Coercion(r1, r2, _, _)
                | InstructionInner::Deref(r1, r2)
                | InstructionInner::Extract(r1, _, r2)
                | InstructionInner::Mov(r1, r2) => {
                    s.insert(r1.clone());
                    s.insert(r2.clone());
                }
                InstructionInner::Unaop(_, r) | InstructionInner::Set(_, r) => {
                    s.insert(r.clone());
                }
                InstructionInner::HArith(_, r1, r2, r3) | InstructionInner::Cmp(_, r1, r2, r3) => {
                    s.insert(r1.clone());
                    s.insert(r2.clone());
                    s.insert(r3.clone());
                }
                InstructionInner::Return(r) => {
                    s.insert(r.clone());
                }
                InstructionInner::Print(_) => (),
            }
        }
    }
    s
}

fn label_of_bid(ctxt: &Context, bid: pasm::BID) -> Label {
    ctxt.bloc_label(bid.id())
}

fn compile_bloc(
    ctxt: &mut Context,
    mut bloc: pasm::Bloc<pasm::Reg>,
    map: &HashMap<pasm::Reg, i64>,
) -> Text {
    let mut asm = Segment::label(label_of_bid(ctxt, bloc.id));
    let mut current_id = bloc.enter_iid;
    while current_id != bloc.out_iid {
        let instr = bloc.content.remove(&current_id).unwrap();
        asm += match instr.instr {
            InstructionInner::Mov(r1, r2) => mov_struct(
                RBP,
                find_pos(map, &r1),
                RBP,
                find_pos(map, &r2),
                r1.size as u64,
                Registers::RegA,
            ),
            InstructionInner::Leaq(LoR::L(p), r) => {
                leaq(lab!(ctxt.fun_label(&p)), RAX) + movq(reg!(RAX), addr!(find_pos(map, &r), RBP))
            }
            InstructionInner::Leaq(LoR::R(r1), r2) => {
                leaq(addr!(find_pos(map, &r1), RBP), RAX)
                    + movq(reg!(RAX), addr!(find_pos(map, &r2), RBP))
            }
            InstructionInner::Set(v, r) => {
                v.to_reg(Registers::RegA)
                    + v.size().mov_addr(Registers::RegA, find_pos(map, &r), RBP)
            }
            InstructionInner::Cmp(cmp, r1, r2, r3) => {
                cmp.size.addr_mov(find_pos(map, &r1), RBP, Registers::RegC)
                    + cmp.size.addr_mov(find_pos(map, &r2), RBP, Registers::RegA)
                    + cmp.size.cmp(ImmOrReg::R(Registers::RegA), Registers::RegC)
                    + set(cmp.get_cond(), reg!(AL))
                    + movb(reg!(AL), addr!(find_pos(map, &r3), RBP))
            }
            InstructionInner::HArith(haop, r1, r2, r3) => {
                let (asm, reg) = haop.to_bin(Registers::RegC);
                haop.size.addr_mov(find_pos(map, &r1), RBP, Registers::RegA)
                    + haop.size.addr_mov(find_pos(map, &r2), RBP, Registers::RegC)
                    + asm
                    + haop.size.mov_addr(reg, find_pos(map, &r3), RBP)
            }
            InstructionInner::LArith(aop, r1, LoR::L(r2), r3) => {
                aop.size()
                    .addr_mov(find_pos(map, &r1), RBP, Registers::RegA)
                    + aop
                        .size()
                        .addr_mov(find_pos(map, &r2), RBP, Registers::RegC)
                    + aop.to_bin(ImmOrReg::R(Registers::RegC), Registers::RegA)
                    + aop
                        .size()
                        .mov_addr(Registers::RegA, find_pos(map, &r3), RBP)
            }
            InstructionInner::LArith(aop, r1, LoR::R(v), r3) => {
                aop.size()
                    .addr_mov(find_pos(map, &r1), RBP, Registers::RegA)
                    + aop.to_bin(ImmOrReg::V(v), Registers::RegA)
                    + aop
                        .size()
                        .mov_addr(Registers::RegA, find_pos(map, &r3), RBP)
            }
            InstructionInner::Unaop(uno, r1) => {
                uno.size()
                    .addr_mov(find_pos(map, &r1), RBP, Registers::RegA)
                    + uno.to_bin(Registers::RegA)
                    + uno
                        .size()
                        .mov_addr(Registers::RegA, find_pos(map, &r1), RBP)
            }
            InstructionInner::Coercion(r1, r2, typ1, typ2) => {
                typ1.size()
                    .addr_mov(find_pos(map, &r1), RBP, Registers::RegA)
                    + coercion(Registers::RegA, typ1, Registers::RegA, typ2)
                    + typ2
                        .size()
                        .mov_addr(Registers::RegA, find_pos(map, &r2), RBP)
            }
            InstructionInner::CallP(fun_name, args, out) => {
                let mut total_size = 0;
                for arg in &args {
                    total_size += arg.size
                }
                let offset = (total_size + out.size) % 16;
                let missing = if offset == 0 { 0 } else { 16 - offset };
                let mut asm = subq(
                    immq(missing as i64 + out.size as i64 + total_size as i64),
                    reg!(RSP),
                );
                let mut current_offset = total_size as i64;
                for arg in args {
                    current_offset -= arg.size as i64;
                    asm += mov_struct(
                        RBP,
                        find_pos(map, &arg),
                        RSP,
                        current_offset,
                        arg.size as u64,
                        Registers::RegA,
                    );
                }
                let call_instr = match fun_name {
                    LoR::L(fun_name) => call(ctxt.fun_label(&fun_name)),
                    LoR::R(r) => call_star(addr!(find_pos(map, &r), RBP)),
                };
                asm + call_instr
                    + mov_struct(
                        RSP,
                        total_size as i64,
                        RBP,
                        find_pos(map, &out),
                        out.size as u64,
                        Registers::RegA,
                    )
                    + addq(
                        immq(missing as i64 + out.size as i64 + total_size as i64),
                        reg!(RSP),
                    )
            }
            InstructionInner::Print(label_name) => {
                leaq(
                    reg::Operand::LabRelAddr(ctxt.string_label(&label_name)),
                    RDI,
                ) + movq(immq(0), reg!(RAX))
                    + call(reg::Label::printf())
            }
            InstructionInner::Return(r) => {
                mov_struct(
                    RBP,
                    find_pos(&map, &r),
                    RBP,
                    ctxt.get_return_offset(),
                    r.size as u64,
                    Registers::RegA,
                ) + movq(reg!(RBP), reg!(RSP))
                    + popq(RBP)
                    + ret()
            }
            InstructionInner::Shift(op, r1, LoR::L(r2), r3) => {
                Sizes::from_int(r2.size).addr_mov(find_pos(map, &r2), RBP, Registers::RegC)
                    + op.size().addr_mov(find_pos(map, &r1), RBP, Registers::RegA)
                    + op.reg(Registers::RegA)
                    + op.size().mov_addr(Registers::RegA, find_pos(map, &r3), RBP)
            }
            InstructionInner::Shift(op, r1, LoR::R(size), r3) => {
                op.size().addr_mov(find_pos(map, &r1), RBP, Registers::RegA)
                    + op.imm(Registers::RegA, size)
                    + op.size().mov_addr(Registers::RegA, find_pos(map, &r3), RBP)
            }
            InstructionInner::Update(r1, r2) => {
                movq(addr!(find_pos(map, &r2), RBP), reg!(RCX))
                    + mov_struct(
                        RBP,
                        find_pos(map, &r1),
                        RCX,
                        0,
                        r1.size as u64,
                        Registers::RegA,
                    )
            }
            InstructionInner::BuildStruct(args, out) => {
                let mut asm = Text::empty();
                for (offset, r) in args {
                    asm += mov_struct(
                        RBP,
                        find_pos(map, &r),
                        RBP,
                        find_pos(map, &out) + offset as i64,
                        r.size as u64,
                        Registers::RegA,
                    )
                }
                asm
            }
            InstructionInner::Deref(r1, r2) => {
                movq(addr!(find_pos(map, &r1), RBP), reg!(RCX))
                    + mov_struct(
                        RCX,
                        0,
                        RBP,
                        find_pos(map, &r2),
                        r2.size as u64,
                        Registers::RegA,
                    )
            }
            InstructionInner::Extract(r1, offset, r2) => {
                movq(addr!(find_pos(map, &r1), RBP), reg!(RCX))
                    + mov_struct(
                        RCX,
                        offset as i64,
                        RBP,
                        find_pos(map, &r2),
                        r2.size as u64,
                        Registers::RegA,
                    )
            }
        };
        current_id = instr.next;
    }

    match bloc.cond {
        Some((r, bid)) => {
            asm += movb(addr!(find_pos(map, &r), RBP), reg!(AL))
                + testb(reg!(AL), reg!(AL))
                + jcc(instr::Cond::NZ, label_of_bid(ctxt, bid))
        }
        None => (),
    }

    asm + match bloc.next {
        Some(bid) => jmp(label_of_bid(ctxt, bid)),
        None => jmp(ctxt.ret_label()),
    }
}

fn compile_fun(f: pasm::DeclFun, ctxt: &mut Context) -> Text {
    let regs = reg_set(&f);

    ctxt.set_fun_name(f.name);
    let mut map = HashMap::new();
    let mut offset = 0;
    for i in regs {
        offset += i.size as i64;
        assert!(map.insert(i, -offset).is_none());
    }
    let padding = offset;
    let mut asm = Segment::label(ctxt.self_label())
        + pushq(reg!(RBP))
        + movq(reg!(RSP), reg!(RBP))
        + subq(immq(padding), reg!(RSP));
    let mut offset = 16;
    for r in f.args.into_iter().rev() {
        asm += mov_struct(
            RBP,
            offset,
            RBP,
            find_pos(&map, &r),
            r.size as u64,
            Registers::RegA,
        );
        offset += r.size as i64;
    }
    ctxt.set_return_offset(offset);
    asm += jmp(label_of_bid(ctxt, f.content.enter));
    for (_, bloc) in f.content.blocs {
        asm += compile_bloc(ctxt, bloc, &map)
    }
    asm + Segment::label(ctxt.ret_label())
        + mov_struct(
            RBP,
            find_pos(&map, &f.content.out_reg),
            RBP,
            ctxt.get_return_offset(),
            f.content.out_reg.size as u64,
            Registers::RegA,
        )
        + movq(reg!(RBP), reg!(RSP))
        + popq(RBP)
        + ret()
}

fn to_asm(file: pasm::File, strings: HashMap<String, String>, ctxt: &mut Context) -> file::File {
    let mut text_ss = Segment::empty();
    for fun_decl in file.funs {
        text_ss += compile_fun(fun_decl, ctxt)
    }

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
    file: pasm::File,
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
