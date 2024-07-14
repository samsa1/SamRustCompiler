use std::collections::{HashMap, HashSet};

use super::context::Context;
use crate::ast::asm::Registers;
use crate::ast::common::PathUL;
use write_x86_64::*;

pub const HEAP_SIZE: usize = 8 << 10;

fn default_vec_function(
    heap_address: &str,
    ctxt: &mut Context,
    vec_info: crate::to_llr::VecInfo,
) -> Segment<instr::Instr> {
    let custom_alloc = true;

    let mut asm = Segment::label(ctxt.fun_label(&PathUL::from_vec(vec!["print_ptr"])))
        + pushq(reg!(RBP))
        + movq(reg!(RSP), reg!(RBP))
        + movq(addr!(16, RBP), reg!(RSI))
        + leaq(
            reg::Operand::LabRelAddr(reg::Label::from_str("my_string".to_string())),
            RDI,
        )
        + call(reg::Label::printf())
        + movq(reg!(RBP), reg!(RSP))
        + popq(RBP)
        + ret();

    /*
    A vector is a pointer to a tuple of 3 elements in the stack :
    - Pointer
    - Length
    - Capacity
    */
    for (id, size) in vec_info.new {
        let path = PathUL::from_vec(vec!["std", "vec", "Vec", "new"]);
        let path = crate::passes::handle_generics::new_path(&path, &id);
        asm += Segment::label(ctxt.fun_label(&path))
            + pushq(reg!(RBP)) /* bit align for malloc */
            + movq(immq(24), reg!(RDI)) /* Allocate chunk for 3 numbers */
            + if custom_alloc {
                // call of src-malloc
                subq(immq(16), reg!(RSP))
                + leaq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), RAX)
                + pushq(reg!(RAX))
                + pushq(reg!(RDI))
                + call(ctxt.fun_label(&PathUL::from_vec(vec!["std", "allocator", "malloc_0"])))
                + addq(immq(16), reg!(RSP))
                + popq(RAX)
                + addq(immq(8), reg!(RSP))
                + addq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), reg!(RAX))
                + addq(immq(32), reg!(RAX))
            } else { call(reg::Label::malloc()) }
            + movq(reg!(RAX), addr!(16, RSP)) /* Store the pointer to return it */
            + movq(reg!(RAX), reg!(RBP)) /* Store the pointer also in Rbp */
            + movq(immq(size as i64), reg!(RDI)) /* Get size of elements */
            + if custom_alloc {
                // call of src-malloc
                subq(immq(16), reg!(RSP))
                + leaq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), RAX)
                + pushq(reg!(RAX))
                + pushq(reg!(RDI))
                + call(ctxt.fun_label(&PathUL::from_vec(vec!["std", "allocator", "malloc_0"])))
                + addq(immq(16), reg!(RSP))
                + popq(RAX)
                + addq(immq(8), reg!(RSP))
                + addq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), reg!(RAX))
                + addq(immq(32), reg!(RAX))
            } else { call(reg::Label::malloc()) }
            + movq(reg!(RAX), addr!(RBP))
            + movq(immq(0), reg!(RAX))
            + movq(reg!(RAX), addr!(8, RBP))
            + movq(immq(1), reg!(RAX))
            + movq(reg!(RAX), addr!(16, RBP))
            + popq(RBP) /* returns */
            + ret()
    }

    let not_empty = !vec_info.len.is_empty();
    for (id, _) in vec_info.len {
        let path = PathUL::from_vec(vec!["std", "vec", "Vec", "len"]);
        let path = crate::passes::handle_generics::new_path(&path, &id);
        asm += Segment::label(ctxt.fun_label(&path))
    }
    if not_empty {
        asm += movq(addr!(8, RSP), reg!(RAX)) /* get pointer to vec */
            + movq(addr!(RAX), reg!(RAX))    /* get pointer to 4-vector */
            + movq(addr!(8, RAX), reg!(RAX)) /* put length in Rax */
            + movq(reg!(RAX), addr!(16, RSP)) /* Store the result in stack */
            + ret()
    }

    let mut hashmap = HashMap::new();

    for (id, size) in vec_info.get {
        match hashmap.get_mut(&size) {
            None => assert!(hashmap.insert(size, HashSet::from([id])).is_none()),
            Some(set) => assert!(set.insert(id)),
        }
    }

    for (size, ids) in hashmap {
        for id in ids {
            let path = PathUL::from_vec(vec!["std", "vec", "Vec", "get"]);
            let path = crate::passes::handle_generics::new_path(&path, &id);
            asm += Segment::label(ctxt.fun_label(&path))
        }
        asm += movq(addr!(16, RSP), reg!(RCX)) /* get pointer to vec */
            + movq(addr!(RCX), reg!(RCX)) /* get pointer to 4-vector */
            + movq(addr!(8, RSP), reg!(RAX)) /* put index in Rax */
            + movq(addr!(8, RCX), reg!(RDX))
            + leaq(
                reg::Operand::LabRelAddr(reg::Label::from_str("OoB_error".to_string())),
                R12,
                )
            + cmpq(reg!(RDX), reg!(RAX))
            + jae(reg::Label::panic())
            + imulq(immq(size as i64), reg!(RAX)) /* multiply by size of elements */
            + addq(addr!(RCX), reg!(RAX)) /* add base of vector to offset */
            + movq(reg!(RAX), addr!(24, RSP)) /* store result */
            + ret()
    }

    /*
    Called with element of size Size then a pointer to pointer to quadri vector
    */
    let mut hashmap = HashMap::new();

    for (id, size) in vec_info.push {
        match hashmap.get_mut(&size) {
            None => assert!(hashmap.insert(size, HashSet::from([id])).is_none()),
            Some(set) => assert!(set.insert(id)),
        }
    }

    for (size, ids) in hashmap {
        for id in ids {
            let path = PathUL::from_vec(vec!["std", "vec", "Vec", "push"]);
            let path = crate::passes::handle_generics::new_path(&path, &id);
            asm += Segment::label(ctxt.fun_label(&path))
        }
        let (label_has_capacity, _) = ctxt.gen_if_labels();
        asm += pushq(reg!(RBP))
            + movq(addr!(16 + size as i64, RSP), reg!(RBP)) /* get pointer to vec */
            + movq(addr!(RBP), reg!(RBP)) /* get pointer to 4-vector */
            + movq(addr!(8, RBP), reg!(RAX)) /* get length */
            + movq(addr!(16, RBP), reg!(RSI)) /* get capacity */
            + cmpq(reg!(RAX), reg!(RSI))
            + jnz(label_has_capacity.clone())
            + addq(reg!(RSI), reg!(RSI)) /* double capacity */
            + imulq(immq(size as i64), reg!(RSI))
            + movq(addr!(RBP), reg!(RDI))
            + if custom_alloc {
                // call of src-realloc
                subq(immq(8), reg!(RSP))
                + leaq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), RAX)
                + pushq(reg!(RAX))
                + subq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), reg!(RDI))
                + subq(immq(32), reg!(RDI))
                + pushq(reg!(RDI))
                + pushq(reg!(RSI))
                + call(ctxt.fun_label(&PathUL::from_vec(vec!["std", "allocator", "realloc_0"])))
                + addq(immq(24), reg!(RSP))
                + popq(RAX)
                + addq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), reg!(RAX))
                + addq(immq(32), reg!(RAX))
            } else { call(reg::Label::realloc()) }
            + movq(reg!(RAX), addr!(RBP)) // store new pointer
            + movq(addr!(8, RBP), reg!(RAX)) /* get length */
            + movq(addr!(16, RBP), reg!(RSI)) /* get capacity */
            + addq(reg!(RSI), reg!(RSI))
            + Segment::label(label_has_capacity)
            //    Rbx pointer to 4-vector
            //    Rsi current capacity
            //    Rax length
            + imulq(immq(size as i64), reg!(RAX))
            + addq(addr!(RBP), reg!(RAX)) /* target of move */
            + super::utils::mov_struct(RSP, 16, RAX, 0, size as u64, Registers::RegD)
            + incq(addr!(8, RBP))
            + popq(RBP)
            + ret()
    }
    asm
}

#[cfg(target_os = "macos")]
const WRITE_SYSCALL: i64 = 0x2000004;

#[cfg(target_os = "linux")]
const WRITE_SYSCALL: i64 = 0x1;
// https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/

fn printf_seg() -> Segment<instr::Instr> {
    use write_x86_64::reg::Label;

    let mut asm = Segment::label(Label::from_str("printf".to_string()))
        + pushq(reg!(RBP))
        + movq(reg!(RDI), reg!(RSI))
        + movq(immq(1), reg!(RDI))
        + xorq(reg!(RDX), reg!(RDX))
        + Segment::label(Label::from_str("printf_if_0".to_string()))
        + movb(addr!(0, RSI, RDX), reg!(AL))
        + testb(reg!(AL), reg!(AL))
        + jz(Label::from_str("printf_else_1".to_string()))
        + incq(reg!(RDX))
        + jmp(Label::from_str("printf_if_0".to_string()))
        + Segment::label(Label::from_str("printf_else_1".to_string()))
        + movq(immq(WRITE_SYSCALL), reg!(RAX))
        + syscall()
        + popq(RBP)
        + ret();

    asm += Segment::label(reg::Label::panic())
        + movq(reg!(R13), reg!(RSP))
        + movq(reg!(R12), reg!(RDI))
        + movq(immq(0), reg!(RAX))
        + call(reg::Label::printf())
        + popq(R13)
        + popq(R12)
        + popq(RBP)
        + movq(immq(1), reg!(RAX))
        + ret();
    asm
}

pub fn base(ctxt: &mut Context, vec_info: crate::to_llr::VecInfo) -> file::File {
    let heap_address = "heap_address".to_string();

    let text_ss = if !vec_info.new.is_empty() {
        Segment::label(reg::Label::from_str("main".to_string()))
            + pushq(reg!(RBP))
            + pushq(reg!(R12))
            + pushq(reg!(R13))
            + movq(reg!(RSP), reg!(R13))
            + leaq(reg::Operand::LabRelAddr(new_label("heap")), RAX)
            + movq(
                reg!(RAX),
                reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.clone())),
            )
            + movq(immq(HEAP_SIZE as i64 / 4), reg!(RCX))
            + movq(reg!(RCX), addr!(8, RAX))
            + movq(reg!(RCX), addr!(16, RAX))
            + movq(immq(4), addr!(24, RAX))
            + movq(reg!(RAX), reg!(RCX))
            + addq(immq(32), reg!(RCX))
            + movq(reg!(RCX), addr!(RAX))
            + leaq(
                reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.clone())),
                RAX,
            )
            + pushq(reg!(RAX))
            + pushq(reg!(RAX))
            + call(ctxt.fun_label(&PathUL::from_vec(vec!["std", "allocator", "init_0"])))
            + addq(immq(16), reg!(RSP))
            + call(ctxt.fun_label(&PathUL::from_vec(vec!["crate", "main_0"])))
            + popq(R13)
            + popq(R12)
            + popq(RBP)
            + xorq(reg!(RAX), reg!(RAX))
            + ret()
            + default_vec_function(&heap_address, ctxt, vec_info)
    } else {
        Segment::label(reg::Label::from_str("main".to_string()))
            + pushq(reg!(RBP))
            + pushq(reg!(R12))
            + pushq(reg!(R13))
            + movq(reg!(RSP), reg!(R13))
            + call(ctxt.fun_label(&PathUL::from_vec(vec!["crate", "main_0"])))
            + popq(R13)
            + popq(R12)
            + popq(RBP)
            + xorq(reg!(RAX), reg!(RAX))
            + ret()
    } + printf_seg();
    let data_ss = Segment::label(reg::Label::from_str("my_string".to_string()))
        + data::dasciz("%zd\\n".to_string())
        + Segment::label(reg::Label::from_str("division_by_zero_str".to_string()))
        + data::dasciz("Division by zero\\n".to_string())
        + Segment::label(reg::Label::from_str("OoB_error".to_string()))
        + data::dasciz("Index out of bound\\n".to_string())
        + Segment::label(reg::Label::from_str(heap_address))
        + data::dquad(0);

    file::File {
        globl: Some(reg::Label::from_str("main".to_string())),
        text_ss,
        data_ss,
    }
}
