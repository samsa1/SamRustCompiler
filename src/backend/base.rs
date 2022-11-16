use write_x86_64::*;
use crate::ast::common::PathUL;
use super::context::Context;

fn default_vec_function(heap_address: &str, ctxt: &Context) -> Text {
    let custom_alloc = true;

    label(ctxt.fun_label(&PathUL::from_vec(vec!["print_ptr"])))
        + pushq(reg!(RBP))
        + movq(reg!(RSP), reg!(RBP))
        + movq(addr!(16, RBP), reg!(RSI))
        + leaq(reg::Operand::LabRelAddr(reg::Label::from_str("my_string".to_string())), RDI)
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
    + label(ctxt.fun_label(&PathUL::from_vec(vec!["std", "vec", "Vec", "new"])))
        + pushq(reg!(RBP)) /* bit align for malloc */
        + movq(immq(32), reg!(RDI)) /* Allocate chunk for 4 numbers */
        + if custom_alloc {
            // call of src-malloc
            subq(immq(16), reg!(RSP))
            + leaq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), RAX)
            + pushq(reg!(RAX))
            + pushq(reg!(RDI))
            + call(ctxt.fun_label(&PathUL::from_vec(vec!["std", "allocator", "malloc"])))
            + addq(immq(16), reg!(RSP))
            + popq(RAX)
            + addq(immq(8), reg!(RSP))
            + addq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), reg!(RAX))
            + addq(immq(32), reg!(RAX))
        } else { call(reg::Label::malloc()) }
        + movq(reg!(RAX), addr!(24, RSP)) /* Store the pointer to return it */
        + movq(reg!(RAX), reg!(RBP)) /* Store the pointer also in Rbp */
        + movq(addr!(16, RSP), reg!(RDI)) /* Get size of elements */
        + movq(reg!(RDI), addr!(24, RBP)) /* Initialize quadri-vector with a capacity of 1 */
        + if custom_alloc {
            // call of src-malloc
            subq(immq(16), reg!(RSP))
            + leaq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), RAX)
            + pushq(reg!(RAX))
            + pushq(reg!(RDI))
            + call(ctxt.fun_label(&PathUL::from_vec(vec!["std", "allocator", "malloc"])))
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
    + label(ctxt.fun_label(&PathUL::from_vec(vec!["std", "vec", "Vec", "len"])))
        + movq(addr!(8, RSP), reg!(RAX)) /* get pointer to vec */
        + movq(addr!(RAX), reg!(RAX))    /* get pointer to 4-vector */
        + movq(addr!(8, RAX), reg!(RAX)) /* put length in Rax */
        + movq(reg!(RAX), addr!(16, RSP)) /* Store the result in stack */
        + ret()
    + label(ctxt.fun_label(&PathUL::from_vec(vec!["std", "vec", "Vec", "get"])))
        + movq(addr!(16, RSP), reg!(RCX)) /* get pointer to vec */
        + movq(addr!(RCX), reg!(RCX)) /* get pointer to 4-vector */
        + movq(addr!(8, RSP), reg!(RAX)) /* put index in Rax */
        + movq(addr!(8, RCX), reg!(RDX))
        + leaq(
            reg::Operand::LabRelAddr(reg::Label::from_str("OoB_error".to_string())),
            R12,
            )
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
    + label(ctxt.fun_label(&PathUL::from_vec(vec!["std", "vec", "Vec", "push"])))
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
        + if custom_alloc {
            // call of src-realloc
            subq(immq(8), reg!(RSP))
            + leaq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), RAX)
            + pushq(reg!(RAX))
            + subq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), reg!(RDI))
            + subq(immq(32), reg!(RDI))
            + pushq(reg!(RDI))
            + pushq(reg!(RSI))
            + call(ctxt.fun_label(&PathUL::from_vec(vec!["std", "allocator", "realloc"])))
            + addq(immq(24), reg!(RSP))
            + popq(RAX)
            + addq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.to_string())), reg!(RAX))
            + addq(immq(32), reg!(RAX))
        } else { call(reg::Label::realloc()) }
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
        + movb(addr!(RCX), reg!(DL))
        + movb(reg!(DL), addr!(RAX))
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
        + movq(reg!(R12), reg!(RDI))
        + movq(immq(0), reg!(RAX))
        + call(reg::Label::printf())
        + popq(R13)
        + popq(R12)
        + popq(RBP)
        + movq(immq(1), reg!(RAX))
        + ret()
}

pub fn base(ctxt: &Context) -> file::File {
    let heap_address = "heap_address".to_string();
    let heap_size = 8 << 10;

    let text_ss = label(reg::Label::from_str("main".to_string()))
        + pushq(reg!(RBP))
        + pushq(reg!(R12))
        + pushq(reg!(R13))
        + movq(reg!(RSP), reg!(R13))
        + movq(immq(heap_size + 32), reg!(RDI)) // 8ko
        + call(reg::Label::malloc())
        + movq(reg!(RAX), reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.clone())))
        + movq(immq(heap_size / 4), reg!(RCX))
        + movq(reg!(RCX), addr!(8, RAX))
        + movq(reg!(RCX), addr!(16, RAX))
        + movq(immq(4), addr!(24, RAX))
        + movq(reg!(RAX), reg!(RCX))
        + addq(immq(32), reg!(RCX))
        + movq(reg!(RCX), addr!(RAX))
        + leaq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.clone())), RAX)
        + pushq(reg!(RAX))
        + pushq(reg!(RAX))
        + call(ctxt.fun_label(&PathUL::from_vec(vec!["std", "allocator", "init"])))
        + addq(immq(16), reg!(RSP))

        + call(ctxt.fun_label(&PathUL::from_vec(vec!["crate", "main"])))
        + movq(reg::Operand::LabRelAddr(reg::Label::from_str(heap_address.clone())), reg!(RDI))
        + call(reg::Label::free())
        + popq(R13)
        + popq(R12)
        + popq(RBP)
        + xorq(reg!(RAX), reg!(RAX))
        + ret()
        + default_vec_function(&heap_address, ctxt);
    let data_ss = data::dstring("my_string".to_string(), "%zd\\n".to_string())
        + data::dstring(
            "division_by_zero_str".to_string(),
            "Division by zero\\n".to_string(),
        )
        + data::dstring("OoB_error".to_string(), "Index out of bound\\n".to_string())
        + data::dquad(heap_address, vec![0]);

    file::File {
        globl: Some(reg::Label::from_str("main".to_string())),
        text_ss,
        data_ss,
    }
}
