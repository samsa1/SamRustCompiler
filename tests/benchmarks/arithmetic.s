	.text
	.globl	main
main:
	pushq %rbp
	pushq %r12
	pushq %r13
	movq %rsp, %r13
	call user_fun..crate..main_0
	popq %r13
	popq %r12
	popq %rbp
	xorq %rax, %rax
	ret
printf:
	pushq %rbp
	movq %rdi, %rsi
	movq $1, %rdi
	xorq %rdx, %rdx
printf_if_0:
	movb 0(%rsi, %rdx, 1), %al
	testb %al, %al
	jz printf_else_1
	incq %rdx
	jmp printf_if_0
printf_else_1:
	movq $1, %rax
	syscall
	popq %rbp
	ret
panic:
	movq %r13, %rsp
	movq %r12, %rdi
	movq $0, %rax
	call printf
	popq %r13
	popq %r12
	popq %rbp
	movq $1, %rax
	ret
user_fun..crate..arithmetic_0:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq 16(%rbp), %rax
	movq $10, %rcx
	shlq %cl, %rax
	imulq $543, %rax
	addq $252, %rax
	subq $245, %rax
	movq %rax, -8(%rbp)
	movq $12, %rax
	movq %rax, -16(%rbp)
	movq -16(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rcx
	shrq %cl, %rax
	movq $3, %rcx
	shlq %cl, %rax
	addq $16, %rsp
	movq %rax, 24(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
user_fun..crate..loop_operations_0:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $0, %rax
	movq %rax, -8(%rbp)
	movq $0, %rax
	movq %rax, -16(%rbp)
rust_while_cond_0:
	movq 16(%rbp), %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rcx
	cmpq %rcx, %rax
	jge rust_while_end_0
	subq $0, %rsp
	subq $16, %rsp
	movq -8(%rbp), %rax
	movq %rax, -32(%rbp)
	call user_fun..crate..arithmetic_0
	movq 8(%rsp), %rax
	addq $8, %rsp
	movq %rax, 0(%rsp)
	subq $24, %rsp
	movq -8(%rbp), %rax
	movq %rax, -48(%rbp)
	call user_fun..crate..arithmetic_0
	movq 8(%rsp), %rax
	addq $16, %rsp
	movq %rax, 0(%rsp)
	movq -16(%rbp), %rax
	popq %rcx
	andq %rcx, %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -16(%rbp)
	movq -8(%rbp), %rax
	addq $1, %rax
	movq %rax, -8(%rbp)
	jmp rust_while_cond_0
rust_while_end_0:
	movq -16(%rbp), %rax
	addq $16, %rsp
	movq %rax, 24(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
user_fun..crate..main_0:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	subq $16, %rsp
	movq $2000000000, %rax
	movq %rax, -16(%rbp)
	call user_fun..crate..loop_operations_0
	addq $16, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
my_string:
		.asciz "%zd\n"
division_by_zero_str:
		.asciz "Division by zero\n"
OoB_error:
		.asciz "Index out of bound\n"
heap_address:
		.quad  0         
heap:
		.space 8224
