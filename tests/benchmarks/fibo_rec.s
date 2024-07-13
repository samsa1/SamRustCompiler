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
user_fun..crate..fibo_0:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq 16(%rbp), %rax
	cmpq $2, %rax
	jae rust_if_else_0
	subq $0, %rsp
	movl $0, %eax
	jmp rust_if_end_0
rust_if_else_0:
	subq $16, %rsp
	movq 16(%rbp), %rax
	subq $1, %rax
	movq %rax, -8(%rbp)
	movq 16(%rbp), %rax
	subq $2, %rax
	movq %rax, -16(%rbp)
	subq $16, %rsp
	movq -16(%rbp), %rax
	movq %rax, -32(%rbp)
	call user_fun..crate..fibo_0
	addq $8, %rsp
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl %eax, 0(%rsp)
	subq $12, %rsp
	movq -8(%rbp), %rax
	movq %rax, -32(%rbp)
	call user_fun..crate..fibo_0
	addq $8, %rsp
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	addq $16, %rsp
rust_if_end_0:
	movl %eax, 24(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
user_fun..crate..main_0:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	subq $16, %rsp
	movq $46, %rax
	movq %rax, -16(%rbp)
	call user_fun..crate..fibo_0
	addq $8, %rsp
	addq $8, %rsp
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
