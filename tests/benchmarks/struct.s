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
user_fun..crate..build1_0:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	subq $10, %rsp
	movb $0, %al
	movb %al, -2(%rbp)
	movl $4, %eax
	movl %eax, -6(%rbp)
	movl $3, %eax
	movl %eax, -10(%rbp)
	movb $1, %al
	movb %al, -1(%rbp)
	movq 0(%rsp), %rax
	movq %rax, 16(%rbp)
	movw 8(%rsp), %ax
	movw %ax, 24(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
user_fun..crate..build2_0:
	pushq %rbp
	movq %rsp, %rbp
	subq $20, %rsp
	subq $12, %rsp
	call user_fun..crate..build1_0
	movq 0(%rsp), %rax
	movq %rax, -10(%rbp)
	movw 8(%rsp), %ax
	movw %ax, -2(%rbp)
	addq $12, %rsp
	subq $12, %rsp
	call user_fun..crate..build1_0
	movq 0(%rsp), %rax
	movq %rax, -20(%rbp)
	movw 8(%rsp), %ax
	movw %ax, -12(%rbp)
	addq $12, %rsp
	subq $20, %rsp
	subq $10, %rsp
	movq -10(%rbp), %rax
	movq %rax, 0(%rsp)
	movw -2(%rbp), %ax
	movw %ax, 8(%rsp)
	movq 0(%rsp), %rax
	movq %rax, -40(%rbp)
	movw 8(%rsp), %ax
	movw %ax, -32(%rbp)
	addq $10, %rsp
	subq $10, %rsp
	movq -20(%rbp), %rax
	movq %rax, 0(%rsp)
	movw -12(%rbp), %ax
	movw %ax, 8(%rsp)
	movq 0(%rsp), %rax
	movq %rax, -30(%rbp)
	movw 8(%rsp), %ax
	movw %ax, -22(%rbp)
	addq $10, %rsp
	movq 0(%rsp), %rax
	movq %rax, 16(%rbp)
	movq 8(%rsp), %rax
	movq %rax, 24(%rbp)
	movl 16(%rsp), %eax
	movl %eax, 32(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
user_fun..crate..build3_0:
	pushq %rbp
	movq %rsp, %rbp
	subq $60, %rsp
	subq $20, %rsp
	call user_fun..crate..build2_0
	movq 0(%rsp), %rax
	movq %rax, -20(%rbp)
	movq 8(%rsp), %rax
	movq %rax, -12(%rbp)
	movl 16(%rsp), %eax
	movl %eax, -4(%rbp)
	addq $20, %rsp
	subq $20, %rsp
	call user_fun..crate..build2_0
	movq 0(%rsp), %rax
	movq %rax, -40(%rbp)
	movq 8(%rsp), %rax
	movq %rax, -32(%rbp)
	movl 16(%rsp), %eax
	movl %eax, -24(%rbp)
	addq $20, %rsp
	subq $20, %rsp
	call user_fun..crate..build2_0
	movq 0(%rsp), %rax
	movq %rax, -60(%rbp)
	movq 8(%rsp), %rax
	movq %rax, -52(%rbp)
	movl 16(%rsp), %eax
	movl %eax, -44(%rbp)
	addq $20, %rsp
	subq $60, %rsp
	subq $20, %rsp
	movq -20(%rbp), %rax
	movq %rax, 0(%rsp)
	movq -12(%rbp), %rax
	movq %rax, 8(%rsp)
	movl -4(%rbp), %eax
	movl %eax, 16(%rsp)
	movq 0(%rsp), %rax
	movq %rax, -120(%rbp)
	movq 8(%rsp), %rax
	movq %rax, -112(%rbp)
	movl 16(%rsp), %eax
	movl %eax, -104(%rbp)
	addq $20, %rsp
	subq $20, %rsp
	movq -40(%rbp), %rax
	movq %rax, 0(%rsp)
	movq -32(%rbp), %rax
	movq %rax, 8(%rsp)
	movl -24(%rbp), %eax
	movl %eax, 16(%rsp)
	movq 0(%rsp), %rax
	movq %rax, -80(%rbp)
	movq 8(%rsp), %rax
	movq %rax, -72(%rbp)
	movl 16(%rsp), %eax
	movl %eax, -64(%rbp)
	addq $20, %rsp
	subq $20, %rsp
	movq -60(%rbp), %rax
	movq %rax, 0(%rsp)
	movq -52(%rbp), %rax
	movq %rax, 8(%rsp)
	movl -44(%rbp), %eax
	movl %eax, 16(%rsp)
	movq 0(%rsp), %rax
	movq %rax, -100(%rbp)
	movq 8(%rsp), %rax
	movq %rax, -92(%rbp)
	movl 16(%rsp), %eax
	movl %eax, -84(%rbp)
	addq $20, %rsp
	movq 0(%rsp), %rax
	movq %rax, 16(%rbp)
	movq 8(%rsp), %rax
	movq %rax, 24(%rbp)
	movq 16(%rsp), %rax
	movq %rax, 32(%rbp)
	movq 24(%rsp), %rax
	movq %rax, 40(%rbp)
	movq 32(%rsp), %rax
	movq %rax, 48(%rbp)
	movq 40(%rsp), %rax
	movq %rax, 56(%rbp)
	movq 48(%rsp), %rax
	movq %rax, 64(%rbp)
	movl 56(%rsp), %eax
	movl %eax, 72(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
user_fun..crate..test_0:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	leaq 16(%rbp), %rax
	addq $20, %rax
	addq $10, %rax
	movl 0(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $20, %rax
	addq $10, %rax
	movl 4(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $20, %rax
	addq $0, %rax
	movl 0(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $20, %rax
	addq $0, %rax
	movl 4(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $40, %rax
	addq $10, %rax
	movl 0(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $40, %rax
	addq $10, %rax
	movl 4(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $40, %rax
	addq $0, %rax
	movl 0(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $40, %rax
	addq $0, %rax
	movl 4(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $0, %rax
	addq $10, %rax
	movl 0(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $0, %rax
	addq $10, %rax
	movl 4(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $0, %rax
	addq $0, %rax
	movl 0(%rax), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	leaq 16(%rbp), %rax
	addq $0, %rax
	addq $0, %rax
	movl 4(%rax), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl %eax, 76(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
user_fun..crate..main_0:
	pushq %rbp
	movq %rsp, %rbp
	subq $4, %rsp
	movl $0, %eax
	movl %eax, -4(%rbp)
rust_while_cond_0:
	movl -4(%rbp), %eax
	cmpl $300000000, %eax
	jge rust_while_end_0
	subq $60, %rsp
	subq $64, %rsp
	call user_fun..crate..build3_0
	movq 0(%rsp), %rax
	movq %rax, -64(%rbp)
	movq 8(%rsp), %rax
	movq %rax, -56(%rbp)
	movq 16(%rsp), %rax
	movq %rax, -48(%rbp)
	movq 24(%rsp), %rax
	movq %rax, -40(%rbp)
	movq 32(%rsp), %rax
	movq %rax, -32(%rbp)
	movq 40(%rsp), %rax
	movq %rax, -24(%rbp)
	movq 48(%rsp), %rax
	movq %rax, -16(%rbp)
	movl 56(%rsp), %eax
	movl %eax, -8(%rbp)
	addq $64, %rsp
	subq $64, %rsp
	subq $60, %rsp
	movq -64(%rbp), %rax
	movq %rax, 0(%rsp)
	movq -56(%rbp), %rax
	movq %rax, 8(%rsp)
	movq -48(%rbp), %rax
	movq %rax, 16(%rsp)
	movq -40(%rbp), %rax
	movq %rax, 24(%rsp)
	movq -32(%rbp), %rax
	movq %rax, 32(%rsp)
	movq -24(%rbp), %rax
	movq %rax, 40(%rsp)
	movq -16(%rbp), %rax
	movq %rax, 48(%rsp)
	movl -8(%rbp), %eax
	movl %eax, 56(%rsp)
	movq 0(%rsp), %rax
	movq %rax, -128(%rbp)
	movq 8(%rsp), %rax
	movq %rax, -120(%rbp)
	movq 16(%rsp), %rax
	movq %rax, -112(%rbp)
	movq 24(%rsp), %rax
	movq %rax, -104(%rbp)
	movq 32(%rsp), %rax
	movq %rax, -96(%rbp)
	movq 40(%rsp), %rax
	movq %rax, -88(%rbp)
	movq 48(%rsp), %rax
	movq %rax, -80(%rbp)
	movl 56(%rsp), %eax
	movl %eax, -72(%rbp)
	addq $60, %rsp
	call user_fun..crate..test_0
	addq $64, %rsp
	movl -4(%rbp), %eax
	addl $1, %eax
	movl %eax, -4(%rbp)
	addq $60, %rsp
	jmp rust_while_cond_0
rust_while_end_0:
	addq $4, %rsp
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
