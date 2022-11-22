	.text
	.globl	_main
_main:
	pushq %rbp
	pushq %r12
	pushq %r13
	movq %rsp, %r13
	movq $8224, %rdi
	call _malloc
	movq %rax, _heap_address(%rip)
	movq $2048, %rcx
	movq %rcx, 8(%rax)
	movq %rcx, 16(%rax)
	movq $4, 24(%rax)
	movq %rax, %rcx
	addq $32, %rcx
	movq %rcx, 0(%rax)
	leaq _heap_address(%rip), %rax
	pushq %rax
	pushq %rax
	call _user_fun..std..allocator..init
	addq $16, %rsp
	call _user_fun..crate..main
	movq _heap_address(%rip), %rdi
	call _free
	popq %r13
	popq %r12
	popq %rbp
	xorq %rax, %rax
	ret
_user_fun..print_ptr:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rsi
	leaq _my_string(%rip), %rdi
	call _printf
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..std..vec..Vec..new:
	pushq %rbp
	movq $32, %rdi
	subq $16, %rsp
	leaq _heap_address(%rip), %rax
	pushq %rax
	pushq %rdi
	call _user_fun..std..allocator..malloc
	addq $16, %rsp
	popq %rax
	addq $8, %rsp
	addq _heap_address(%rip), %rax
	addq $32, %rax
	movq %rax, 24(%rsp)
	movq %rax, %rbp
	movq 16(%rsp), %rdi
	movq %rdi, 24(%rbp)
	subq $16, %rsp
	leaq _heap_address(%rip), %rax
	pushq %rax
	pushq %rdi
	call _user_fun..std..allocator..malloc
	addq $16, %rsp
	popq %rax
	addq $8, %rsp
	addq _heap_address(%rip), %rax
	addq $32, %rax
	movq %rax, 0(%rbp)
	movq $0, %rax
	movq %rax, 8(%rbp)
	movq $1, %rax
	movq %rax, 16(%rbp)
	popq %rbp
	ret
_user_fun..std..vec..Vec..len:
	movq 8(%rsp), %rax
	movq 0(%rax), %rax
	movq 8(%rax), %rax
	movq %rax, 16(%rsp)
	ret
_user_fun..std..vec..Vec..get:
	movq 16(%rsp), %rcx
	movq 0(%rcx), %rcx
	movq 8(%rsp), %rax
	movq 8(%rcx), %rdx
	leaq _OoB_error(%rip), %r12
	cmpq %rdx, %rax
	jae _panic
	imulq 24(%rcx), %rax
	addq 0(%rcx), %rax
	movq %rax, 24(%rsp)
	ret
_user_fun..std..vec..Vec..push:
	pushq %rbp
	movq 16(%rsp), %rbp
	movq 0(%rbp), %rbp
	movq 8(%rbp), %rax
	movq 16(%rbp), %rsi
	cmpq %rax, %rsi
	jnz _push_has_capacity
	addq %rsi, %rsi
	imulq 24(%rbp), %rsi
	movq 0(%rbp), %rdi
	subq $8, %rsp
	leaq _heap_address(%rip), %rax
	pushq %rax
	subq _heap_address(%rip), %rdi
	subq $32, %rdi
	pushq %rdi
	pushq %rsi
	call _user_fun..std..allocator..realloc
	addq $24, %rsp
	popq %rax
	addq _heap_address(%rip), %rax
	addq $32, %rax
	movq %rax, 0(%rbp)
	movq 8(%rbp), %rax
	movq 16(%rbp), %rsi
	addq %rsi, %rsi
_push_has_capacity:
	movq 24(%rbp), %rdi
	imulq %rdi, %rax
	addq 0(%rbp), %rax
	leaq 24(%rsp), %rcx
_push_copy_while_start:
	testq %rdi, %rdi
	jz _push_copy_while_end
	movb 0(%rcx), %dl
	movb %dl, 0(%rax)
	decq %rdi
	incq %rax
	incq %rcx
	jmp _push_copy_while_start
_push_copy_while_end:
	incq 8(%rbp)
	popq %rbp
	ret
_panic:
	movq %r13, %rsp
	movq %r12, %rdi
	movq $0, %rax
	call _printf
	popq %r13
	popq %r12
	popq %rbp
	movq $1, %rax
	ret
_user_fun..std..main:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	subq $0, %rsp
	call _user_fun..std..allocator..testing
	addq $0, %rsp
	addq $0, %rsp
	addq $0, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..std..allocator..const_zero:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq $0, %rax
	addq $0, %rsp
	movq %rax, 16(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..std..allocator..init:
	pushq %rbp
	movq %rsp, %rbp
	subq $44, %rsp
	subq $20, %rsp
	movq 16(%rbp), %rax
	movq %rax, -64(%rbp)
	call _user_fun..std..vec..Vec..len
	addq $8, %rsp
	movq 0(%rsp), %rax
	movq %rax, -8(%rbp)
	addq $12, %rsp
	movq $4, %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rcx
	subq %rcx, %rax
	movq %rax, -16(%rbp)
	movq -16(%rbp), %rax
	nop
	movl %eax, -20(%rbp)
	subq $36, %rsp
	movq 16(%rbp), %rax
	movq %rax, -72(%rbp)
	movq $0, %rax
	movq %rax, -80(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $12, %rsp
	pushq %rax
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -20(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	orl %ecx, %eax
	popq %rcx
	movl %eax, 0(%rcx)
	subq $36, %rsp
	movq 16(%rbp), %rax
	movq %rax, -72(%rbp)
	movq $1, %rax
	movq %rax, -80(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $12, %rsp
	pushq %rax
	movl $0, %eax
	popq %rcx
	movl %eax, 0(%rcx)
	subq $20, %rsp
	movq 16(%rbp), %rax
	movq %rax, -64(%rbp)
	call _user_fun..std..vec..Vec..len
	addq $8, %rsp
	movq 0(%rsp), %rax
	movq %rax, -28(%rbp)
	addq $12, %rsp
	movq $2, %rax
	pushq %rax
	movq -28(%rbp), %rax
	popq %rcx
	subq %rcx, %rax
	movq %rax, -36(%rbp)
	subq $36, %rsp
	movq 16(%rbp), %rax
	movq %rax, -72(%rbp)
	movq -36(%rbp), %rax
	movq %rax, -80(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $12, %rsp
	pushq %rax
	movl $1, %eax
	popq %rcx
	movl %eax, 0(%rcx)
	movq $1, %rax
	pushq %rax
	movq -28(%rbp), %rax
	popq %rcx
	subq %rcx, %rax
	movq %rax, -44(%rbp)
	subq $36, %rsp
	movq 16(%rbp), %rax
	movq %rax, -72(%rbp)
	movq -44(%rbp), %rax
	movq %rax, -80(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $12, %rsp
	pushq %rax
	movl $0, %eax
	popq %rcx
	movl %eax, 0(%rcx)
	addq $44, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..std..allocator..malloc:
	pushq %rbp
	movq %rsp, %rbp
	subq $24, %rsp
	movq $4294967296, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rcx
	cmpq %rcx, %rax
	setae %al
	testb %al, %al
	jz _rust_if_else_0
	subq $0, %rsp
	movq $-1, %rax
	movq %rax, 32(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
	addq $0, %rsp
	jmp _rust_if_end_0
_rust_if_else_0:
	subq $0, %rsp
	addq $0, %rsp
_rust_if_end_0:
	movq 16(%rbp), %rax
	nop
	movl %eax, -4(%rbp)
	movl $7, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -4(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl %eax, -8(%rbp)
	movl $3, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -8(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	shrl %cl, %eax
	movl %eax, -12(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -12(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	shll %cl, %eax
	movl %eax, -4(%rbp)
	movq $0, %rax
	movq %rax, -20(%rbp)
	movl $0, %eax
	movl %eax, -24(%rbp)
_rust_while_cond_0:
	movb $1, %al
	testb %al, %al
	jz _rust_while_end_0
	subq $4, %rsp
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	subq $32, %rsp
	movq 24(%rbp), %rax
	movq %rax, -56(%rbp)
	movq -20(%rbp), %rax
	movq %rax, -64(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $8, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl 0(%rsp), %ecx
	addq $4, %rsp
	andl %ecx, %eax
	movl %eax, -28(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -28(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	cmpl %ecx, %eax
	setz %al
	testb %al, %al
	jz _rust_if_else_4
	subq $24, %rsp
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	subq $24, %rsp
	movq 24(%rbp), %rax
	movq %rax, -72(%rbp)
	movq -20(%rbp), %rax
	movq %rax, -80(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $0, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl 0(%rsp), %ecx
	addq $4, %rsp
	subl %ecx, %eax
	movl %eax, -32(%rbp)
	movl $2, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -4(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl %eax, -36(%rbp)
	movl -36(%rbp), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -32(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	cmpl %ecx, %eax
	setae %al
	testb %al, %al
	jz _rust_if_else_2
	subq $20, %rsp
	movl $4, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -4(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl %eax, -56(%rbp)
	movl -56(%rbp), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -32(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	cmpl %ecx, %eax
	seta %al
	testb %al, %al
	jz _rust_if_else_1
	subq $88, %rsp
	movl -4(%rbp), %eax
	movl %eax, %eax
	movq %rax, -80(%rbp)
	movq -80(%rbp), %rax
	pushq %rax
	movq -20(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -88(%rbp)
	movq $2, %rax
	pushq %rax
	movq -88(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -96(%rbp)
	movl -4(%rbp), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -32(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	subl %ecx, %eax
	movl %eax, -100(%rbp)
	movl $2, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -100(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	subl %ecx, %eax
	movl %eax, -104(%rbp)
	subq $32, %rsp
	movq 24(%rbp), %rax
	movq %rax, -184(%rbp)
	movq -96(%rbp), %rax
	movq %rax, -192(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $8, %rsp
	pushq %rax
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -104(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	orl %ecx, %eax
	popq %rcx
	movl %eax, 0(%rcx)
	movl -4(%rbp), %eax
	movl %eax, %eax
	movq %rax, -112(%rbp)
	movq -112(%rbp), %rax
	pushq %rax
	movq -20(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -120(%rbp)
	movq $3, %rax
	pushq %rax
	movq -120(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -128(%rbp)
	subq $32, %rsp
	movq 24(%rbp), %rax
	movq %rax, -184(%rbp)
	movq -128(%rbp), %rax
	movq %rax, -192(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $8, %rsp
	pushq %rax
	movq -20(%rbp), %rax
	nop
	popq %rcx
	movl %eax, 0(%rcx)
	movl -32(%rbp), %eax
	movl %eax, %eax
	movq %rax, -136(%rbp)
	movq -136(%rbp), %rax
	pushq %rax
	movq -20(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -144(%rbp)
	movq $3, %rax
	pushq %rax
	movq -144(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -152(%rbp)
	movq -20(%rbp), %rax
	nop
	movl %eax, -156(%rbp)
	movl -4(%rbp), %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -156(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl %eax, -160(%rbp)
	subq $32, %rsp
	movq 24(%rbp), %rax
	movq %rax, -184(%rbp)
	movq -152(%rbp), %rax
	movq %rax, -192(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $8, %rsp
	pushq %rax
	movl $2, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -160(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	popq %rcx
	movl %eax, 0(%rcx)
	addq $88, %rsp
	jmp _rust_if_end_1
_rust_if_else_1:
	subq $0, %rsp
	movl -32(%rbp), %eax
	movl %eax, -4(%rbp)
	addq $0, %rsp
_rust_if_end_1:
	subq $24, %rsp
	movq 24(%rbp), %rax
	movq %rax, -88(%rbp)
	movq -20(%rbp), %rax
	movq %rax, -96(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $0, %rsp
	pushq %rax
	movl $2, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -4(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	popq %rcx
	movl %eax, 0(%rcx)
	movq $1, %rax
	pushq %rax
	movq -20(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -64(%rbp)
	subq $24, %rsp
	movq 24(%rbp), %rax
	movq %rax, -88(%rbp)
	movq -64(%rbp), %rax
	movq %rax, -96(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $0, %rsp
	pushq %rax
	movl -24(%rbp), %eax
	popq %rcx
	movl %eax, 0(%rcx)
	movq $2, %rax
	pushq %rax
	movq -20(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -72(%rbp)
	movq $2, %rax
	pushq %rax
	movq -72(%rbp), %rax
	popq %rcx
	shlq %cl, %rax
	movq %rax, 32(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
	addq $20, %rsp
	jmp _rust_if_end_2
_rust_if_else_2:
	subq $0, %rsp
	addq $0, %rsp
_rust_if_end_2:
	movq -20(%rbp), %rax
	nop
	movl %eax, -24(%rbp)
	subq $28, %rsp
	movq 24(%rbp), %rax
	movq %rax, -72(%rbp)
	movq -20(%rbp), %rax
	movq %rax, -80(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $4, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl %eax, %eax
	movq %rax, -44(%rbp)
	movq -44(%rbp), %rax
	pushq %rax
	movq -20(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -52(%rbp)
	movq $1, %rax
	pushq %rax
	movq -52(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -20(%rbp)
	addq $24, %rsp
	jmp _rust_if_end_4
_rust_if_else_4:
	subq $8, %rsp
	movl $0, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	subq $24, %rsp
	movq 24(%rbp), %rax
	movq %rax, -56(%rbp)
	movq -20(%rbp), %rax
	movq %rax, -64(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $0, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl 0(%rsp), %ecx
	addq $4, %rsp
	cmpl %ecx, %eax
	setz %al
	testb %al, %al
	jz _rust_if_else_3
	subq $8, %rsp
	leaq _user_string....rust_string_file_4(%rip), %rdi
	movq $0, %rax
	subq $4, %rsp
	call _printf
	addq $4, %rsp
	subq $20, %rsp
	call _user_fun..std..allocator..const_zero
	addq $0, %rsp
	movq 0(%rsp), %rax
	movq %rax, -44(%rbp)
	addq $20, %rsp
	movq -44(%rbp), %rax
	pushq %rax
	movq $1, %rax
	popq %rcx
	leaq _division_by_zero_str(%rip), %r12
	testq %rcx, %rcx
	jz _panic
	xorq %rdx, %rdx
	divq %rcx
	addq $8, %rsp
	jmp _rust_if_end_3
_rust_if_else_3:
	subq $0, %rsp
	addq $0, %rsp
_rust_if_end_3:
	movq -20(%rbp), %rax
	nop
	movl %eax, -24(%rbp)
	subq $28, %rsp
	movq 24(%rbp), %rax
	movq %rax, -56(%rbp)
	movq -20(%rbp), %rax
	movq %rax, -64(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $4, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl %eax, %eax
	movq %rax, -36(%rbp)
	movq -36(%rbp), %rax
	pushq %rax
	movq -20(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -20(%rbp)
	addq $8, %rsp
_rust_if_end_4:
	addq $4, %rsp
	jmp _rust_while_cond_0
_rust_while_end_0:
	movq $0, %rax
	addq $24, %rsp
	movq %rax, 32(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..std..allocator..realloc:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	movq $-1, %rax
	pushq %rax
	movq 24(%rbp), %rax
	popq %rcx
	cmpq %rcx, %rax
	setz %al
	testb %al, %al
	jz _rust_if_else_9
	subq $0, %rsp
	subq $32, %rsp
	movq 32(%rbp), %rax
	movq %rax, -24(%rbp)
	movq 16(%rbp), %rax
	movq %rax, -32(%rbp)
	call _user_fun..std..allocator..malloc
	addq $16, %rsp
	popq %rax
	addq $8, %rsp
	jmp _rust_if_end_9
_rust_if_else_9:
	subq $0, %rsp
	movq $0, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rcx
	cmpq %rcx, %rax
	setz %al
	testb %al, %al
	jz _rust_if_else_8
	subq $0, %rsp
	subq $16, %rsp
	movq 32(%rbp), %rax
	movq %rax, -8(%rbp)
	movq 24(%rbp), %rax
	movq %rax, -16(%rbp)
	call _user_fun..std..allocator..free
	addq $16, %rsp
	addq $0, %rsp
	movq $-1, %rax
	addq $0, %rsp
	jmp _rust_if_end_8
_rust_if_else_8:
	subq $72, %rsp
	movq $2, %rax
	pushq %rax
	movq 24(%rbp), %rax
	popq %rcx
	shrq %cl, %rax
	movq %rax, -8(%rbp)
	movq $2, %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rcx
	subq %rcx, %rax
	movq %rax, -16(%rbp)
	subq $24, %rsp
	movq 32(%rbp), %rax
	movq %rax, -88(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -96(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $0, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl %eax, %eax
	movq %rax, -24(%rbp)
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -32(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	subq $36, %rsp
	movq 32(%rbp), %rax
	movq %rax, -104(%rbp)
	movq -32(%rbp), %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $12, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl 0(%rsp), %ecx
	addq $4, %rsp
	andl %ecx, %eax
	movl %eax, -36(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -36(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	cmpl %ecx, %eax
	setz %al
	testb %al, %al
	jz _rust_if_else_5
	subq $4, %rsp
	subq $36, %rsp
	movq 32(%rbp), %rax
	movq %rax, -104(%rbp)
	movq -32(%rbp), %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $12, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $0, %rsp
	movl %eax, 0(%rsp)
	subq $32, %rsp
	movq 32(%rbp), %rax
	movq %rax, -104(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $8, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl %eax, -76(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -76(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	subl %ecx, %eax
	addq $4, %rsp
	jmp _rust_if_end_5
_rust_if_else_5:
	subq $0, %rsp
	movl $2, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	subq $36, %rsp
	movq 32(%rbp), %rax
	movq %rax, -104(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $12, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl 0(%rsp), %ecx
	addq $4, %rsp
	subl %ecx, %eax
	addq $0, %rsp
_rust_if_end_5:
	movl %eax, -40(%rbp)
	movq $7, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -48(%rbp)
	movq $3, %rax
	pushq %rax
	movq -48(%rbp), %rax
	popq %rcx
	shrq %cl, %rax
	movq %rax, -56(%rbp)
	movq $1, %rax
	pushq %rax
	movq -56(%rbp), %rax
	popq %rcx
	shlq %cl, %rax
	movq %rax, -64(%rbp)
	movl -40(%rbp), %eax
	movl %eax, %eax
	movq %rax, -72(%rbp)
	movq 16(%rbp), %rax
	pushq %rax
	movq -72(%rbp), %rax
	popq %rcx
	cmpq %rcx, %rax
	setae %al
	testb %al, %al
	jz _rust_if_else_6
	subq $0, %rsp
	movb $0, %al
	addq $0, %rsp
	jmp _rust_if_end_6
_rust_if_else_6:
	subq $0, %rsp
	movb $0, %al
	addq $0, %rsp
_rust_if_end_6:
	testb %al, %al
	jz _rust_if_else_7
	subq $32, %rsp
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	subq $36, %rsp
	movq 32(%rbp), %rax
	movq %rax, -136(%rbp)
	movq -32(%rbp), %rax
	movq %rax, -144(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $12, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl 0(%rsp), %ecx
	addq $4, %rsp
	orl %ecx, %eax
	movl %eax, -76(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -76(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	addl %ecx, %eax
	movl %eax, -80(%rbp)
	movl -80(%rbp), %eax
	movl %eax, %eax
	movq %rax, -88(%rbp)
	movq -88(%rbp), %rax
	pushq %rax
	movq -32(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -96(%rbp)
	leaq _user_string....rust_string_file_4(%rip), %rdi
	movq $0, %rax
	subq $8, %rsp
	call _printf
	addq $8, %rsp
	subq $8, %rsp
	call _user_fun..std..allocator..const_zero
	addq $0, %rsp
	movq 0(%rsp), %rax
	movq %rax, -104(%rbp)
	addq $8, %rsp
	movq -104(%rbp), %rax
	pushq %rax
	movq $1, %rax
	popq %rcx
	leaq _division_by_zero_str(%rip), %r12
	testq %rcx, %rcx
	jz _panic
	xorq %rdx, %rdx
	divq %rcx
	addq $32, %rsp
	jmp _rust_if_end_7
_rust_if_else_7:
	subq $24, %rsp
	subq $32, %rsp
	movq 32(%rbp), %rax
	movq %rax, -120(%rbp)
	movq 16(%rbp), %rax
	movq %rax, -128(%rbp)
	call _user_fun..std..allocator..malloc
	addq $16, %rsp
	movq 0(%rsp), %rax
	movq %rax, -80(%rbp)
	addq $16, %rsp
	movq $2, %rax
	pushq %rax
	movq -80(%rbp), %rax
	popq %rcx
	shrq %cl, %rax
	movq %rax, -88(%rbp)
	movq $0, %rax
	movq %rax, -96(%rbp)
_rust_while_cond_1:
	movq $2, %rax
	pushq %rax
	subq $24, %rsp
	movq 32(%rbp), %rax
	movq %rax, -120(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -128(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $0, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl %eax, %eax
	popq %rcx
	subq %rcx, %rax
	pushq %rax
	movq -96(%rbp), %rax
	popq %rcx
	cmpq %rcx, %rax
	setb %al
	testb %al, %al
	jz _rust_while_end_1
	subq $24, %rsp
	movq -96(%rbp), %rax
	pushq %rax
	movq -88(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -104(%rbp)
	movq $2, %rax
	pushq %rax
	movq 24(%rbp), %rax
	popq %rcx
	shrq %cl, %rax
	movq %rax, -112(%rbp)
	movq -96(%rbp), %rax
	pushq %rax
	movq -112(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -120(%rbp)
	subq $24, %rsp
	movq 32(%rbp), %rax
	movq %rax, -136(%rbp)
	movq -104(%rbp), %rax
	movq %rax, -144(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $0, %rsp
	pushq %rax
	subq $32, %rsp
	movq 32(%rbp), %rax
	movq %rax, -152(%rbp)
	movq -120(%rbp), %rax
	movq %rax, -160(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $8, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movq -128(%rbp), %rcx
	movl 0(%rsp), %eax
	movl %eax, 0(%rcx)
	addq $12, %rsp
	movq $1, %rax
	pushq %rax
	movq -96(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -96(%rbp)
	addq $24, %rsp
	jmp _rust_while_cond_1
_rust_while_end_1:
	subq $16, %rsp
	movq 32(%rbp), %rax
	movq %rax, -104(%rbp)
	movq 24(%rbp), %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..allocator..free
	addq $16, %rsp
	addq $0, %rsp
	movq $2, %rax
	pushq %rax
	movq -88(%rbp), %rax
	popq %rcx
	shlq %cl, %rax
	addq $24, %rsp
_rust_if_end_7:
	addq $72, %rsp
_rust_if_end_8:
	addq $0, %rsp
_rust_if_end_9:
	addq $0, %rsp
	movq %rax, 40(%rbp)
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..std..allocator..free:
	pushq %rbp
	movq %rsp, %rbp
	subq $24, %rsp
	movq $2, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rcx
	shrq %cl, %rax
	movq %rax, -8(%rbp)
	movq $2, %rax
	pushq %rax
	movq -8(%rbp), %rax
	popq %rcx
	subq %rcx, %rax
	movq %rax, -16(%rbp)
	subq $24, %rsp
	movq 24(%rbp), %rax
	movq %rax, -40(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -48(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $0, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	nop
	movl %eax, -20(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -20(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	andl %ecx, %eax
	movl %eax, -24(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -24(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	cmpl %ecx, %eax
	setz %al
	testb %al, %al
	jz _rust_if_else_12
	subq $8, %rsp
	leaq _user_string....rust_string_file_4(%rip), %rdi
	movq $0, %rax
	subq $0, %rsp
	call _printf
	addq $0, %rsp
	subq $16, %rsp
	call _user_fun..std..allocator..const_zero
	addq $0, %rsp
	movq 0(%rsp), %rax
	movq %rax, -32(%rbp)
	addq $16, %rsp
	movq -32(%rbp), %rax
	pushq %rax
	movq $1, %rax
	popq %rcx
	leaq _division_by_zero_str(%rip), %r12
	testq %rcx, %rcx
	jz _panic
	xorq %rdx, %rdx
	divq %rcx
	addq $8, %rsp
	jmp _rust_if_end_12
_rust_if_else_12:
	subq $60, %rsp
	subq $28, %rsp
	movq 24(%rbp), %rax
	movq %rax, -104(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $4, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl %eax, %eax
	movq %rax, -32(%rbp)
	movq -32(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -40(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	subq $24, %rsp
	movq 24(%rbp), %rax
	movq %rax, -104(%rbp)
	movq -40(%rbp), %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $0, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl 0(%rsp), %ecx
	addq $4, %rsp
	andl %ecx, %eax
	movl %eax, -44(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -44(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	cmpl %ecx, %eax
	setz %al
	testb %al, %al
	jz _rust_if_else_10
	subq $16, %rsp
	subq $28, %rsp
	movq 24(%rbp), %rax
	movq %rax, -120(%rbp)
	movq -40(%rbp), %rax
	movq %rax, -128(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $4, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl %eax, %eax
	movq %rax, -92(%rbp)
	movq -92(%rbp), %rax
	pushq %rax
	movq -40(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -100(%rbp)
	movq $1, %rax
	pushq %rax
	movq -100(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -40(%rbp)
	addq $16, %rsp
	jmp _rust_if_end_10
_rust_if_else_10:
	subq $0, %rsp
	addq $0, %rsp
_rust_if_end_10:
	subq $28, %rsp
	movq 24(%rbp), %rax
	movq %rax, -104(%rbp)
	movq $1, %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $4, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl %eax, %eax
	movq %rax, -52(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	subq $24, %rsp
	movq 24(%rbp), %rax
	movq %rax, -104(%rbp)
	movq -52(%rbp), %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $0, %rsp
	subq $4, %rsp
	movl 0(%rax), %ecx
	movl %ecx, 0(%rsp)
	movl 0(%rsp), %eax
	addq $4, %rsp
	movl 0(%rsp), %ecx
	addq $4, %rsp
	andl %ecx, %eax
	movl %eax, -56(%rbp)
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -56(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	cmpl %ecx, %eax
	setz %al
	testb %al, %al
	jz _rust_if_else_11
	subq $0, %rsp
	movq -52(%rbp), %rax
	movq %rax, -16(%rbp)
	addq $0, %rsp
	jmp _rust_if_end_11
_rust_if_else_11:
	subq $0, %rsp
	addq $0, %rsp
_rust_if_end_11:
	movq $1, %rax
	pushq %rax
	movq -40(%rbp), %rax
	popq %rcx
	addq %rcx, %rax
	movq %rax, -64(%rbp)
	subq $28, %rsp
	movq 24(%rbp), %rax
	movq %rax, -104(%rbp)
	movq -64(%rbp), %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $4, %rsp
	pushq %rax
	movq -16(%rbp), %rax
	nop
	popq %rcx
	movl %eax, 0(%rcx)
	movq -16(%rbp), %rax
	pushq %rax
	movq -40(%rbp), %rax
	popq %rcx
	subq %rcx, %rax
	movq %rax, -72(%rbp)
	movq $2, %rax
	pushq %rax
	movq -72(%rbp), %rax
	popq %rcx
	subq %rcx, %rax
	movq %rax, -80(%rbp)
	movq -80(%rbp), %rax
	nop
	movl %eax, -84(%rbp)
	subq $28, %rsp
	movq 24(%rbp), %rax
	movq %rax, -104(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..vec..Vec..get
	addq $16, %rsp
	popq %rax
	addq $4, %rsp
	pushq %rax
	movl $1, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -84(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	orl %ecx, %eax
	popq %rcx
	movl %eax, 0(%rcx)
	addq $60, %rsp
_rust_if_end_12:
	addq $24, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..std..allocator..testing:
	pushq %rbp
	movq %rsp, %rbp
	subq $80, %rsp
	subq $168, %rsp
	subq $24, %rsp
	movq $4, %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..new
	addq $8, %rsp
	movq 0(%rsp), %rax
	movq %rax, -88(%rbp)
	addq $16, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -96(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -96(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -104(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -104(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -112(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -112(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -120(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -120(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -128(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -128(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -136(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -136(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -144(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -144(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -152(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -152(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -160(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -160(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -168(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -168(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -176(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -176(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -184(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -184(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -192(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -192(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -200(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -200(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -208(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -208(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -216(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -216(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -224(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -224(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -232(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -232(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -240(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -240(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	leaq -88(%rbp), %rax
	movq %rax, -248(%rbp)
	subq $24, %rsp
	movl $42, %eax
	movl %eax, -264(%rbp)
	movq -248(%rbp), %rax
	movq %rax, -272(%rbp)
	call _user_fun..std..vec..Vec..push
	addq $12, %rsp
	addq $12, %rsp
	movq -88(%rbp), %rax
	addq $168, %rsp
	movq %rax, -8(%rbp)
	leaq -8(%rbp), %rax
	movq %rax, -16(%rbp)
	subq $16, %rsp
	movq -16(%rbp), %rax
	movq %rax, -96(%rbp)
	call _user_fun..std..allocator..init
	addq $8, %rsp
	addq $8, %rsp
	leaq _user_string....rust_string_file_4(%rip), %rdi
	movq $0, %rax
	subq $0, %rsp
	call _printf
	addq $0, %rsp
	leaq -8(%rbp), %rax
	movq %rax, -24(%rbp)
	subq $32, %rsp
	movq -24(%rbp), %rax
	movq %rax, -104(%rbp)
	movq $2, %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..allocator..malloc
	addq $16, %rsp
	movq 0(%rsp), %rax
	movq %rax, -32(%rbp)
	addq $16, %rsp
	leaq _user_string....rust_string_file_4(%rip), %rdi
	movq $0, %rax
	subq $0, %rsp
	call _printf
	addq $0, %rsp
	leaq -8(%rbp), %rax
	movq %rax, -40(%rbp)
	subq $32, %rsp
	movq -40(%rbp), %rax
	movq %rax, -104(%rbp)
	movq $1, %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..allocator..malloc
	addq $16, %rsp
	movq 0(%rsp), %rax
	movq %rax, -48(%rbp)
	addq $16, %rsp
	leaq _user_string....rust_string_file_4(%rip), %rdi
	movq $0, %rax
	subq $0, %rsp
	call _printf
	addq $0, %rsp
	leaq -8(%rbp), %rax
	movq %rax, -56(%rbp)
	subq $32, %rsp
	movq -56(%rbp), %rax
	movq %rax, -104(%rbp)
	movq $3, %rax
	movq %rax, -112(%rbp)
	call _user_fun..std..allocator..malloc
	addq $16, %rsp
	movq 0(%rsp), %rax
	movq %rax, -64(%rbp)
	addq $16, %rsp
	leaq _user_string....rust_string_file_4(%rip), %rdi
	movq $0, %rax
	subq $0, %rsp
	call _printf
	addq $0, %rsp
	leaq -8(%rbp), %rax
	movq %rax, -72(%rbp)
	subq $16, %rsp
	movq -72(%rbp), %rax
	movq %rax, -88(%rbp)
	movq -48(%rbp), %rax
	movq %rax, -96(%rbp)
	call _user_fun..std..allocator..free
	addq $16, %rsp
	addq $0, %rsp
	leaq _user_string....rust_string_file_4(%rip), %rdi
	movq $0, %rax
	subq $0, %rsp
	call _printf
	addq $0, %rsp
	leaq -8(%rbp), %rax
	movq %rax, -80(%rbp)
	subq $16, %rsp
	movq -80(%rbp), %rax
	movq %rax, -88(%rbp)
	movq -64(%rbp), %rax
	movq %rax, -96(%rbp)
	call _user_fun..std..allocator..free
	addq $16, %rsp
	addq $0, %rsp
	leaq _user_string....rust_string_file_4(%rip), %rdi
	movq $0, %rax
	subq $0, %rsp
	call _printf
	addq $0, %rsp
	addq $80, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..crate..main:
	pushq %rbp
	movq %rsp, %rbp
	subq $4, %rsp
	subq $12, %rsp
	call _user_fun..crate..module1..exec1
	addq $0, %rsp
	addq $12, %rsp
	subq $12, %rsp
	call _user_fun..crate..module2..exec
	addq $0, %rsp
	addq $12, %rsp
	subq $12, %rsp
	call _user_fun..crate..module2..module3..exec_top
	addq $0, %rsp
	addq $12, %rsp
	movl $3, %eax
	movl %eax, -4(%rbp)
	movl $3, %eax
	subq $4, %rsp
	movl %eax, 0(%rsp)
	movl -4(%rbp), %eax
	movl 0(%rsp), %ecx
	addq $4, %rsp
	cmpl %ecx, %eax
	setz %al
	testb %al, %al
	jz _rust_if_else_13
	subq $0, %rsp
	leaq _user_string....rust_string_file_0(%rip), %rdi
	movq $0, %rax
	subq $12, %rsp
	call _printf
	addq $12, %rsp
	addq $0, %rsp
	jmp _rust_if_end_13
_rust_if_else_13:
	subq $0, %rsp
	addq $0, %rsp
_rust_if_end_13:
	addq $4, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..crate..module2..exec:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	subq $0, %rsp
	call _user_fun..crate..module2..module3..exec
	addq $0, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..crate..module2..module3..exec:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	leaq _user_string....rust_string_file_1(%rip), %rdi
	movq $0, %rax
	subq $0, %rsp
	call _printf
	addq $0, %rsp
	addq $0, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..crate..module2..module3..exec_top:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	leaq _user_string....rust_string_file_2(%rip), %rdi
	movq $0, %rax
	subq $0, %rsp
	call _printf
	addq $0, %rsp
	addq $0, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
_user_fun..crate..module1..exec1:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	leaq _user_string....rust_string_file_3(%rip), %rdi
	movq $0, %rax
	subq $0, %rsp
	call _printf
	addq $0, %rsp
	addq $0, %rsp
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
_my_string:
	.string "%zd\n"
_division_by_zero_str:
	.string "Division by zero\n"
_OoB_error:
	.string "Index out of bound\n"
_heap_address:
	.quad 0
_user_string....rust_string_file_2:
	.string "module 3 from far away\n"
_user_string....rust_string_file_3:
	.string "module1\n"
_user_string....rust_string_file_1:
	.string "module3\n"
_user_string....rust_string_file_4:
	.string "an ommited string"
_user_string....rust_string_file_0:
	.string "test\n"
