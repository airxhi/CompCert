# File generated by CompCert 3.5
# Command line: for_loop.c -dasm -c -o wtf
	.text
	.align	16
	.globl foo
foo:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset	8
	leaq	16(%rsp), %rax
	movq	%rax, 0(%rsp)
	xorl	%eax, %eax
	xorl	%ecx, %ecx
.L100:
	leal	0(%eax,%ecx,1), %eax
	leal	1(%ecx), %ecx
	cmpl	$10, %ecx
	jl	.L100
	addq	$8, %rsp
	ret
	.cfi_endproc
	.type	foo, @function
	.size	foo, . - foo
	.text
	.align	16
	.globl main
main:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset	8
	leaq	16(%rsp), %rax
	movq	%rax, 0(%rsp)
	call	foo
	xorl	%eax, %eax
	addq	$8, %rsp
	ret
	.cfi_endproc
	.type	main, @function
	.size	main, . - main