# File generated by CompCert 3.5
# Command line: -dasm test.c test_lib.c -o clean/wtf
	.section	.rodata
	.align	1
__stringlit_1:
	.ascii	"%d\012\000"
	.type	__stringlit_1, @object
	.size	__stringlit_1, . - __stringlit_1
	.text
	.align	16
	.globl foo
foo:
	.cfi_startproc
	subq	$8, %rsp
	leaq	16(%rsp), %rax
	movq	%rax, 0(%rsp)
	leal	0(%edi,%esi,1), %eax
	addq	$8, %rsp
	ret
	.cfi_endproc
	.type	foo, @function
	.size	foo, . - foo
	.text
	.align	16
	.globl bar
bar:
	.cfi_startproc
	subq	$8, %rsp
	leaq	16(%rsp), %rax
	movq	%rax, 0(%rsp)
	testl	%edi, %edi
	jge	.L103
	xorl	%eax, %eax
	jmp	.L104
.L103:
	xorl	%ecx, %ecx
	xorl	%eax, %eax
.L105:
	cmpl	%edi, %ecx
	jge	.L104
	leal	0(%eax,%ecx,1), %eax
	leal	1(%ecx), %ecx
	jmp	.L105
.L104:
	addq	$8, %rsp
	ret
	.cfi_endproc
	.type	bar, @function
	.size	bar, . - bar
	.text
	.align	16
	.globl main
main:
	.cfi_startproc
	subq	$56, %rsp
	leaq	64(%rsp), %rax
	movq	%rax, 0(%rsp)
	movq	%rbx, 8(%rsp)
	movl	$1, %edi
	movl	$2, %esi
	call	foo
	movl	$5, %edi
	movl	$3, %esi
	call	extr
	movq	%rax, %rbx
	leaq	__stringlit_1(%rip), %rdi
	movq	%rbx, %rsi
	movl	$0, %eax
	call	printf
	movq	%rbx, %rax
	movq	8(%rsp), %rbx
	addq	$56, %rsp
	ret
	.cfi_endproc
	.type	main, @function
	.size	main, . - main
