# File generated by CompCert 3.5
# Command line: call_add.c -c -o object_files/add.o
	.section	.rodata
	.align	1
__stringlit_1:
	.ascii	"%d\000"
	.type	__stringlit_1, @object
	.size	__stringlit_1, . - __stringlit_1
	.text
	.align	16
	.globl main
main:
	.cfi_startproc
[debug]	Pfreeframe
	movl	$2, %edi
	movl	$4, %esi
	call	add
	movq	%rax, %rsi
	leaq	__stringlit_1(%rip), %rdi
	call	printf
	xorl	%eax, %eax
[debug]	Pfreeframe
	ret
	.cfi_endproc
	.type	main, @function
	.size	main, . - main
