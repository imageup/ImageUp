	.text
	.file	"test.ll"
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp0:
	.cfi_def_cfa_offset 16
	leaq	.Lfmt_str(%rip), %rdi
	leaq	.Lsystem_string(%rip), %rsi
	xorl	%eax, %eax
	callq	prints@PLT
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc

	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.1,@object         # @fmt.1
.Lfmt.1:
	.asciz	"%g\n"
	.size	.Lfmt.1, 4

	.type	.Lfmt_str,@object       # @fmt_str
.Lfmt_str:
	.asciz	"%s\n"
	.size	.Lfmt_str, 4

	.type	.Lsystem_string,@object # @system_string
.Lsystem_string:
	.asciz	"hello world!"
	.size	.Lsystem_string, 13


	.section	".note.GNU-stack","",@progbits
