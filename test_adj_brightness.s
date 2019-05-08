	.text
	.file	"test_adj_brightness.ll"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4626322717216342016     # double 20
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp0:
	.cfi_def_cfa_offset 32
	leaq	.Lpath_name(%rip), %rdi
	callq	read_c@PLT
	movq	%rax, 16(%rsp)
	movsd	.LCPI0_0(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%rax, %rdi
	callq	adjust_brightness@PLT
	movq	%rax, 8(%rsp)
	leaq	.Lpath_name.3(%rip), %rdi
	movq	%rax, %rsi
	callq	save_c@PLT
	xorl	%eax, %eax
	addq	$24, %rsp
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc

	.globl	beautify
	.align	16, 0x90
	.type	beautify,@function
beautify:                               # @beautify
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$40, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 48
	movq	%rdi, 32(%rsp)
	movsd	%xmm0, 24(%rsp)
	movsd	%xmm1, 16(%rsp)
	movsd	%xmm2, 8(%rsp)
	movsd	%xmm3, (%rsp)
	movsd	24(%rsp), %xmm0         # xmm0 = mem[0],zero
	movq	32(%rsp), %rdi
	callq	smooth_c@PLT
	movq	%rax, 32(%rsp)
	movsd	16(%rsp), %xmm0         # xmm0 = mem[0],zero
	movq	%rax, %rdi
	callq	adjust_brightness@PLT
	movq	%rax, 32(%rsp)
	movsd	8(%rsp), %xmm0          # xmm0 = mem[0],zero
	movq	%rax, %rdi
	callq	adjust_contrast@PLT
	movq	%rax, 32(%rsp)
	movsd	(%rsp), %xmm0           # xmm0 = mem[0],zero
	movq	%rax, %rdi
	callq	saturation_c@PLT
	movq	%rax, 32(%rsp)
	addq	$40, %rsp
	retq
.Lfunc_end1:
	.size	beautify, .Lfunc_end1-beautify
	.cfi_endproc

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI2_0:
	.quad	4599075939470750515     # double 0.29999999999999999
.LCPI2_1:
	.quad	4603489467105573601     # double 0.58999999999999997
.LCPI2_2:
	.quad	4592590756007337001     # double 0.11
	.text
	.globl	to_gray
	.align	16, 0x90
	.type	to_gray,@function
to_gray:                                # @to_gray
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp2:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp3:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp4:
	.cfi_def_cfa_offset 96
.Ltmp5:
	.cfi_offset %rbx, -24
.Ltmp6:
	.cfi_offset %r14, -16
	movq	%rdi, 64(%rsp)
	callq	size_c@PLT
	movsd	(%rax), %xmm0           # xmm0 = mem[0],zero
	movsd	%xmm0, 16(%rsp)         # 8-byte Spill
	movsd	8(%rax), %xmm0          # xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rsp)          # 8-byte Spill
	movl	$24, %edi
	callq	malloc@PLT
	movsd	16(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	movsd	8(%rsp), %xmm0          # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rax)
	movq	$0, 16(%rax)
	movq	%rax, 40(%rsp)
	movq	64(%rsp), %rdi
	callq	copy_c@PLT
	movq	%rax, 24(%rsp)
	movl	$0, 52(%rsp)
	jmp	.LBB2_1
	.align	16, 0x90
.LBB2_5:                                # %merge
                                        #   in Loop: Header=BB2_1 Depth=1
	incl	52(%rsp)
.LBB2_1:                                # %while
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_3 Depth 2
	movq	40(%rsp), %rax
	cvttsd2si	(%rax), %eax
	cmpl	%eax, 52(%rsp)
	jge	.LBB2_6
# BB#2:                                 # %while_body
                                        #   in Loop: Header=BB2_1 Depth=1
	movl	$0, 48(%rsp)
	jmp	.LBB2_3
	.align	16, 0x90
.LBB2_4:                                # %while_body5
                                        #   in Loop: Header=BB2_3 Depth=2
	movq	64(%rsp), %rbx
	cvtsi2sdl	52(%rsp), %xmm0
	movsd	%xmm0, 16(%rsp)         # 8-byte Spill
	xorps	%xmm0, %xmm0
	cvtsi2sdl	48(%rsp), %xmm0
	movsd	%xmm0, 8(%rsp)          # 8-byte Spill
	movl	$24, %edi
	callq	malloc@PLT
	movsd	16(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	movsd	8(%rsp), %xmm0          # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rax)
	movq	$0, 16(%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	get_pixel_c@PLT
	movq	%rax, 32(%rsp)
	movsd	(%rax), %xmm0           # xmm0 = mem[0],zero
	mulsd	.LCPI2_0(%rip), %xmm0
	movsd	8(%rax), %xmm1          # xmm1 = mem[0],zero
	mulsd	.LCPI2_1(%rip), %xmm1
	addsd	%xmm0, %xmm1
	movsd	16(%rax), %xmm0         # xmm0 = mem[0],zero
	mulsd	.LCPI2_2(%rip), %xmm0
	addsd	%xmm1, %xmm0
	movsd	%xmm0, 56(%rsp)
	movq	24(%rsp), %r14
	xorps	%xmm0, %xmm0
	cvtsi2sdl	52(%rsp), %xmm0
	movsd	%xmm0, 16(%rsp)         # 8-byte Spill
	xorps	%xmm0, %xmm0
	cvtsi2sdl	48(%rsp), %xmm0
	movsd	%xmm0, 8(%rsp)          # 8-byte Spill
	movl	$24, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movsd	16(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, (%rbx)
	movsd	8(%rsp), %xmm0          # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rbx)
	movq	$0, 16(%rbx)
	movsd	56(%rsp), %xmm0         # xmm0 = mem[0],zero
	movsd	%xmm0, 16(%rsp)         # 8-byte Spill
	movl	$24, %edi
	callq	malloc@PLT
	movsd	16(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	movsd	%xmm0, 8(%rax)
	movsd	%xmm0, 16(%rax)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	write_pixel_c@PLT
	incl	48(%rsp)
.LBB2_3:                                # %while4
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%rsp), %rax
	cvttsd2si	8(%rax), %eax
	cmpl	%eax, 48(%rsp)
	jl	.LBB2_4
	jmp	.LBB2_5
.LBB2_6:                                # %merge56
	movq	24(%rsp), %rax
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.Lfunc_end2:
	.size	to_gray, .Lfunc_end2-to_gray
	.cfi_endproc

	.globl	adjust_contrast
	.align	16, 0x90
	.type	adjust_contrast,@function
adjust_contrast:                        # @adjust_contrast
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp7:
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	movsd	%xmm0, 8(%rsp)
	movq	16(%rsp), %rdi
	xorps	%xmm1, %xmm1
	callq	adjust_image@PLT
	addq	$24, %rsp
	retq
.Lfunc_end3:
	.size	adjust_contrast, .Lfunc_end3-adjust_contrast
	.cfi_endproc

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI4_0:
	.quad	4607182418800017408     # double 1
	.text
	.globl	adjust_brightness
	.align	16, 0x90
	.type	adjust_brightness,@function
adjust_brightness:                      # @adjust_brightness
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp8:
	.cfi_def_cfa_offset 32
	movapd	%xmm0, %xmm1
	movq	%rdi, 16(%rsp)
	movsd	%xmm1, 8(%rsp)
	movq	16(%rsp), %rdi
	movsd	.LCPI4_0(%rip), %xmm0   # xmm0 = mem[0],zero
	callq	adjust_image@PLT
	addq	$24, %rsp
	retq
.Lfunc_end4:
	.size	adjust_brightness, .Lfunc_end4-adjust_brightness
	.cfi_endproc

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI5_0:
	.quad	4643176031446892544     # double 255
	.text
	.globl	adjust_image
	.align	16, 0x90
	.type	adjust_image,@function
adjust_image:                           # @adjust_image
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp9:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp10:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp11:
	.cfi_def_cfa_offset 32
	subq	$112, %rsp
.Ltmp12:
	.cfi_def_cfa_offset 144
.Ltmp13:
	.cfi_offset %rbx, -32
.Ltmp14:
	.cfi_offset %r14, -24
.Ltmp15:
	.cfi_offset %r15, -16
	movq	%rdi, 104(%rsp)
	movsd	%xmm0, 96(%rsp)
	movsd	%xmm1, 88(%rsp)
	movq	104(%rsp), %rdi
	callq	size_c@PLT
	movsd	(%rax), %xmm0           # xmm0 = mem[0],zero
	movsd	%xmm0, 24(%rsp)         # 8-byte Spill
	movsd	8(%rax), %xmm0          # xmm0 = mem[0],zero
	movsd	%xmm0, 16(%rsp)         # 8-byte Spill
	movl	$24, %edi
	callq	malloc@PLT
	movsd	24(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	movsd	16(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rax)
	movq	$0, 16(%rax)
	movq	%rax, 48(%rsp)
	movq	104(%rsp), %rdi
	callq	copy_c@PLT
	movq	%rax, 32(%rsp)
	movl	$0, 60(%rsp)
	movabsq	$4643176031446892544, %r15 # imm = 0x406FE00000000000
	jmp	.LBB5_1
	.align	16, 0x90
.LBB5_12:                               # %merge84
                                        #   in Loop: Header=BB5_1 Depth=1
	incl	60(%rsp)
.LBB5_1:                                # %while
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_3 Depth 2
	movq	48(%rsp), %rax
	cvttsd2si	(%rax), %eax
	cmpl	%eax, 60(%rsp)
	jge	.LBB5_13
# BB#2:                                 # %while_body
                                        #   in Loop: Header=BB5_1 Depth=1
	movl	$0, 56(%rsp)
	jmp	.LBB5_3
	.align	16, 0x90
.LBB5_10:                               # %merge59
                                        #   in Loop: Header=BB5_3 Depth=2
	movq	32(%rsp), %r14
	xorps	%xmm0, %xmm0
	cvtsi2sdl	60(%rsp), %xmm0
	movsd	%xmm0, 24(%rsp)         # 8-byte Spill
	xorps	%xmm0, %xmm0
	cvtsi2sdl	56(%rsp), %xmm0
	movsd	%xmm0, 16(%rsp)         # 8-byte Spill
	movl	$24, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movsd	24(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, (%rbx)
	movsd	16(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rbx)
	movq	$0, 16(%rbx)
	movsd	80(%rsp), %xmm0         # xmm0 = mem[0],zero
	movsd	%xmm0, 24(%rsp)         # 8-byte Spill
	movsd	72(%rsp), %xmm0         # xmm0 = mem[0],zero
	movsd	%xmm0, 16(%rsp)         # 8-byte Spill
	movsd	64(%rsp), %xmm0         # xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rsp)          # 8-byte Spill
	movl	$24, %edi
	callq	malloc@PLT
	movsd	24(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	movsd	16(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rax)
	movsd	8(%rsp), %xmm0          # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, 16(%rax)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	write_pixel_c@PLT
	incl	56(%rsp)
.LBB5_3:                                # %while6
                                        #   Parent Loop BB5_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	48(%rsp), %rax
	cvttsd2si	8(%rax), %eax
	cmpl	%eax, 56(%rsp)
	jge	.LBB5_12
# BB#4:                                 # %while_body7
                                        #   in Loop: Header=BB5_3 Depth=2
	movq	104(%rsp), %rbx
	cvtsi2sdl	60(%rsp), %xmm0
	movsd	%xmm0, 24(%rsp)         # 8-byte Spill
	xorps	%xmm0, %xmm0
	cvtsi2sdl	56(%rsp), %xmm0
	movsd	%xmm0, 16(%rsp)         # 8-byte Spill
	movl	$24, %edi
	callq	malloc@PLT
	movsd	24(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, (%rax)
	movsd	16(%rsp), %xmm0         # 8-byte Reload
                                        # xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rax)
	movq	$0, 16(%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	get_pixel_c@PLT
	movq	%rax, 40(%rsp)
	movsd	96(%rsp), %xmm0         # xmm0 = mem[0],zero
	mulsd	16(%rax), %xmm0
	addsd	88(%rsp), %xmm0
	movsd	%xmm0, 80(%rsp)
	ucomisd	.LCPI5_0(%rip), %xmm0
	jbe	.LBB5_5
# BB#14:                                # %then
                                        #   in Loop: Header=BB5_3 Depth=2
	movq	%r15, 80(%rsp)
.LBB5_5:                                # %merge
                                        #   in Loop: Header=BB5_3 Depth=2
	xorpd	%xmm1, %xmm1
	ucomisd	80(%rsp), %xmm1
	jbe	.LBB5_6
# BB#15:                                # %then26
                                        #   in Loop: Header=BB5_3 Depth=2
	movq	$0, 80(%rsp)
.LBB5_6:                                # %merge25
                                        #   in Loop: Header=BB5_3 Depth=2
	movsd	96(%rsp), %xmm0         # xmm0 = mem[0],zero
	movq	40(%rsp), %rax
	mulsd	8(%rax), %xmm0
	addsd	88(%rsp), %xmm0
	movsd	%xmm0, 72(%rsp)
	ucomisd	.LCPI5_0(%rip), %xmm0
	jbe	.LBB5_7
# BB#16:                                # %then38
                                        #   in Loop: Header=BB5_3 Depth=2
	movq	%r15, 72(%rsp)
.LBB5_7:                                # %merge37
                                        #   in Loop: Header=BB5_3 Depth=2
	ucomisd	72(%rsp), %xmm1
	jbe	.LBB5_8
# BB#17:                                # %then43
                                        #   in Loop: Header=BB5_3 Depth=2
	movq	$0, 72(%rsp)
.LBB5_8:                                # %merge42
                                        #   in Loop: Header=BB5_3 Depth=2
	movsd	96(%rsp), %xmm0         # xmm0 = mem[0],zero
	movq	40(%rsp), %rax
	mulsd	(%rax), %xmm0
	addsd	88(%rsp), %xmm0
	movsd	%xmm0, 64(%rsp)
	ucomisd	.LCPI5_0(%rip), %xmm0
	jbe	.LBB5_9
# BB#18:                                # %then55
                                        #   in Loop: Header=BB5_3 Depth=2
	movq	%r15, 64(%rsp)
.LBB5_9:                                # %merge54
                                        #   in Loop: Header=BB5_3 Depth=2
	ucomisd	64(%rsp), %xmm1
	jbe	.LBB5_10
# BB#11:                                # %then60
                                        #   in Loop: Header=BB5_3 Depth=2
	movq	$0, 64(%rsp)
	jmp	.LBB5_10
.LBB5_13:                               # %merge93
	movq	32(%rsp), %rax
	addq	$112, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Lfunc_end5:
	.size	adjust_image, .Lfunc_end5-adjust_image
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

	.type	.Lfmt.2,@object         # @fmt.2
.Lfmt.2:
	.asciz	"%s\n"
	.size	.Lfmt.2, 4

	.type	.Lpath_name,@object     # @path_name
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lpath_name:
	.asciz	"./images/face1.jpg"
	.size	.Lpath_name, 19

	.type	.Lpath_name.3,@object   # @path_name.3
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lpath_name.3:
	.asciz	"test.out.jpg"
	.size	.Lpath_name.3, 13

	.type	.Lfmt.4,@object         # @fmt.4
.Lfmt.4:
	.asciz	"%d\n"
	.size	.Lfmt.4, 4

	.type	.Lfmt.5,@object         # @fmt.5
.Lfmt.5:
	.asciz	"%g\n"
	.size	.Lfmt.5, 4

	.type	.Lfmt.6,@object         # @fmt.6
.Lfmt.6:
	.asciz	"%s\n"
	.size	.Lfmt.6, 4

	.type	.Lfmt.7,@object         # @fmt.7
.Lfmt.7:
	.asciz	"%d\n"
	.size	.Lfmt.7, 4

	.type	.Lfmt.8,@object         # @fmt.8
.Lfmt.8:
	.asciz	"%g\n"
	.size	.Lfmt.8, 4

	.type	.Lfmt.9,@object         # @fmt.9
.Lfmt.9:
	.asciz	"%s\n"
	.size	.Lfmt.9, 4

	.type	.Lfmt.10,@object        # @fmt.10
.Lfmt.10:
	.asciz	"%d\n"
	.size	.Lfmt.10, 4

	.type	.Lfmt.11,@object        # @fmt.11
.Lfmt.11:
	.asciz	"%g\n"
	.size	.Lfmt.11, 4

	.type	.Lfmt.12,@object        # @fmt.12
.Lfmt.12:
	.asciz	"%s\n"
	.size	.Lfmt.12, 4

	.type	.Lfmt.13,@object        # @fmt.13
.Lfmt.13:
	.asciz	"%d\n"
	.size	.Lfmt.13, 4

	.type	.Lfmt.14,@object        # @fmt.14
.Lfmt.14:
	.asciz	"%g\n"
	.size	.Lfmt.14, 4

	.type	.Lfmt.15,@object        # @fmt.15
.Lfmt.15:
	.asciz	"%s\n"
	.size	.Lfmt.15, 4

	.type	.Lfmt.16,@object        # @fmt.16
.Lfmt.16:
	.asciz	"%d\n"
	.size	.Lfmt.16, 4

	.type	.Lfmt.17,@object        # @fmt.17
.Lfmt.17:
	.asciz	"%g\n"
	.size	.Lfmt.17, 4

	.type	.Lfmt.18,@object        # @fmt.18
.Lfmt.18:
	.asciz	"%s\n"
	.size	.Lfmt.18, 4


	.section	".note.GNU-stack","",@progbits
