	.file "geometry.pas"
# Begin asmlist al_begin
# End asmlist al_begin
# Begin asmlist al_stabs
# End asmlist al_stabs
# Begin asmlist al_procedures

.section .text
	.balign 8,0x90
.globl	GEOMETRY_plus$TVECTOR3$TVECTOR3$$TVECTOR3
	.type	GEOMETRY_plus$TVECTOR3$TVECTOR3$$TVECTOR3,@function
GEOMETRY_plus$TVECTOR3$TVECTOR3$$TVECTOR3:
.Lc1:
# Temps allocated between rbp-24 and rbp-8
# [geometry.pas]
# [100] begin
	pushq	%rbp
.Lc3:
.Lc4:
	movq	%rsp,%rbp
.Lc5:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp+40
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [101] Result.X := A.X + B.X;
	movsd	16(%rbp),%xmm0
	addsd	40(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,(%rax)
# [102] Result.Y := A.Y + B.Y;
	movsd	24(%rbp),%xmm0
	addsd	48(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [103] Result.Z := A.Z + B.Z;
	movsd	32(%rbp),%xmm0
	addsd	56(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [104] end;
	leave
	ret
.Lc2:
.Le0:
	.size	GEOMETRY_plus$TVECTOR3$TVECTOR3$$TVECTOR3, .Le0 - GEOMETRY_plus$TVECTOR3$TVECTOR3$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_plus$TVECTOR4$TVECTOR4$$TVECTOR4
	.type	GEOMETRY_plus$TVECTOR4$TVECTOR4$$TVECTOR4,@function
GEOMETRY_plus$TVECTOR4$TVECTOR4$$TVECTOR4:
.Lc6:
# Temps allocated between rbp-24 and rbp-8
# [107] begin
	pushq	%rbp
.Lc8:
.Lc9:
	movq	%rsp,%rbp
.Lc10:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp+48
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [108] Result.X := A.X + B.X;
	movsd	16(%rbp),%xmm0
	addsd	48(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,(%rax)
# [109] Result.Y := A.Y + B.Y;
	movsd	24(%rbp),%xmm0
	addsd	56(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [110] Result.Z := A.Z + B.Z;
	movsd	32(%rbp),%xmm0
	addsd	64(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [111] Result.W := A.W + B.W;
	movsd	40(%rbp),%xmm0
	addsd	72(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,24(%rax)
# [112] end;
	leave
	ret
.Lc7:
.Le1:
	.size	GEOMETRY_plus$TVECTOR4$TVECTOR4$$TVECTOR4, .Le1 - GEOMETRY_plus$TVECTOR4$TVECTOR4$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_minus$TVECTOR3$TVECTOR3$$TVECTOR3
	.type	GEOMETRY_minus$TVECTOR3$TVECTOR3$$TVECTOR3,@function
GEOMETRY_minus$TVECTOR3$TVECTOR3$$TVECTOR3:
.Lc11:
# Temps allocated between rbp-24 and rbp-8
# [115] begin
	pushq	%rbp
.Lc13:
.Lc14:
	movq	%rsp,%rbp
.Lc15:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp+40
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [116] Result.X := A.X - B.X;
	movsd	16(%rbp),%xmm0
	subsd	40(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,(%rax)
# [117] Result.Y := A.Y - B.Y;
	movsd	24(%rbp),%xmm0
	subsd	48(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [118] Result.Z := A.Z - B.Z;
	movsd	32(%rbp),%xmm0
	subsd	56(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [119] end;
	leave
	ret
.Lc12:
.Le2:
	.size	GEOMETRY_minus$TVECTOR3$TVECTOR3$$TVECTOR3, .Le2 - GEOMETRY_minus$TVECTOR3$TVECTOR3$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_minus$TVECTOR4$TVECTOR4$$TVECTOR4
	.type	GEOMETRY_minus$TVECTOR4$TVECTOR4$$TVECTOR4,@function
GEOMETRY_minus$TVECTOR4$TVECTOR4$$TVECTOR4:
.Lc16:
# Temps allocated between rbp-24 and rbp-8
# [122] begin
	pushq	%rbp
.Lc18:
.Lc19:
	movq	%rsp,%rbp
.Lc20:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp+48
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [123] Result.X := A.X - B.X;
	movsd	16(%rbp),%xmm0
	subsd	48(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,(%rax)
# [124] Result.Y := A.Y - B.Y;
	movsd	24(%rbp),%xmm0
	subsd	56(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [125] Result.Z := A.Z - B.Z;
	movsd	32(%rbp),%xmm0
	subsd	64(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [126] Result.W := A.W - B.W;
	movsd	40(%rbp),%xmm0
	subsd	72(%rbp),%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,24(%rax)
# [127] end;
	leave
	ret
.Lc17:
.Le3:
	.size	GEOMETRY_minus$TVECTOR4$TVECTOR4$$TVECTOR4, .Le3 - GEOMETRY_minus$TVECTOR4$TVECTOR4$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_minus$TVECTOR3$$TVECTOR3
	.type	GEOMETRY_minus$TVECTOR3$$TVECTOR3,@function
GEOMETRY_minus$TVECTOR3$$TVECTOR3:
.Lc21:
# Temps allocated between rbp-24 and rbp-8
# [130] begin
	pushq	%rbp
.Lc23:
.Lc24:
	movq	%rsp,%rbp
.Lc25:
	subq	$32,%rsp
# Var A located at rbp+16
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [131] Result.X := -A.X;
	movsd	16(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld1,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,(%rax)
# [132] Result.Y := -A.Y;
	movsd	24(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld2,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [133] Result.Z := -A.Z;
	movsd	32(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld3,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [134] end;
	leave
	ret
.Lc22:
.Le4:
	.size	GEOMETRY_minus$TVECTOR3$$TVECTOR3, .Le4 - GEOMETRY_minus$TVECTOR3$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_minus$TVECTOR4$$TVECTOR4
	.type	GEOMETRY_minus$TVECTOR4$$TVECTOR4,@function
GEOMETRY_minus$TVECTOR4$$TVECTOR4:
.Lc26:
# Temps allocated between rbp-24 and rbp-8
# [137] begin
	pushq	%rbp
.Lc28:
.Lc29:
	movq	%rsp,%rbp
.Lc30:
	subq	$32,%rsp
# Var A located at rbp+16
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [138] Result.X := -A.X;
	movsd	16(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld4,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,(%rax)
# [139] Result.Y := -A.Y;
	movsd	24(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld5,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [140] Result.Z := -A.Z;
	movsd	32(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld6,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [141] Result.W := -A.W;
	movsd	40(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld7,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,24(%rax)
# [142] end;
	leave
	ret
.Lc27:
.Le5:
	.size	GEOMETRY_minus$TVECTOR4$$TVECTOR4, .Le5 - GEOMETRY_minus$TVECTOR4$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_star$TVECTOR3$TVECTOR3$$SINGLE
	.type	GEOMETRY_star$TVECTOR3$TVECTOR3$$SINGLE,@function
GEOMETRY_star$TVECTOR3$TVECTOR3$$SINGLE:
.Lc31:
# Temps allocated between rbp-24 and rbp-4
# [145] begin
	pushq	%rbp
.Lc33:
.Lc34:
	movq	%rsp,%rbp
.Lc35:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp+40
# Var $result located at rbp-4
# [146] Result := A.X * B.X +
	movsd	16(%rbp),%xmm0
	mulsd	40(%rbp),%xmm0
# [147] A.Y * B.Y +
	movsd	24(%rbp),%xmm1
	mulsd	48(%rbp),%xmm1
	addsd	%xmm0,%xmm1
# [148] A.Z * B.Z;
	movsd	32(%rbp),%xmm0
	mulsd	56(%rbp),%xmm0
	addsd	%xmm1,%xmm0
	cvtsd2ss	%xmm0,%xmm0
	movss	%xmm0,-4(%rbp)
# [149] end;
	movss	-4(%rbp),%xmm0
	leave
	ret
.Lc32:
.Le6:
	.size	GEOMETRY_star$TVECTOR3$TVECTOR3$$SINGLE, .Le6 - GEOMETRY_star$TVECTOR3$TVECTOR3$$SINGLE

.section .text
	.balign 8,0x90
.globl	GEOMETRY_star$TVECTOR4$TVECTOR4$$SINGLE
	.type	GEOMETRY_star$TVECTOR4$TVECTOR4$$SINGLE,@function
GEOMETRY_star$TVECTOR4$TVECTOR4$$SINGLE:
.Lc36:
# Temps allocated between rbp-24 and rbp-4
# [152] begin
	pushq	%rbp
.Lc38:
.Lc39:
	movq	%rsp,%rbp
.Lc40:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp+48
# Var $result located at rbp-4
# [153] Result := A.X * B.X +
	movsd	16(%rbp),%xmm0
	mulsd	48(%rbp),%xmm0
# [154] A.Y * B.Y +
	movsd	24(%rbp),%xmm1
	mulsd	56(%rbp),%xmm1
	addsd	%xmm0,%xmm1
# [155] A.Z * B.Z +
	movsd	32(%rbp),%xmm2
	mulsd	64(%rbp),%xmm2
	addsd	%xmm1,%xmm2
# [156] A.W * B.W;
	movsd	40(%rbp),%xmm0
	mulsd	72(%rbp),%xmm0
	addsd	%xmm2,%xmm0
	cvtsd2ss	%xmm0,%xmm0
	movss	%xmm0,-4(%rbp)
# [157] end;
	movss	-4(%rbp),%xmm0
	leave
	ret
.Lc37:
.Le7:
	.size	GEOMETRY_star$TVECTOR4$TVECTOR4$$SINGLE, .Le7 - GEOMETRY_star$TVECTOR4$TVECTOR4$$SINGLE

.section .text
	.balign 8,0x90
.globl	GEOMETRY_star$TVECTOR3$DOUBLE$$TVECTOR3
	.type	GEOMETRY_star$TVECTOR3$DOUBLE$$TVECTOR3,@function
GEOMETRY_star$TVECTOR3$DOUBLE$$TVECTOR3:
.Lc41:
# Temps allocated between rbp-32 and rbp-16
# [160] begin
	pushq	%rbp
.Lc43:
.Lc44:
	movq	%rsp,%rbp
.Lc45:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp-8
# Var $result located at rbp-16
	movq	%rdi,-16(%rbp)
	movsd	%xmm0,-8(%rbp)
# [161] Result.X := A.X * B;
	movsd	16(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,(%rax)
# [162] Result.Y := A.Y * B;
	movsd	24(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [163] Result.Z := A.Z * B;
	movsd	32(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [164] end;
	leave
	ret
.Lc42:
.Le8:
	.size	GEOMETRY_star$TVECTOR3$DOUBLE$$TVECTOR3, .Le8 - GEOMETRY_star$TVECTOR3$DOUBLE$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_star$TVECTOR4$DOUBLE$$TVECTOR4
	.type	GEOMETRY_star$TVECTOR4$DOUBLE$$TVECTOR4,@function
GEOMETRY_star$TVECTOR4$DOUBLE$$TVECTOR4:
.Lc46:
# Temps allocated between rbp-32 and rbp-16
# [167] begin
	pushq	%rbp
.Lc48:
.Lc49:
	movq	%rsp,%rbp
.Lc50:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp-8
# Var $result located at rbp-16
	movq	%rdi,-16(%rbp)
	movsd	%xmm0,-8(%rbp)
# [168] Result.X := A.X * B;
	movsd	16(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,(%rax)
# [169] Result.Y := A.Y * B;
	movsd	24(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [170] Result.Z := A.Z * B;
	movsd	32(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [171] Result.W := A.W * B;
	movsd	40(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,24(%rax)
# [172] end;
	leave
	ret
.Lc47:
.Le9:
	.size	GEOMETRY_star$TVECTOR4$DOUBLE$$TVECTOR4, .Le9 - GEOMETRY_star$TVECTOR4$DOUBLE$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_star$DOUBLE$TVECTOR3$$TVECTOR3
	.type	GEOMETRY_star$DOUBLE$TVECTOR3$$TVECTOR3,@function
GEOMETRY_star$DOUBLE$TVECTOR3$$TVECTOR3:
.Lc51:
# Temps allocated between rbp-32 and rbp-16
# [175] begin
	pushq	%rbp
.Lc53:
.Lc54:
	movq	%rsp,%rbp
.Lc55:
	subq	$32,%rsp
# Var A located at rbp-8
# Var B located at rbp+16
# Var $result located at rbp-16
	movq	%rdi,-16(%rbp)
	movsd	%xmm0,-8(%rbp)
# [176] Result := B * A;
	movsd	16(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,(%rax)
	movsd	24(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,8(%rax)
	movsd	32(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,16(%rax)
	movq	-16(%rbp),%rax
# [177] end;
	leave
	ret
.Lc52:
.Le10:
	.size	GEOMETRY_star$DOUBLE$TVECTOR3$$TVECTOR3, .Le10 - GEOMETRY_star$DOUBLE$TVECTOR3$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_star$DOUBLE$TVECTOR4$$TVECTOR4
	.type	GEOMETRY_star$DOUBLE$TVECTOR4$$TVECTOR4,@function
GEOMETRY_star$DOUBLE$TVECTOR4$$TVECTOR4:
.Lc56:
# Temps allocated between rbp-32 and rbp-16
# [180] begin
	pushq	%rbp
.Lc58:
.Lc59:
	movq	%rsp,%rbp
.Lc60:
	subq	$32,%rsp
# Var A located at rbp-8
# Var B located at rbp+16
# Var $result located at rbp-16
	movq	%rdi,-16(%rbp)
	movsd	%xmm0,-8(%rbp)
# [181] Result := B * A;
	movsd	16(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,(%rax)
	movsd	24(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,8(%rax)
	movsd	32(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,16(%rax)
	movsd	40(%rbp),%xmm0
	mulsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,24(%rax)
	movq	-16(%rbp),%rax
# [182] end;
	leave
	ret
.Lc57:
.Le11:
	.size	GEOMETRY_star$DOUBLE$TVECTOR4$$TVECTOR4, .Le11 - GEOMETRY_star$DOUBLE$TVECTOR4$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_starstar$TVECTOR3$TVECTOR3$$TVECTOR3
	.type	GEOMETRY_starstar$TVECTOR3$TVECTOR3$$TVECTOR3,@function
GEOMETRY_starstar$TVECTOR3$TVECTOR3$$TVECTOR3:
.Lc61:
# Temps allocated between rbp-24 and rbp-8
# [185] begin
	pushq	%rbp
.Lc63:
.Lc64:
	movq	%rsp,%rbp
.Lc65:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp+40
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [186] Result.X := A.Y * B.Z - A.Z * B.Y;
	movsd	24(%rbp),%xmm1
	mulsd	56(%rbp),%xmm1
	movsd	32(%rbp),%xmm0
	mulsd	48(%rbp),%xmm0
	subsd	%xmm0,%xmm1
	movq	-8(%rbp),%rax
	movsd	%xmm1,(%rax)
# [187] Result.Y := A.Z * B.X - A.X * B.Z;
	movsd	32(%rbp),%xmm0
	mulsd	40(%rbp),%xmm0
	movsd	16(%rbp),%xmm1
	mulsd	56(%rbp),%xmm1
	subsd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [188] Result.Z := A.X * B.Y - A.Y * B.X;
	movsd	16(%rbp),%xmm0
	mulsd	48(%rbp),%xmm0
	movsd	24(%rbp),%xmm1
	mulsd	40(%rbp),%xmm1
	subsd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [189] end;
	leave
	ret
.Lc62:
.Le12:
	.size	GEOMETRY_starstar$TVECTOR3$TVECTOR3$$TVECTOR3, .Le12 - GEOMETRY_starstar$TVECTOR3$TVECTOR3$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_slash$TVECTOR3$DOUBLE$$TVECTOR3
	.type	GEOMETRY_slash$TVECTOR3$DOUBLE$$TVECTOR3,@function
GEOMETRY_slash$TVECTOR3$DOUBLE$$TVECTOR3:
.Lc66:
# Temps allocated between rbp-32 and rbp-16
# [197] begin
	pushq	%rbp
.Lc68:
.Lc69:
	movq	%rsp,%rbp
.Lc70:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp-8
# Var $result located at rbp-16
	movq	%rdi,-16(%rbp)
	movsd	%xmm0,-8(%rbp)
# [198] Result.X := A.X / B;
	movsd	16(%rbp),%xmm0
	divsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,(%rax)
# [199] Result.Y := A.Y / B;
	movsd	24(%rbp),%xmm0
	divsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [200] Result.Z := A.Z / B;
	movsd	32(%rbp),%xmm0
	divsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [201] end;
	leave
	ret
.Lc67:
.Le13:
	.size	GEOMETRY_slash$TVECTOR3$DOUBLE$$TVECTOR3, .Le13 - GEOMETRY_slash$TVECTOR3$DOUBLE$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_slash$TVECTOR4$DOUBLE$$TVECTOR4
	.type	GEOMETRY_slash$TVECTOR4$DOUBLE$$TVECTOR4,@function
GEOMETRY_slash$TVECTOR4$DOUBLE$$TVECTOR4:
.Lc71:
# Temps allocated between rbp-32 and rbp-16
# [204] begin
	pushq	%rbp
.Lc73:
.Lc74:
	movq	%rsp,%rbp
.Lc75:
	subq	$32,%rsp
# Var A located at rbp+16
# Var B located at rbp-8
# Var $result located at rbp-16
	movq	%rdi,-16(%rbp)
	movsd	%xmm0,-8(%rbp)
# [205] Result.X := A.X / B;
	movsd	16(%rbp),%xmm0
	divsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,(%rax)
# [206] Result.Y := A.Y / B;
	movsd	24(%rbp),%xmm0
	divsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [207] Result.Z := A.Z / B;
	movsd	32(%rbp),%xmm0
	divsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [208] Result.W := A.W / B;
	movsd	40(%rbp),%xmm0
	divsd	-8(%rbp),%xmm0
	movq	-16(%rbp),%rax
	movsd	%xmm0,24(%rax)
# [209] end;
	leave
	ret
.Lc72:
.Le14:
	.size	GEOMETRY_slash$TVECTOR4$DOUBLE$$TVECTOR4, .Le14 - GEOMETRY_slash$TVECTOR4$DOUBLE$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_assign$TVECTOR3$$TVECTOR3F
	.type	GEOMETRY_assign$TVECTOR3$$TVECTOR3F,@function
GEOMETRY_assign$TVECTOR3$$TVECTOR3F:
.Lc76:
# Temps allocated between rbp-8 and rbp-8
# [212] begin
	pushq	%rbp
.Lc78:
.Lc79:
	movq	%rsp,%rbp
.Lc80:
	subq	$16,%rsp
# Var A located at rbp+16
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [213] Result[0] := A.X;
	movq	-8(%rbp),%rax
	fldl	16(%rbp)
	fstps	(%rax)
# [214] Result[1] := A.Y;
	movq	-8(%rbp),%rax
	fldl	24(%rbp)
	fstps	4(%rax)
# [215] Result[2] := A.Z;
	movq	-8(%rbp),%rax
	fldl	32(%rbp)
	fstps	8(%rax)
# [216] end;
	leave
	ret
.Lc77:
.Le15:
	.size	GEOMETRY_assign$TVECTOR3$$TVECTOR3F, .Le15 - GEOMETRY_assign$TVECTOR3$$TVECTOR3F

.section .text
	.balign 8,0x90
.globl	GEOMETRY_assign$TVECTOR3F$$TVECTOR3
	.type	GEOMETRY_assign$TVECTOR3F$$TVECTOR3,@function
GEOMETRY_assign$TVECTOR3F$$TVECTOR3:
.Lc81:
# Temps allocated between rbp-28 and rbp-16
# [219] begin
	pushq	%rbp
.Lc83:
.Lc84:
	movq	%rsp,%rbp
.Lc85:
	subq	$32,%rsp
# Var A located at rbp-8
# Var $result located at rbp-16
	movq	%rdi,-16(%rbp)
	movq	%rsi,-8(%rbp)
	movq	-8(%rbp),%rax
	movq	(%rax),%rdx
	movq	%rdx,-28(%rbp)
	movl	8(%rax),%eax
	movl	%eax,-20(%rbp)
# [220] Result.X := A[0];
	movq	-16(%rbp),%rax
	flds	-28(%rbp)
	fstpl	(%rax)
# [221] Result.Y := A[1];
	movq	-16(%rbp),%rax
	flds	-24(%rbp)
	fstpl	8(%rax)
# [222] Result.Z := A[2];
	movq	-16(%rbp),%rax
	flds	-20(%rbp)
	fstpl	16(%rax)
# [223] end;
	leave
	ret
.Lc82:
.Le16:
	.size	GEOMETRY_assign$TVECTOR3F$$TVECTOR3, .Le16 - GEOMETRY_assign$TVECTOR3F$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_assign$TVECTOR4$$TVECTOR4F
	.type	GEOMETRY_assign$TVECTOR4$$TVECTOR4F,@function
GEOMETRY_assign$TVECTOR4$$TVECTOR4F:
.Lc86:
# Temps allocated between rbp-8 and rbp-8
# [226] begin
	pushq	%rbp
.Lc88:
.Lc89:
	movq	%rsp,%rbp
.Lc90:
	subq	$16,%rsp
# Var A located at rbp+16
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [227] Result[0] := A.X;
	movq	-8(%rbp),%rax
	fldl	16(%rbp)
	fstps	(%rax)
# [228] Result[1] := A.Y;
	movq	-8(%rbp),%rax
	fldl	24(%rbp)
	fstps	4(%rax)
# [229] Result[2] := A.Z;
	movq	-8(%rbp),%rax
	fldl	32(%rbp)
	fstps	8(%rax)
# [230] Result[3] := A.W;
	movq	-8(%rbp),%rax
	fldl	40(%rbp)
	fstps	12(%rax)
# [231] end;
	leave
	ret
.Lc87:
.Le17:
	.size	GEOMETRY_assign$TVECTOR4$$TVECTOR4F, .Le17 - GEOMETRY_assign$TVECTOR4$$TVECTOR4F

.section .text
	.balign 8,0x90
.globl	GEOMETRY_assign$TVECTOR4F$$TVECTOR4
	.type	GEOMETRY_assign$TVECTOR4F$$TVECTOR4,@function
GEOMETRY_assign$TVECTOR4F$$TVECTOR4:
.Lc91:
# Temps allocated between rbp-32 and rbp-16
# [234] begin
	pushq	%rbp
.Lc93:
.Lc94:
	movq	%rsp,%rbp
.Lc95:
	subq	$32,%rsp
# Var A located at rbp-8
# Var $result located at rbp-16
	movq	%rdi,-16(%rbp)
	movq	%rsi,-8(%rbp)
	movq	-8(%rbp),%rdx
	movq	(%rdx),%rax
	movq	%rax,-32(%rbp)
	movq	8(%rdx),%rax
	movq	%rax,-24(%rbp)
# [235] Result.X := A[0];
	movq	-16(%rbp),%rax
	flds	-32(%rbp)
	fstpl	(%rax)
# [236] Result.Y := A[1];
	movq	-16(%rbp),%rax
	flds	-28(%rbp)
	fstpl	8(%rax)
# [237] Result.Z := A[2];
	movq	-16(%rbp),%rax
	flds	-24(%rbp)
	fstpl	16(%rax)
# [238] Result.W := A[3];
	movq	-16(%rbp),%rax
	flds	-20(%rbp)
	fstpl	24(%rax)
# [239] end;
	leave
	ret
.Lc92:
.Le18:
	.size	GEOMETRY_assign$TVECTOR4F$$TVECTOR4, .Le18 - GEOMETRY_assign$TVECTOR4F$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_not$TVECTOR3$$TVECTOR3
	.type	GEOMETRY_not$TVECTOR3$$TVECTOR3,@function
GEOMETRY_not$TVECTOR3$$TVECTOR3:
.Lc96:
# Temps allocated between rbp-24 and rbp-8
# [242] begin
	pushq	%rbp
.Lc98:
.Lc99:
	movq	%rsp,%rbp
.Lc100:
	subq	$32,%rsp
# Var A located at rbp+16
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [243] Result.X := -A.X;
	movsd	16(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld8,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,(%rax)
# [244] Result.Y := -A.Y;
	movsd	24(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld9,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [245] Result.Z := -A.Z;
	movsd	32(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld10,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [246] end;
	leave
	ret
.Lc97:
.Le19:
	.size	GEOMETRY_not$TVECTOR3$$TVECTOR3, .Le19 - GEOMETRY_not$TVECTOR3$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_not$TVECTOR4$$TVECTOR4
	.type	GEOMETRY_not$TVECTOR4$$TVECTOR4,@function
GEOMETRY_not$TVECTOR4$$TVECTOR4:
.Lc101:
# Temps allocated between rbp-24 and rbp-8
# [249] begin
	pushq	%rbp
.Lc103:
.Lc104:
	movq	%rsp,%rbp
.Lc105:
	subq	$32,%rsp
# Var A located at rbp+16
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [250] Result.X := -A.X;
	movsd	16(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld11,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,(%rax)
# [251] Result.Y := -A.Y;
	movsd	24(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld12,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [252] Result.Z := -A.Z;
	movsd	32(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld13,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [253] Result.W := -A.W;
	movsd	40(%rbp),%xmm0
	movsd	_$GEOMETRY$_Ld14,%xmm1
	xorpd	%xmm1,%xmm0
	movq	-8(%rbp),%rax
	movsd	%xmm0,24(%rax)
# [254] end;
	leave
	ret
.Lc102:
.Le20:
	.size	GEOMETRY_not$TVECTOR4$$TVECTOR4, .Le20 - GEOMETRY_not$TVECTOR4$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_NORMALIZE$TVECTOR3$$TVECTOR3
	.type	GEOMETRY_NORMALIZE$TVECTOR3$$TVECTOR3,@function
GEOMETRY_NORMALIZE$TVECTOR3$$TVECTOR3:
.Lc106:
# Temps allocated between rbp-8 and rbp-8
# [257] begin
	pushq	%rbp
.Lc108:
.Lc109:
	movq	%rsp,%rbp
.Lc110:
	subq	$16,%rsp
# Var Vec3 located at rbp+16
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [258] Result := Vec3;
	movq	-8(%rbp),%rdx
	movq	16(%rbp),%rax
	movq	%rax,(%rdx)
	movq	24(%rbp),%rax
	movq	%rax,8(%rdx)
	movq	32(%rbp),%rax
	movq	%rax,16(%rdx)
# [259] NormalizeInPlace(Result);
	movq	-8(%rbp),%rdi
	call	GEOMETRY_NORMALIZEINPLACE$TVECTOR3
# [260] end;
	leave
	ret
.Lc107:
.Le21:
	.size	GEOMETRY_NORMALIZE$TVECTOR3$$TVECTOR3, .Le21 - GEOMETRY_NORMALIZE$TVECTOR3$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_NORMALIZE$TVECTOR4$$TVECTOR4
	.type	GEOMETRY_NORMALIZE$TVECTOR4$$TVECTOR4,@function
GEOMETRY_NORMALIZE$TVECTOR4$$TVECTOR4:
.Lc111:
# Temps allocated between rbp-8 and rbp-8
# [263] begin
	pushq	%rbp
.Lc113:
.Lc114:
	movq	%rsp,%rbp
.Lc115:
	subq	$16,%rsp
# Var Vec4 located at rbp+16
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [264] Result := Vec4;
	movq	-8(%rbp),%rdi
	leaq	16(%rbp),%rsi
	movq	$4,%rcx
	rep
	movsq
# [265] NormalizeInPlace(Result);
	movq	-8(%rbp),%rdi
	call	GEOMETRY_NORMALIZEINPLACE$TVECTOR4
# [266] end;
	leave
	ret
.Lc112:
.Le22:
	.size	GEOMETRY_NORMALIZE$TVECTOR4$$TVECTOR4, .Le22 - GEOMETRY_NORMALIZE$TVECTOR4$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_NORMALIZEINPLACE$TVECTOR3
	.type	GEOMETRY_NORMALIZEINPLACE$TVECTOR3,@function
GEOMETRY_NORMALIZEINPLACE$TVECTOR3:
.Lc116:
# Temps allocated between rbp-80 and rbp-40
# [271] begin
	pushq	%rbp
.Lc118:
.Lc119:
	movq	%rsp,%rbp
.Lc120:
	subq	$112,%rsp
# Var Vec3 located at rbp-32
# Var Len located at rbp-40
	movq	%rdi,-32(%rbp)
# [272] Len := VLength(Vec3);
	movq	-32(%rbp),%rdx
	movq	(%rdx),%rax
	movq	%rax,-64(%rbp)
	movq	8(%rdx),%rax
	movq	%rax,-56(%rbp)
	movq	16(%rdx),%rax
	movq	%rax,-48(%rbp)
	movq	-64(%rbp),%rax
	movq	%rax,(%rsp)
	movq	-56(%rbp),%rax
	movq	%rax,8(%rsp)
	movq	-48(%rbp),%rax
	movq	%rax,16(%rsp)
	call	GEOMETRY_VLENGTH$TVECTOR3$$DOUBLE
	movsd	%xmm0,%xmm0
	movsd	%xmm0,-40(%rbp)
# [273] Vec3.X := Vec3.X / Len;
	movq	-32(%rbp),%rax
	movsd	(%rax),%xmm0
	divsd	-40(%rbp),%xmm0
	movq	-32(%rbp),%rax
	movsd	%xmm0,(%rax)
# [274] Vec3.Y := Vec3.Y / Len;
	movq	-32(%rbp),%rax
	movsd	8(%rax),%xmm0
	divsd	-40(%rbp),%xmm0
	movq	-32(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [275] Vec3.Z := Vec3.Z / Len;
	movq	-32(%rbp),%rax
	movsd	16(%rax),%xmm0
	divsd	-40(%rbp),%xmm0
	movq	-32(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [276] end;
	leave
	ret
.Lc117:
.Le23:
	.size	GEOMETRY_NORMALIZEINPLACE$TVECTOR3, .Le23 - GEOMETRY_NORMALIZEINPLACE$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_NORMALIZEINPLACE$TVECTOR4
	.type	GEOMETRY_NORMALIZEINPLACE$TVECTOR4,@function
GEOMETRY_NORMALIZEINPLACE$TVECTOR4:
.Lc121:
# Temps allocated between rbp-96 and rbp-48
# [281] begin
	pushq	%rbp
.Lc123:
.Lc124:
	movq	%rsp,%rbp
.Lc125:
	subq	$128,%rsp
# Var Vec4 located at rbp-40
# Var Len located at rbp-48
	movq	%rdi,-40(%rbp)
# [282] Len := VLength(Vec4);
	movq	-40(%rbp),%rsi
	leaq	-80(%rbp),%rdi
	movq	$4,%rcx
	rep
	movsq
	movq	%rsp,%rdi
	leaq	-80(%rbp),%rsi
	movq	$4,%rcx
	rep
	movsq
	call	GEOMETRY_VLENGTH$TVECTOR4$$DOUBLE
	movsd	%xmm0,%xmm0
	movsd	%xmm0,-48(%rbp)
# [283] Vec4.X := Vec4.X / Len;
	movq	-40(%rbp),%rax
	movsd	(%rax),%xmm0
	divsd	-48(%rbp),%xmm0
	movq	-40(%rbp),%rax
	movsd	%xmm0,(%rax)
# [284] Vec4.Y := Vec4.Y / Len;
	movq	-40(%rbp),%rax
	movsd	8(%rax),%xmm0
	divsd	-48(%rbp),%xmm0
	movq	-40(%rbp),%rax
	movsd	%xmm0,8(%rax)
# [285] Vec4.Z := Vec4.Z / Len;
	movq	-40(%rbp),%rax
	movsd	16(%rax),%xmm0
	divsd	-48(%rbp),%xmm0
	movq	-40(%rbp),%rax
	movsd	%xmm0,16(%rax)
# [286] end;
	leave
	ret
.Lc122:
.Le24:
	.size	GEOMETRY_NORMALIZEINPLACE$TVECTOR4, .Le24 - GEOMETRY_NORMALIZEINPLACE$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_VLENGTH$TVECTOR3$$DOUBLE
	.type	GEOMETRY_VLENGTH$TVECTOR3$$DOUBLE,@function
GEOMETRY_VLENGTH$TVECTOR3$$DOUBLE:
.Lc126:
# Temps allocated between rbp-24 and rbp-8
# [289] begin
	pushq	%rbp
.Lc128:
.Lc129:
	movq	%rsp,%rbp
.Lc130:
	subq	$32,%rsp
# Var Vec3 located at rbp+16
# Var $result located at rbp-8
# [290] Result := Sqrt(Sqr(Vec3.X) + Sqr(Vec3.Y) + Sqr(Vec3.Z));
	movsd	16(%rbp),%xmm0
	mulsd	%xmm0,%xmm0
	movsd	24(%rbp),%xmm1
	mulsd	%xmm1,%xmm1
	addsd	%xmm0,%xmm1
	movsd	32(%rbp),%xmm0
	mulsd	%xmm0,%xmm0
	addsd	%xmm1,%xmm0
	sqrtsd	%xmm0,%xmm0
	movsd	%xmm0,-8(%rbp)
# [291] end;
	movsd	-8(%rbp),%xmm0
	leave
	ret
.Lc127:
.Le25:
	.size	GEOMETRY_VLENGTH$TVECTOR3$$DOUBLE, .Le25 - GEOMETRY_VLENGTH$TVECTOR3$$DOUBLE

.section .text
	.balign 8,0x90
.globl	GEOMETRY_VLENGTH$TVECTOR4$$DOUBLE
	.type	GEOMETRY_VLENGTH$TVECTOR4$$DOUBLE,@function
GEOMETRY_VLENGTH$TVECTOR4$$DOUBLE:
.Lc131:
# Temps allocated between rbp-24 and rbp-8
# [294] begin
	pushq	%rbp
.Lc133:
.Lc134:
	movq	%rsp,%rbp
.Lc135:
	subq	$32,%rsp
# Var Vec4 located at rbp+16
# Var $result located at rbp-8
# [295] Result := Sqrt(Sqr(Vec4.X) + Sqr(Vec4.Y) + Sqr(Vec4.Z) + Sqr(Vec4.W));
	movsd	16(%rbp),%xmm1
	mulsd	%xmm1,%xmm1
	movsd	24(%rbp),%xmm0
	mulsd	%xmm0,%xmm0
	addsd	%xmm1,%xmm0
	movsd	32(%rbp),%xmm1
	mulsd	%xmm1,%xmm1
	addsd	%xmm0,%xmm1
	movsd	40(%rbp),%xmm0
	mulsd	%xmm0,%xmm0
	addsd	%xmm1,%xmm0
	sqrtsd	%xmm0,%xmm0
	movsd	%xmm0,-8(%rbp)
# [296] end;
	movsd	-8(%rbp),%xmm0
	leave
	ret
.Lc132:
.Le26:
	.size	GEOMETRY_VLENGTH$TVECTOR4$$DOUBLE, .Le26 - GEOMETRY_VLENGTH$TVECTOR4$$DOUBLE

.section .text
	.balign 8,0x90
.globl	GEOMETRY_VECTOR3F$DOUBLE$DOUBLE$DOUBLE$$TVECTOR3
	.type	GEOMETRY_VECTOR3F$DOUBLE$DOUBLE$DOUBLE$$TVECTOR3,@function
GEOMETRY_VECTOR3F$DOUBLE$DOUBLE$DOUBLE$$TVECTOR3:
.Lc136:
# Temps allocated between rbp-32 and rbp-32
# [299] begin
	pushq	%rbp
.Lc138:
.Lc139:
	movq	%rsp,%rbp
.Lc140:
	subq	$32,%rsp
# Var X located at rbp-8
# Var Y located at rbp-16
# Var Z located at rbp-24
# Var $result located at rbp-32
	movq	%rdi,-32(%rbp)
	movsd	%xmm0,-8(%rbp)
	movsd	%xmm1,-16(%rbp)
	movsd	%xmm2,-24(%rbp)
# [300] Result.X := X;
	movq	-32(%rbp),%rdx
	movq	-8(%rbp),%rax
	movq	%rax,(%rdx)
# [301] Result.Y := Y;
	movq	-32(%rbp),%rax
	movq	-16(%rbp),%rdx
	movq	%rdx,8(%rax)
# [302] Result.Z := Z;
	movq	-32(%rbp),%rax
	movq	-24(%rbp),%rdx
	movq	%rdx,16(%rax)
# [303] end;
	leave
	ret
.Lc137:
.Le27:
	.size	GEOMETRY_VECTOR3F$DOUBLE$DOUBLE$DOUBLE$$TVECTOR3, .Le27 - GEOMETRY_VECTOR3F$DOUBLE$DOUBLE$DOUBLE$$TVECTOR3

.section .text
	.balign 8,0x90
.globl	GEOMETRY_VECTOR4F$TVECTOR3$DOUBLE$$TVECTOR4
	.type	GEOMETRY_VECTOR4F$TVECTOR3$DOUBLE$$TVECTOR4,@function
GEOMETRY_VECTOR4F$TVECTOR3$DOUBLE$$TVECTOR4:
.Lc141:
# Temps allocated between rbp-16 and rbp-16
# [306] begin
	pushq	%rbp
.Lc143:
.Lc144:
	movq	%rsp,%rbp
.Lc145:
	subq	$16,%rsp
# Var Vec3 located at rbp+16
# Var W located at rbp-8
# Var $result located at rbp-16
	movq	%rdi,-16(%rbp)
	movsd	%xmm0,-8(%rbp)
# [307] Result.Vec3 := Vec3;
	movq	-16(%rbp),%rdx
	movq	16(%rbp),%rax
	movq	%rax,(%rdx)
	movq	24(%rbp),%rax
	movq	%rax,8(%rdx)
	movq	32(%rbp),%rax
	movq	%rax,16(%rdx)
# [308] Result.W := W;
	movq	-16(%rbp),%rdx
	movq	-8(%rbp),%rax
	movq	%rax,24(%rdx)
# [309] end;
	leave
	ret
.Lc142:
.Le28:
	.size	GEOMETRY_VECTOR4F$TVECTOR3$DOUBLE$$TVECTOR4, .Le28 - GEOMETRY_VECTOR4F$TVECTOR3$DOUBLE$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_VECTOR4F$DOUBLE$DOUBLE$DOUBLE$DOUBLE$$TVECTOR4
	.type	GEOMETRY_VECTOR4F$DOUBLE$DOUBLE$DOUBLE$DOUBLE$$TVECTOR4,@function
GEOMETRY_VECTOR4F$DOUBLE$DOUBLE$DOUBLE$DOUBLE$$TVECTOR4:
.Lc146:
# Temps allocated between rbp-40 and rbp-40
# [312] begin
	pushq	%rbp
.Lc148:
.Lc149:
	movq	%rsp,%rbp
.Lc150:
	subq	$48,%rsp
# Var X located at rbp-8
# Var Y located at rbp-16
# Var Z located at rbp-24
# Var W located at rbp-32
# Var $result located at rbp-40
	movq	%rdi,-40(%rbp)
	movsd	%xmm0,-8(%rbp)
	movsd	%xmm1,-16(%rbp)
	movsd	%xmm2,-24(%rbp)
	movsd	%xmm3,-32(%rbp)
# [313] Result.X := X;
	movq	-40(%rbp),%rdx
	movq	-8(%rbp),%rax
	movq	%rax,(%rdx)
# [314] Result.Y := Y;
	movq	-40(%rbp),%rax
	movq	-16(%rbp),%rdx
	movq	%rdx,8(%rax)
# [315] Result.Z := Z;
	movq	-40(%rbp),%rax
	movq	-24(%rbp),%rdx
	movq	%rdx,16(%rax)
# [316] Result.W := W;
	movq	-40(%rbp),%rax
	movq	-32(%rbp),%rdx
	movq	%rdx,24(%rax)
# [317] end;
	leave
	ret
.Lc147:
.Le29:
	.size	GEOMETRY_VECTOR4F$DOUBLE$DOUBLE$DOUBLE$DOUBLE$$TVECTOR4, .Le29 - GEOMETRY_VECTOR4F$DOUBLE$DOUBLE$DOUBLE$DOUBLE$$TVECTOR4

.section .text
	.balign 8,0x90
.globl	GEOMETRY_FORMATVECTOR$TVECTOR3$$ANSISTRING
	.type	GEOMETRY_FORMATVECTOR$TVECTOR3$$ANSISTRING,@function
GEOMETRY_FORMATVECTOR$TVECTOR3$$ANSISTRING:
.Lc151:
# Temps allocated between rbp-90 and rbp-8
# [320] begin
	pushq	%rbp
.Lc153:
.Lc154:
	movq	%rsp,%rbp
.Lc155:
	subq	$96,%rsp
# Var Vec3 located at rbp+16
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [321] Result := Format('vec3(%.3f, %.3f, %.3f)', [Vec3.X, Vec3.Y, Vec3.Z]);
	fldl	16(%rbp)
	fstpt	-70(%rbp)
	leaq	-70(%rbp),%rax
	movq	%rax,-48(%rbp)
	movq	$3,-56(%rbp)
	fldl	24(%rbp)
	fstpt	-80(%rbp)
	leaq	-80(%rbp),%rax
	movq	%rax,-32(%rbp)
	movq	$3,-40(%rbp)
	fldl	32(%rbp)
	fstpt	-90(%rbp)
	leaq	-90(%rbp),%rax
	movq	%rax,-16(%rbp)
	movq	$3,-24(%rbp)
	leaq	-56(%rbp),%rdx
	movq	-8(%rbp),%rdi
	movq	$2,%rcx
	movq	$_$GEOMETRY$_Ld15,%rsi
	call	SYSUTILS_FORMAT$ANSISTRING$array_of_const$$ANSISTRING
# [322] end;
	leave
	ret
.Lc152:
.Le30:
	.size	GEOMETRY_FORMATVECTOR$TVECTOR3$$ANSISTRING, .Le30 - GEOMETRY_FORMATVECTOR$TVECTOR3$$ANSISTRING

.section .text
	.balign 8,0x90
.globl	GEOMETRY_FORMATVECTOR$TVECTOR4$$ANSISTRING
	.type	GEOMETRY_FORMATVECTOR$TVECTOR4$$ANSISTRING,@function
GEOMETRY_FORMATVECTOR$TVECTOR4$$ANSISTRING:
.Lc156:
# Temps allocated between rbp-120 and rbp-8
# [325] begin
	pushq	%rbp
.Lc158:
.Lc159:
	movq	%rsp,%rbp
.Lc160:
	subq	$128,%rsp
# Var Vec4 located at rbp+16
# Var $result located at rbp-8
	movq	%rdi,-8(%rbp)
# [326] Result := Format('vec4(%.3f, %.3f, %.3f, %.3f)', [Vec4.X, Vec4.Y, Vec4.Z, Vec4.W]);
	fldl	16(%rbp)
	fstpt	-90(%rbp)
	leaq	-90(%rbp),%rax
	movq	%rax,-64(%rbp)
	movq	$3,-72(%rbp)
	fldl	24(%rbp)
	fstpt	-100(%rbp)
	leaq	-100(%rbp),%rax
	movq	%rax,-48(%rbp)
	movq	$3,-56(%rbp)
	fldl	32(%rbp)
	fstpt	-110(%rbp)
	leaq	-110(%rbp),%rax
	movq	%rax,-32(%rbp)
	movq	$3,-40(%rbp)
	fldl	40(%rbp)
	fstpt	-120(%rbp)
	leaq	-120(%rbp),%rax
	movq	%rax,-16(%rbp)
	movq	$3,-24(%rbp)
	leaq	-72(%rbp),%rdx
	movq	-8(%rbp),%rdi
	movq	$3,%rcx
	movq	$_$GEOMETRY$_Ld17,%rsi
	call	SYSUTILS_FORMAT$ANSISTRING$array_of_const$$ANSISTRING
# [327] end;
	leave
	ret
.Lc157:
.Le31:
	.size	GEOMETRY_FORMATVECTOR$TVECTOR4$$ANSISTRING, .Le31 - GEOMETRY_FORMATVECTOR$TVECTOR4$$ANSISTRING
# End asmlist al_procedures
# Begin asmlist al_globals

.section .data
	.balign 8
.globl	THREADVARLIST_GEOMETRY
	.type	THREADVARLIST_GEOMETRY,@object
THREADVARLIST_GEOMETRY:
	.quad	0
# [331] 
.Le32:
	.size	THREADVARLIST_GEOMETRY, .Le32 - THREADVARLIST_GEOMETRY
# End asmlist al_globals
# Begin asmlist al_const
# End asmlist al_const
# Begin asmlist al_typedconsts
.globl	_$GEOMETRY$_Ld1
_$GEOMETRY$_Ld1:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld2
_$GEOMETRY$_Ld2:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld3
_$GEOMETRY$_Ld3:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld4
_$GEOMETRY$_Ld4:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld5
_$GEOMETRY$_Ld5:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld6
_$GEOMETRY$_Ld6:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld7
_$GEOMETRY$_Ld7:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld8
_$GEOMETRY$_Ld8:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld9
_$GEOMETRY$_Ld9:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld10
_$GEOMETRY$_Ld10:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld11
_$GEOMETRY$_Ld11:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld12
_$GEOMETRY$_Ld12:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld13
_$GEOMETRY$_Ld13:
	.long	0,-2147483648
.globl	_$GEOMETRY$_Ld14
_$GEOMETRY$_Ld14:
	.long	0,-2147483648

.section .data
	.balign 8
.globl	_$GEOMETRY$_Ld16
_$GEOMETRY$_Ld16:
	.quad	-1,22
.globl	_$GEOMETRY$_Ld15
_$GEOMETRY$_Ld15:
	.ascii	"vec3(%.3f, %.3f, %.3f)\000"

.section .data
	.balign 8
.globl	_$GEOMETRY$_Ld18
_$GEOMETRY$_Ld18:
	.quad	-1,28
.globl	_$GEOMETRY$_Ld17
_$GEOMETRY$_Ld17:
	.ascii	"vec4(%.3f, %.3f, %.3f, %.3f)\000"
# End asmlist al_typedconsts
# Begin asmlist al_rotypedconsts
# End asmlist al_rotypedconsts
# Begin asmlist al_threadvars
# End asmlist al_threadvars
# Begin asmlist al_imports
# End asmlist al_imports
# Begin asmlist al_exports
# End asmlist al_exports
# Begin asmlist al_resources
# End asmlist al_resources
# Begin asmlist al_rtti

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TVECTOR2_ARRAY
	.type	INIT_GEOMETRY_TVECTOR2_ARRAY,@object
INIT_GEOMETRY_TVECTOR2_ARRAY:
	.byte	12
	.ascii	"\016TVector2_Array"
	.quad	8,2
	.quad	INIT_SYSTEM_DOUBLE
	.long	5
.Le33:
	.size	INIT_GEOMETRY_TVECTOR2_ARRAY, .Le33 - INIT_GEOMETRY_TVECTOR2_ARRAY

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TVECTOR2_ARRAY
	.type	RTTI_GEOMETRY_TVECTOR2_ARRAY,@object
RTTI_GEOMETRY_TVECTOR2_ARRAY:
	.byte	12
	.ascii	"\016TVector2_Array"
	.quad	8,2
	.quad	RTTI_SYSTEM_DOUBLE
	.long	5
.Le34:
	.size	RTTI_GEOMETRY_TVECTOR2_ARRAY, .Le34 - RTTI_GEOMETRY_TVECTOR2_ARRAY

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TVECTOR2
	.type	INIT_GEOMETRY_TVECTOR2,@object
INIT_GEOMETRY_TVECTOR2:
	.byte	13
	.ascii	"\010TVector2"
	.long	16,0
.Le35:
	.size	INIT_GEOMETRY_TVECTOR2, .Le35 - INIT_GEOMETRY_TVECTOR2

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TVECTOR2
	.type	RTTI_GEOMETRY_TVECTOR2,@object
RTTI_GEOMETRY_TVECTOR2:
	.byte	13
	.ascii	"\010TVector2"
	.long	16,7
	.quad	RTTI_SYSTEM_DOUBLE
	.long	0
	.quad	RTTI_SYSTEM_DOUBLE
	.long	8
	.quad	RTTI_GEOMETRY_TVECTOR2_ARRAY
	.long	0
	.quad	RTTI_SYSTEM_DOUBLE
	.long	0
	.quad	RTTI_SYSTEM_DOUBLE
	.long	8
	.quad	RTTI_SYSTEM_DOUBLE
	.long	0
	.quad	RTTI_SYSTEM_DOUBLE
	.long	8
.Le36:
	.size	RTTI_GEOMETRY_TVECTOR2, .Le36 - RTTI_GEOMETRY_TVECTOR2

.section .data
	.balign 8
.globl	INIT_GEOMETRY_PVECTOR2
	.type	INIT_GEOMETRY_PVECTOR2,@object
INIT_GEOMETRY_PVECTOR2:
	.byte	0
	.ascii	"\010PVector2"
.Le37:
	.size	INIT_GEOMETRY_PVECTOR2, .Le37 - INIT_GEOMETRY_PVECTOR2

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_PVECTOR2
	.type	RTTI_GEOMETRY_PVECTOR2,@object
RTTI_GEOMETRY_PVECTOR2:
	.byte	0
	.ascii	"\010PVector2"
.Le38:
	.size	RTTI_GEOMETRY_PVECTOR2, .Le38 - RTTI_GEOMETRY_PVECTOR2

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TVECTOR3_ARRAY
	.type	INIT_GEOMETRY_TVECTOR3_ARRAY,@object
INIT_GEOMETRY_TVECTOR3_ARRAY:
	.byte	12
	.ascii	"\016TVector3_Array"
	.quad	8,3
	.quad	INIT_SYSTEM_DOUBLE
	.long	5
.Le39:
	.size	INIT_GEOMETRY_TVECTOR3_ARRAY, .Le39 - INIT_GEOMETRY_TVECTOR3_ARRAY

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TVECTOR3_ARRAY
	.type	RTTI_GEOMETRY_TVECTOR3_ARRAY,@object
RTTI_GEOMETRY_TVECTOR3_ARRAY:
	.byte	12
	.ascii	"\016TVector3_Array"
	.quad	8,3
	.quad	RTTI_SYSTEM_DOUBLE
	.long	5
.Le40:
	.size	RTTI_GEOMETRY_TVECTOR3_ARRAY, .Le40 - RTTI_GEOMETRY_TVECTOR3_ARRAY

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TVECTOR3
	.type	INIT_GEOMETRY_TVECTOR3,@object
INIT_GEOMETRY_TVECTOR3:
	.byte	13
	.ascii	"\010TVector3"
	.long	24,0
.Le41:
	.size	INIT_GEOMETRY_TVECTOR3, .Le41 - INIT_GEOMETRY_TVECTOR3

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TVECTOR3
	.type	RTTI_GEOMETRY_TVECTOR3,@object
RTTI_GEOMETRY_TVECTOR3:
	.byte	13
	.ascii	"\010TVector3"
	.long	24,7
	.quad	RTTI_SYSTEM_DOUBLE
	.long	0
	.quad	RTTI_SYSTEM_DOUBLE
	.long	8
	.quad	RTTI_SYSTEM_DOUBLE
	.long	16
	.quad	RTTI_GEOMETRY_TVECTOR3_ARRAY
	.long	0
	.quad	RTTI_SYSTEM_DOUBLE
	.long	0
	.quad	RTTI_SYSTEM_DOUBLE
	.long	8
	.quad	RTTI_SYSTEM_DOUBLE
	.long	16
.Le42:
	.size	RTTI_GEOMETRY_TVECTOR3, .Le42 - RTTI_GEOMETRY_TVECTOR3

.section .data
	.balign 8
.globl	INIT_GEOMETRY_PVECTOR3
	.type	INIT_GEOMETRY_PVECTOR3,@object
INIT_GEOMETRY_PVECTOR3:
	.byte	0
	.ascii	"\010PVector3"
.Le43:
	.size	INIT_GEOMETRY_PVECTOR3, .Le43 - INIT_GEOMETRY_PVECTOR3

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_PVECTOR3
	.type	RTTI_GEOMETRY_PVECTOR3,@object
RTTI_GEOMETRY_PVECTOR3:
	.byte	0
	.ascii	"\010PVector3"
.Le44:
	.size	RTTI_GEOMETRY_PVECTOR3, .Le44 - RTTI_GEOMETRY_PVECTOR3

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TTRIANGLE3
	.type	INIT_GEOMETRY_TTRIANGLE3,@object
INIT_GEOMETRY_TTRIANGLE3:
	.byte	12
	.ascii	"\012TTriangle3"
	.quad	24,3
	.quad	INIT_GEOMETRY_TVECTOR3
	.long	-1
.Le45:
	.size	INIT_GEOMETRY_TTRIANGLE3, .Le45 - INIT_GEOMETRY_TTRIANGLE3

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TTRIANGLE3
	.type	RTTI_GEOMETRY_TTRIANGLE3,@object
RTTI_GEOMETRY_TTRIANGLE3:
	.byte	12
	.ascii	"\012TTriangle3"
	.quad	24,3
	.quad	RTTI_GEOMETRY_TVECTOR3
	.long	-1
.Le46:
	.size	RTTI_GEOMETRY_TTRIANGLE3, .Le46 - RTTI_GEOMETRY_TTRIANGLE3

.section .data
	.balign 8
.globl	INIT_GEOMETRY_PTRIANGLE3
	.type	INIT_GEOMETRY_PTRIANGLE3,@object
INIT_GEOMETRY_PTRIANGLE3:
	.byte	0
	.ascii	"\012PTriangle3"
.Le47:
	.size	INIT_GEOMETRY_PTRIANGLE3, .Le47 - INIT_GEOMETRY_PTRIANGLE3

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_PTRIANGLE3
	.type	RTTI_GEOMETRY_PTRIANGLE3,@object
RTTI_GEOMETRY_PTRIANGLE3:
	.byte	0
	.ascii	"\012PTriangle3"
.Le48:
	.size	RTTI_GEOMETRY_PTRIANGLE3, .Le48 - RTTI_GEOMETRY_PTRIANGLE3

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TVECTOR4_ARRAY
	.type	INIT_GEOMETRY_TVECTOR4_ARRAY,@object
INIT_GEOMETRY_TVECTOR4_ARRAY:
	.byte	12
	.ascii	"\016TVector4_Array"
	.quad	8,4
	.quad	INIT_SYSTEM_DOUBLE
	.long	5
.Le49:
	.size	INIT_GEOMETRY_TVECTOR4_ARRAY, .Le49 - INIT_GEOMETRY_TVECTOR4_ARRAY

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TVECTOR4_ARRAY
	.type	RTTI_GEOMETRY_TVECTOR4_ARRAY,@object
RTTI_GEOMETRY_TVECTOR4_ARRAY:
	.byte	12
	.ascii	"\016TVector4_Array"
	.quad	8,4
	.quad	RTTI_SYSTEM_DOUBLE
	.long	5
.Le50:
	.size	RTTI_GEOMETRY_TVECTOR4_ARRAY, .Le50 - RTTI_GEOMETRY_TVECTOR4_ARRAY

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TVECTOR4_LENGTH
	.type	INIT_GEOMETRY_TVECTOR4_LENGTH,@object
INIT_GEOMETRY_TVECTOR4_LENGTH:
	.byte	12
	.ascii	"\017TVector4_Length"
	.quad	8,4
	.quad	INIT_SYSTEM_DOUBLE
	.long	5
.Le51:
	.size	INIT_GEOMETRY_TVECTOR4_LENGTH, .Le51 - INIT_GEOMETRY_TVECTOR4_LENGTH

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TVECTOR4_LENGTH
	.type	RTTI_GEOMETRY_TVECTOR4_LENGTH,@object
RTTI_GEOMETRY_TVECTOR4_LENGTH:
	.byte	12
	.ascii	"\017TVector4_Length"
	.quad	8,4
	.quad	RTTI_SYSTEM_DOUBLE
	.long	5
.Le52:
	.size	RTTI_GEOMETRY_TVECTOR4_LENGTH, .Le52 - RTTI_GEOMETRY_TVECTOR4_LENGTH

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TVECTOR4
	.type	INIT_GEOMETRY_TVECTOR4,@object
INIT_GEOMETRY_TVECTOR4:
	.byte	13
	.ascii	"\010TVector4"
	.long	32,0
.Le53:
	.size	INIT_GEOMETRY_TVECTOR4, .Le53 - INIT_GEOMETRY_TVECTOR4

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TVECTOR4
	.type	RTTI_GEOMETRY_TVECTOR4,@object
RTTI_GEOMETRY_TVECTOR4:
	.byte	13
	.ascii	"\010TVector4"
	.long	32,10
	.quad	RTTI_SYSTEM_DOUBLE
	.long	0
	.quad	RTTI_SYSTEM_DOUBLE
	.long	8
	.quad	RTTI_SYSTEM_DOUBLE
	.long	16
	.quad	RTTI_SYSTEM_DOUBLE
	.long	24
	.quad	RTTI_GEOMETRY_TVECTOR4_ARRAY
	.long	0
	.quad	RTTI_SYSTEM_DOUBLE
	.long	0
	.quad	RTTI_SYSTEM_DOUBLE
	.long	8
	.quad	RTTI_SYSTEM_DOUBLE
	.long	16
	.quad	RTTI_SYSTEM_DOUBLE
	.long	24
	.quad	RTTI_GEOMETRY_TVECTOR3
	.long	0
.Le54:
	.size	RTTI_GEOMETRY_TVECTOR4, .Le54 - RTTI_GEOMETRY_TVECTOR4

.section .data
	.balign 8
.globl	INIT_GEOMETRY_PVECTOR4
	.type	INIT_GEOMETRY_PVECTOR4,@object
INIT_GEOMETRY_PVECTOR4:
	.byte	0
	.ascii	"\010PVector4"
.Le55:
	.size	INIT_GEOMETRY_PVECTOR4, .Le55 - INIT_GEOMETRY_PVECTOR4

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_PVECTOR4
	.type	RTTI_GEOMETRY_PVECTOR4,@object
RTTI_GEOMETRY_PVECTOR4:
	.byte	0
	.ascii	"\010PVector4"
.Le56:
	.size	RTTI_GEOMETRY_PVECTOR4, .Le56 - RTTI_GEOMETRY_PVECTOR4

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TTRIANGLE4
	.type	INIT_GEOMETRY_TTRIANGLE4,@object
INIT_GEOMETRY_TTRIANGLE4:
	.byte	12
	.ascii	"\012TTriangle4"
	.quad	32,3
	.quad	INIT_GEOMETRY_TVECTOR4
	.long	-1
.Le57:
	.size	INIT_GEOMETRY_TTRIANGLE4, .Le57 - INIT_GEOMETRY_TTRIANGLE4

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TTRIANGLE4
	.type	RTTI_GEOMETRY_TTRIANGLE4,@object
RTTI_GEOMETRY_TTRIANGLE4:
	.byte	12
	.ascii	"\012TTriangle4"
	.quad	32,3
	.quad	RTTI_GEOMETRY_TVECTOR4
	.long	-1
.Le58:
	.size	RTTI_GEOMETRY_TTRIANGLE4, .Le58 - RTTI_GEOMETRY_TTRIANGLE4

.section .data
	.balign 8
.globl	INIT_GEOMETRY_PTRIANGLE4
	.type	INIT_GEOMETRY_PTRIANGLE4,@object
INIT_GEOMETRY_PTRIANGLE4:
	.byte	0
	.ascii	"\012PTriangle4"
.Le59:
	.size	INIT_GEOMETRY_PTRIANGLE4, .Le59 - INIT_GEOMETRY_PTRIANGLE4

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_PTRIANGLE4
	.type	RTTI_GEOMETRY_PTRIANGLE4,@object
RTTI_GEOMETRY_PTRIANGLE4:
	.byte	0
	.ascii	"\012PTriangle4"
.Le60:
	.size	RTTI_GEOMETRY_PTRIANGLE4, .Le60 - RTTI_GEOMETRY_PTRIANGLE4

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TVECTOR3F
	.type	INIT_GEOMETRY_TVECTOR3F,@object
INIT_GEOMETRY_TVECTOR3F:
	.byte	12
	.ascii	"\011TVector3f"
	.quad	4,3
	.quad	INIT_SYSTEM_SINGLE
	.long	4
.Le61:
	.size	INIT_GEOMETRY_TVECTOR3F, .Le61 - INIT_GEOMETRY_TVECTOR3F

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TVECTOR3F
	.type	RTTI_GEOMETRY_TVECTOR3F,@object
RTTI_GEOMETRY_TVECTOR3F:
	.byte	12
	.ascii	"\011TVector3f"
	.quad	4,3
	.quad	RTTI_SYSTEM_SINGLE
	.long	4
.Le62:
	.size	RTTI_GEOMETRY_TVECTOR3F, .Le62 - RTTI_GEOMETRY_TVECTOR3F

.section .data
	.balign 8
.globl	INIT_GEOMETRY_PVECTOR3F
	.type	INIT_GEOMETRY_PVECTOR3F,@object
INIT_GEOMETRY_PVECTOR3F:
	.byte	0
	.ascii	"\011PVector3f"
.Le63:
	.size	INIT_GEOMETRY_PVECTOR3F, .Le63 - INIT_GEOMETRY_PVECTOR3F

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_PVECTOR3F
	.type	RTTI_GEOMETRY_PVECTOR3F,@object
RTTI_GEOMETRY_PVECTOR3F:
	.byte	0
	.ascii	"\011PVector3f"
.Le64:
	.size	RTTI_GEOMETRY_PVECTOR3F, .Le64 - RTTI_GEOMETRY_PVECTOR3F

.section .data
	.balign 8
.globl	INIT_GEOMETRY_TVECTOR4F
	.type	INIT_GEOMETRY_TVECTOR4F,@object
INIT_GEOMETRY_TVECTOR4F:
	.byte	12
	.ascii	"\011TVector4f"
	.quad	4,4
	.quad	INIT_SYSTEM_SINGLE
	.long	4
.Le65:
	.size	INIT_GEOMETRY_TVECTOR4F, .Le65 - INIT_GEOMETRY_TVECTOR4F

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_TVECTOR4F
	.type	RTTI_GEOMETRY_TVECTOR4F,@object
RTTI_GEOMETRY_TVECTOR4F:
	.byte	12
	.ascii	"\011TVector4f"
	.quad	4,4
	.quad	RTTI_SYSTEM_SINGLE
	.long	4
.Le66:
	.size	RTTI_GEOMETRY_TVECTOR4F, .Le66 - RTTI_GEOMETRY_TVECTOR4F

.section .data
	.balign 8
.globl	INIT_GEOMETRY_PVECTOR4F
	.type	INIT_GEOMETRY_PVECTOR4F,@object
INIT_GEOMETRY_PVECTOR4F:
	.byte	0
	.ascii	"\011PVector4f"
.Le67:
	.size	INIT_GEOMETRY_PVECTOR4F, .Le67 - INIT_GEOMETRY_PVECTOR4F

.section .data
	.balign 8
.globl	RTTI_GEOMETRY_PVECTOR4F
	.type	RTTI_GEOMETRY_PVECTOR4F,@object
RTTI_GEOMETRY_PVECTOR4F:
	.byte	0
	.ascii	"\011PVector4f"
.Le68:
	.size	RTTI_GEOMETRY_PVECTOR4F, .Le68 - RTTI_GEOMETRY_PVECTOR4F
# End asmlist al_rtti
# Begin asmlist al_dwarf_frame

.section .debug_frame
.Lc161:
	.long	.Lc163-.Lc162
.Lc162:
	.long	-1
	.byte	1
	.byte	0
	.uleb128	1
	.sleb128	-4
	.byte	16
	.byte	12
	.uleb128	7
	.uleb128	8
	.byte	5
	.uleb128	16
	.uleb128	2
	.balign 4,0
.Lc163:
	.long	.Lc165-.Lc164
.Lc164:
	.long	.Lc161
	.quad	.Lc1
	.quad	.Lc2-.Lc1
	.byte	4
	.long	.Lc3-.Lc1
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc4-.Lc3
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc5-.Lc4
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc165:
	.long	.Lc167-.Lc166
.Lc166:
	.long	.Lc161
	.quad	.Lc6
	.quad	.Lc7-.Lc6
	.byte	4
	.long	.Lc8-.Lc6
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc9-.Lc8
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc10-.Lc9
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc167:
	.long	.Lc169-.Lc168
.Lc168:
	.long	.Lc161
	.quad	.Lc11
	.quad	.Lc12-.Lc11
	.byte	4
	.long	.Lc13-.Lc11
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc14-.Lc13
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc15-.Lc14
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc169:
	.long	.Lc171-.Lc170
.Lc170:
	.long	.Lc161
	.quad	.Lc16
	.quad	.Lc17-.Lc16
	.byte	4
	.long	.Lc18-.Lc16
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc19-.Lc18
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc20-.Lc19
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc171:
	.long	.Lc173-.Lc172
.Lc172:
	.long	.Lc161
	.quad	.Lc21
	.quad	.Lc22-.Lc21
	.byte	4
	.long	.Lc23-.Lc21
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc24-.Lc23
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc25-.Lc24
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc173:
	.long	.Lc175-.Lc174
.Lc174:
	.long	.Lc161
	.quad	.Lc26
	.quad	.Lc27-.Lc26
	.byte	4
	.long	.Lc28-.Lc26
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc29-.Lc28
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc30-.Lc29
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc175:
	.long	.Lc177-.Lc176
.Lc176:
	.long	.Lc161
	.quad	.Lc31
	.quad	.Lc32-.Lc31
	.byte	4
	.long	.Lc33-.Lc31
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc34-.Lc33
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc35-.Lc34
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc177:
	.long	.Lc179-.Lc178
.Lc178:
	.long	.Lc161
	.quad	.Lc36
	.quad	.Lc37-.Lc36
	.byte	4
	.long	.Lc38-.Lc36
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc39-.Lc38
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc40-.Lc39
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc179:
	.long	.Lc181-.Lc180
.Lc180:
	.long	.Lc161
	.quad	.Lc41
	.quad	.Lc42-.Lc41
	.byte	4
	.long	.Lc43-.Lc41
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc44-.Lc43
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc45-.Lc44
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc181:
	.long	.Lc183-.Lc182
.Lc182:
	.long	.Lc161
	.quad	.Lc46
	.quad	.Lc47-.Lc46
	.byte	4
	.long	.Lc48-.Lc46
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc49-.Lc48
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc50-.Lc49
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc183:
	.long	.Lc185-.Lc184
.Lc184:
	.long	.Lc161
	.quad	.Lc51
	.quad	.Lc52-.Lc51
	.byte	4
	.long	.Lc53-.Lc51
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc54-.Lc53
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc55-.Lc54
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc185:
	.long	.Lc187-.Lc186
.Lc186:
	.long	.Lc161
	.quad	.Lc56
	.quad	.Lc57-.Lc56
	.byte	4
	.long	.Lc58-.Lc56
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc59-.Lc58
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc60-.Lc59
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc187:
	.long	.Lc189-.Lc188
.Lc188:
	.long	.Lc161
	.quad	.Lc61
	.quad	.Lc62-.Lc61
	.byte	4
	.long	.Lc63-.Lc61
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc64-.Lc63
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc65-.Lc64
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc189:
	.long	.Lc191-.Lc190
.Lc190:
	.long	.Lc161
	.quad	.Lc66
	.quad	.Lc67-.Lc66
	.byte	4
	.long	.Lc68-.Lc66
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc69-.Lc68
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc70-.Lc69
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc191:
	.long	.Lc193-.Lc192
.Lc192:
	.long	.Lc161
	.quad	.Lc71
	.quad	.Lc72-.Lc71
	.byte	4
	.long	.Lc73-.Lc71
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc74-.Lc73
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc75-.Lc74
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc193:
	.long	.Lc195-.Lc194
.Lc194:
	.long	.Lc161
	.quad	.Lc76
	.quad	.Lc77-.Lc76
	.byte	4
	.long	.Lc78-.Lc76
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc79-.Lc78
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc80-.Lc79
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc195:
	.long	.Lc197-.Lc196
.Lc196:
	.long	.Lc161
	.quad	.Lc81
	.quad	.Lc82-.Lc81
	.byte	4
	.long	.Lc83-.Lc81
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc84-.Lc83
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc85-.Lc84
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc197:
	.long	.Lc199-.Lc198
.Lc198:
	.long	.Lc161
	.quad	.Lc86
	.quad	.Lc87-.Lc86
	.byte	4
	.long	.Lc88-.Lc86
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc89-.Lc88
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc90-.Lc89
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc199:
	.long	.Lc201-.Lc200
.Lc200:
	.long	.Lc161
	.quad	.Lc91
	.quad	.Lc92-.Lc91
	.byte	4
	.long	.Lc93-.Lc91
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc94-.Lc93
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc95-.Lc94
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc201:
	.long	.Lc203-.Lc202
.Lc202:
	.long	.Lc161
	.quad	.Lc96
	.quad	.Lc97-.Lc96
	.byte	4
	.long	.Lc98-.Lc96
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc99-.Lc98
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc100-.Lc99
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc203:
	.long	.Lc205-.Lc204
.Lc204:
	.long	.Lc161
	.quad	.Lc101
	.quad	.Lc102-.Lc101
	.byte	4
	.long	.Lc103-.Lc101
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc104-.Lc103
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc105-.Lc104
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc205:
	.long	.Lc207-.Lc206
.Lc206:
	.long	.Lc161
	.quad	.Lc106
	.quad	.Lc107-.Lc106
	.byte	4
	.long	.Lc108-.Lc106
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc109-.Lc108
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc110-.Lc109
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc207:
	.long	.Lc209-.Lc208
.Lc208:
	.long	.Lc161
	.quad	.Lc111
	.quad	.Lc112-.Lc111
	.byte	4
	.long	.Lc113-.Lc111
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc114-.Lc113
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc115-.Lc114
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc209:
	.long	.Lc211-.Lc210
.Lc210:
	.long	.Lc161
	.quad	.Lc116
	.quad	.Lc117-.Lc116
	.byte	4
	.long	.Lc118-.Lc116
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc119-.Lc118
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc120-.Lc119
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc211:
	.long	.Lc213-.Lc212
.Lc212:
	.long	.Lc161
	.quad	.Lc121
	.quad	.Lc122-.Lc121
	.byte	4
	.long	.Lc123-.Lc121
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc124-.Lc123
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc125-.Lc124
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc213:
	.long	.Lc215-.Lc214
.Lc214:
	.long	.Lc161
	.quad	.Lc126
	.quad	.Lc127-.Lc126
	.byte	4
	.long	.Lc128-.Lc126
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc129-.Lc128
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc130-.Lc129
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc215:
	.long	.Lc217-.Lc216
.Lc216:
	.long	.Lc161
	.quad	.Lc131
	.quad	.Lc132-.Lc131
	.byte	4
	.long	.Lc133-.Lc131
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc134-.Lc133
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc135-.Lc134
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc217:
	.long	.Lc219-.Lc218
.Lc218:
	.long	.Lc161
	.quad	.Lc136
	.quad	.Lc137-.Lc136
	.byte	4
	.long	.Lc138-.Lc136
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc139-.Lc138
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc140-.Lc139
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc219:
	.long	.Lc221-.Lc220
.Lc220:
	.long	.Lc161
	.quad	.Lc141
	.quad	.Lc142-.Lc141
	.byte	4
	.long	.Lc143-.Lc141
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc144-.Lc143
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc145-.Lc144
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc221:
	.long	.Lc223-.Lc222
.Lc222:
	.long	.Lc161
	.quad	.Lc146
	.quad	.Lc147-.Lc146
	.byte	4
	.long	.Lc148-.Lc146
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc149-.Lc148
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc150-.Lc149
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc223:
	.long	.Lc225-.Lc224
.Lc224:
	.long	.Lc161
	.quad	.Lc151
	.quad	.Lc152-.Lc151
	.byte	4
	.long	.Lc153-.Lc151
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc154-.Lc153
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc155-.Lc154
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc225:
	.long	.Lc227-.Lc226
.Lc226:
	.long	.Lc161
	.quad	.Lc156
	.quad	.Lc157-.Lc156
	.byte	4
	.long	.Lc158-.Lc156
	.byte	14
	.uleb128	16
	.byte	4
	.long	.Lc159-.Lc158
	.byte	5
	.uleb128	6
	.uleb128	4
	.byte	4
	.long	.Lc160-.Lc159
	.byte	13
	.uleb128	6
	.balign 4,0
.Lc227:
# End asmlist al_dwarf_frame
# Begin asmlist al_dwarf_info
# End asmlist al_dwarf_info
# Begin asmlist al_dwarf_abbrev
# End asmlist al_dwarf_abbrev
# Begin asmlist al_dwarf_line
# End asmlist al_dwarf_line
# Begin asmlist al_picdata
# End asmlist al_picdata
# Begin asmlist al_resourcestrings
# End asmlist al_resourcestrings
# Begin asmlist al_end
# End asmlist al_end
.section .note.GNU-stack,"",%progbits

