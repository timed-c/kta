
# Factorial function according to Harris and Harris (2013)
# No branch delay slots

main:
	li	$a0,4
	jal 	fact
	
stop:	j	stop	
	

fact:
	addi 	$sp, $sp, -8
	sw 	$a0, 4($sp)
	sw 	$ra, 0($sp)
	addi 	$t0, $0, 2
	slt 	$t0, $a0, $t0
	beq 	$t0, $0, else
	addi 	$v0, $0, 1
	addi 	$sp, $sp, 8
	jr 	$ra
else:
	addi 	$a0, $a0,-1
	jal	fact
	lw	$ra, 0($sp)
	lw 	$a0, 4($sp)	
	addi 	$sp, $sp, 8
	mul	$v0, $a0, $v0
	jr	$ra
	