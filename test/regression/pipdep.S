
# TEST 7 - Check pipeline dependencies
# Command: kta wcet pipdep.S main
# Expected output: WCET=13
# Command: kta wcet pipdep.S main -pipeline
# Expected output: WCET=19  ( 4 + 13 + 2 dependencies) dep1: #2,#3 dep2: #3,#4
# Command: kta wcet pipdep.S main -cache
# Expected output: WCET=63 (4 i-misses + 1 d-miss)
# Command: kta wcet pipdep.S main -cache -pipeline
# Expected output: WCET=69


.set noreorder
.data
var:
	.word 100

.text
.align 4


loop:
	la   $t2, var
	lw   $t3, 0($t2)
	slt   $t4, $t3, $a0 
	beqz  $t4, out
	nop
	j loop
	addi $a0, $a0,1
out: 
	jr $ra
	nop


main:
	addi $a0, $zero, 100
	jal loop
	nop
	jr $ra
	nop
