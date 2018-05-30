
# TEST 8.3 - Check pipeline/cache dependencies
# Command: kta wcet pipdep.S main
# Expected output: WCET=6
# Command: kta wcet pipdep.S main -pipeline
# Expected output: WCET=11 # 1 dep
# Command: kta wcet pipdep.S main -cache
# Expected output: WCET=56 # 3 data misses + 2 instruction misses
# Command: kta wcet pipdep.S main -cache -pipeline
# Expected output: WCET=62 # + 1 cycle: i-miss following a d-miss (3 instr before)


.set noreorder
.data
var:
	.word 100

.text
.align 4

main:
	nop
	lw $t3, 0($t1)
	lw $t4, 0($t2)
	sw $t4, 0($t0)  # dep to prev instr.
	jr $ra
	nop
