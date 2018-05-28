# TEST 8.1 - Check pipeline/cache dependencies
# Command: kta wcet test.S main
# Expected output: WCET=6
# Command: kta wcet test.S main -pipeline
# Expected output: WCET=11
# Command: kta wcet test.S main -cache
# Expected output: WCET=406 
# Command: kta wcet test.S main -cache -pipeline
# Expected output: WCET=410 (1 cycle less due to pipeline)


.set noreorder
.data
var:
	.word 100

.text
.align 4

main:
	nop			# I-miss
	lw $t3, 0($t1)  	# D-miss
	add $t4, $t2, $t3	# dep with prev instr.
	sw $t4, 0($t0)          # D-miss
	jr $ra                  # I-miss
	nop
