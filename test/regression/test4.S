
# TEST 8.4 - Check pipeline/cache dependencies
# Command: kta wcet pipdep.S main
# Expected output: WCET=6
# Command: kta wcet pipdep.S main -pipeline
# Expected output: WCET=10  # No deps
# Command: kta wcet pipdep.S main -cache
# Expected output: WCET=36 (2 i-misses + 1 d-miss)
# Command: kta wcet pipdep.S main -cache -pipeline
# Expected output: WCET=41 # i-miss + load


.set noreorder
.data
var:
	.word 100

.text
.align 4

main:
	nop
	lw $t3, 0($t1)
	nop		# lw $t3, 0($t1): WCET=310
	nop		# lw $t3, 0($t1): WCET=309
	jr $ra
	nop
