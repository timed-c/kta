
# TEST 8.2 - Check pipeline/cache dependencies
# Command: kta wcet test2.S main
# Expected output: WCET=6
# Command: kta wcet test2.S main -pipeline
# Expected output: WCET=10 
# Command: kta wcet test2.S main -cache
# Expected output: WCET=46 
# Command: kta wcet test2.S main -cache -pipeline
# Expected output: WCET=51 # 1 extra cycle due to the data miss by "lw"


.set noreorder
.data
var:
	.word 100

.text
.align 4

main:
	nop			# I-miss
	lw $t3, 0($t1)		# D-miss
	add $t4, $t2, $t1	# No dep
	sw $t4, 0($t0)		# D-miss
	jr $ra			# I-miss
	nop			# -
