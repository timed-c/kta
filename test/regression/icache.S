
# TEST 3 - check instruction cache, i-cache size 4096, direct-mapped, bs=16bytes
# Command: kta wcet icache.S main
# Expected output: t0=0, WCET=510 (4 (jal) + 500 (loop) + 2 (beq) + 2 (out) + 2 (main return)
# Command: kta wcet icache.S main -pipeline
# Expected output: t0=0, WCET=514 (execute stage 1 cycle)
# Command: kta wcet icache.S main -cache
# Expected output: t0=0, WCET=910
# Command: kta wcet icache.S main -cache -pipeline
# Expected output: t0=0, WCET=914


.set noreorder
.data
.align 2

table:
        .word  0,1,2,4,5,6,7,8,9

.text

loop: 
	beq $a0, $a1, out
	nop
	addi $a0, 1
	j loop
	nop
out: 
	jr $ra
	nop

main:
        li      $a1,100     #Instruction Miss: 2 instructions
        li	$a0, 0
	jal     loop        #Instruction Miss
	nop
	jr $ra
	nop
