
# TEST 4 - check instruction cache - i-cache size 4096, direct-mapped, bs=16bytes
# Command: kta wcet icache2.S main
# Expected output: t0=0, WCET=510
# Command: kta wcet icache2.S main -pipeline
# Expected output: t0=0, WCET=514 (execute stage 1 cycle)
# Command: kta wcet icache2.S main -cache
# Expected output: t0=0, WCET=21010  (2x100 + 2x100x100 (loop misses) + 3x100)
# Command: kta wcet icache2.S main -cache -pipeline
# Expected output: t0=0, WCET=21014


.set noreorder
.data
.align 12  # 4096 bytes i-cache size

table:
        .word  0,1,2,4,5,6,7,8,9

.text
.align 12

loop1: 
	beq $a0, $a1, out
	nop
	addi $a0, 1
	j loop2
	nop

.align 12

loop2: 
	beq $a0, $a1, out
	nop
	addi $a0, 1
	j loop1
	nop

.align 12
out: 
	jr $ra
	nop
main:
        li      $a1,100      #Instruction Miss: 2 instructions
        li	$a0, 0
	jal     loop1         #Instruction Miss
	nop
	jr $ra
	nop
