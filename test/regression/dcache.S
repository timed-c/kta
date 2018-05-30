
# TEST 5 - check data cache - size 4096
# Command: kta wcet dcache.S main
# Expected output:  WCET=38 //1410
# Command: kta wcet dcache.S main -pipeline
# Expected output:  WCET=46 //1614 (execute stage 1 cycle - 2 dependencies lw -> add, addi -> beq (next loop))
# Command: kta wcet dcache.S main -cache
# Expected output:  WCET=148 //1138 //24510  
# Command: kta wcet dcache.S main -cache -pipeline
# Expected output:  WCET=155 //1145 //24713 - d-cache miss -> i-cache miss - One cycle less


.set noreorder
.data
.align 12  # 4096 bytes i-cache size

table1:
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
.align 12  # 4096 bytes i-cache size

table2:
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
        .word  0,1,2,4,5,6,7,8,9
.text
.align 12

loop: 
	beq $a0, $a1, out
	nop
	la $t0, table1
	la $t1, table2
	add $t0, $a0
	add $t1, $a0
	lw $t2, 0($t0)
	lw $t3, 0($t1)
	add $t4, $t2, $t3
	sw $t4, 0($t0)
	j loop
	addi $a0, 4

out: 
	jr $ra
	nop
main:
        li      $a1, 8      #Instruction Miss: 2 instructions
        li	$a0, 0
	jal     loop         #Instruction Miss
	nop
	jr $ra
	nop
