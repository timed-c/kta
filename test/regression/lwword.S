
# TEST 6 - check instruction/data cache
# Command: kta wcet lwword.S main
# Expected output: t0=0, WCET=5
# Command: kta wcet lwword.S main -pipeline
# Expected output: t0=0, WCET=9
# Command: kta wcet lwword.S main -cache
# Expected output: t0=0, WCET=35
# Command: kta wcet lwword.S main -cache -pipeline
# Expected output: t0=0, WCET=39


.set noreorder
.data
.align 2

table:
        .word  0,1,2,4,5,6,7,8,9

.text

main:
        la      $a1,table   #Instruction Miss: 2 instructions
        lw      $t0,0($a1)  #Data Miss
        jr      $ra         #Instruction Miss
	nop
