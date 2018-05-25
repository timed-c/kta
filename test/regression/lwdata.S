
# TEST 1 - check declared data, no pipeline, no cache
# Command: kta wcet lwdata.S main
# Expected output: t0=0, t1=1, t2=3 WCET=5


.data
.align 2

table:
        .word  0,1,2,4,5,6,7,8,9

.text

main:
        la      $a1,table
        lw      $t0,0($a1)
        lw      $t1,4($a1)
        lw      $t2,8($a1)
        jr      $ra
