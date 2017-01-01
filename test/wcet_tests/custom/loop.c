void test2() {
  asm("       addi $t0,$zero,100");
  asm("       lui $s0,0x1001");
  asm("       ori $s0,$s0,400");
  asm("loop2: lw $t1,0($s0)");
  asm("       add $s0,$s0,8");
  asm("       addi $t0,$t0,-1");
  asm("       bne $t0,$zero,loop2");
}

void test1() {
  asm("       lui $s0,0x1234");
  asm("       ori $s0,0x2000");
  asm("       addi $t0,$zero,20");
  asm("loop1: add $t1,$t0,$s0");
  asm("       lw $t2,-4($t1)");
  asm("       addi $t0,$t0,-4");
  asm("       bne $t0,$zero,loop1");
}



void test0() {
  asm("       lui $s0, 0x1001");
  asm("       addi $t0, $0, 0");
  asm("       addi $t1, $0, 0");
  asm("loop0: slti $t2, $t0, 2000");
  asm("       beq $t2, $0, _done");
  asm("       lw $t2, 0($s0)");
  asm("       add $t1, $t1, $t2");
  asm("       addi $s0, $s0, 4");
  asm("       addi $t0, $t0, 1");
  asm("       j loop0");
  asm("_done:");
}


