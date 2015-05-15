

int main(){
  __asm__ __volatile__ (
   "nop\n"
   "nop\n"
   "add $0,$at,$v0\n"        // 'add' and test all registers 
   "add $v1,$a0,$a1\n"
   "add $a2,$a3,$t0\n"
   "add $t1,$t2,$t3\n"
   "add $t4,$t5,$t6\n"
   "add $t7,$s0,$s1\n"
   "add $s2,$s3,$s4\n"
   "add $s5,$s6,$s7\n"
   "add $t8,$t9,$k0\n"
   "add $k1,$gp,$sp\n"
   "add $fp,$ra,$t0\n"
   "nop\n"
   "nop\n"
   "addi $t0,$s0,100\n"      // 'addi' and test of sign extension for immediate
   "addi $t0,$s0,-100\n"      
   "addi $t0,$s0,32767\n"      
   "addi $t0,$s0,-32768\n"      
   "addi $t0,$s0,-1\n"      
   "addi $t0,$s0,0\n"      
   "nop\n"
   "nop\n"
   "addiu $s0,$s1,555\n"     // 'addiu' 
   "addu $t1,$t2,$t3\n"      // 'addu'
   "\n"
   "\n"
   "nop\n"
   :::);
}

