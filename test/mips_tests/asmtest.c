

int main(){
  __asm__ __volatile__ (
   "nop\n"
   "nop\n"
   "add   $0,$at,$v0\n"       // 'add' and test all registers 
   "add   $v1,$a0,$a1\n"
   "add   $a2,$a3,$t0\n"
   "add   $t1,$t2,$t3\n"
   "add   $t4,$t5,$t6\n"
   "add   $t7,$s0,$s1\n"
   "add   $s2,$s3,$s4\n"
   "add   $s5,$s6,$s7\n"
   "add   $t8,$t9,$k0\n"
   "add   $k1,$gp,$sp\n"
   "add   $fp,$ra,$t0\n"
   "nop\n"
   "nop\n"
   "addi  $t0,$s0,100\n"      // 'addi' and test of sign extension for immediate
   "addi  $t0,$s0,-100\n"      
   "addi  $t0,$s0,32767\n"      
   "addi  $t0,$s0,-32768\n"      
   "addi  $t0,$s0,-1\n"      
   "addi  $t0,$s0,0\n"      
   "nop\n"
   "nop\n"
   "addiu $s0,$s1,555\n"      // 'addiu' 
   "addu  $t1,$t2,$t3\n"      // 'addu'
   "and   $t1,$t2,$t3\n"      // 'and'
   "andi  $s0,$s1,555\n"      // 'andi' 
   "nop\n"
   "nop\n"
   "beq   $t1,$t2,label1\n"   // 'beq'
   "nop\n"
   "label1:\n"
   "addi  $t1,$0,0\n"
   "bne   $t1,$0,label1\n"    // 'bne'
   "nop\n"
   "jalr  $t1\n"              // 'jalr'
   "jr    $t1\n"              // 'jr'
   "label2:\n"
   "j     label2\n"           // 'j'
   "jal   label2\n"           // 'jal'
   "nop\n"
   "nop\n"
   "lb    $t1,0($t2)\n"       // 'lb'
   "lb    $t1,256($t2)\n"     
   "lb    $t1,-256($t2)\n"    
   "lb    $t1,32767($t2)\n"   
   "lb    $t1,-1($t2)\n"     
   "nop\n"
   "lbu   $t1,500($t2)\n"     // 'lbu'
   "lbu   $t1,-1($t2)\n"      
   "nop\n"
   "nop\n"
   "lui   $t1,100\n"          // 'lui'
   "nop\n"
   "nop\n"
   "lw    $t1,500($t2)\n"     // 'lw'
   "mul   $t1,$t2,$t3\n"      // 'mul'
   "nor   $t1,$t2,$t3\n"      // 'nor'
   "or    $t1,$t2,$t3\n"      // 'or'
   "ori   $t1,$t2,8\n"        // 'ori'
   "nop\n"
   "nop\n"
   "slt   $t1,$t2,$t3\n"      // 'slt'
   "sltu  $t1,$t2,$t3\n"      // 'sltu'
   "slti  $t1,$t2,100\n"      // 'slti'
   "sltiu $t1,$t2,100\n"      // 'sltiu'
   "nop\n"
   "nop\n"
   "sll   $t1,$t2,0\n"        // 'sll' and test different shamt
   "sll   $t1,$t2,4\n"         
   "sll   $t1,$t2,31\n"        
   "nop\n"
   "nop\n"
   "sllv  $t1,$t2,$t3\n"      // 'sllv'
   "sra   $t1,$t2,8\n"        // 'sra'
   "srl   $t1,$t2,8\n"        // 'srl'
   "srlv  $t1,$t2,$t3\n"      // 'srlv'
   "nop\n"
   "nop\n"
   "sb    $t1,500($t2)\n"     // 'sb'
   "sw    $t1,500($t2)\n"     // 'sw'
   "nop\n"
   "nop\n"
   "sub   $t1,$t2,$t3\n"      // 'sub'
   "subu  $t1,$t2,$t3\n"      // 'subu'
   "xor   $t1,$t2,$t3\n"      // 'xor'
   "xori  $t1,$t2,500\n"      // 'xori'
   :::);
}














