


int v;
int k = 100;

int foo(int x){
  return x + k + v;
}

int main(){
  k = 5;
  v = 3;
  return foo(4);
}


/*

  gp = 409040

  main  0x00400018
  foo   0x00400030
 
 .text  0x00400018   2c
 .sbss  0x00401048   4      
 .sdata 0x00401044   4

store k = 5
x - 32764 = 4198472 ==>   x = 4231236,  0x409044

401040

addiu   $v1,$0,5
addiu   $v0,$0,12
sw      $v1,-32764($gp)
addiu   $v1,$0,3
jr      $ra
sw      $v1,-32760($gp)
lw      $v0,-32764($gp)
lw      $v1,-32760($gp)
addu    $v0,$a0,$v0
jr      $ra
addu    $v0,$v0,$v1


00401048 S __bss_start
00401048 G _edata
0040104c S _end
00401048 S _fbss
00401044 G _fdata
00400018 T _ftext
00409040 a _gp
         U _start
00400030 T foo
00401044 G k
00400018 T main
00401048 S v

*/








