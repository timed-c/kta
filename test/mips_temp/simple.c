
int k;
int v = 1;

/*
int foo(int x, int y){
  return x + y * 10;
}
*/


// Tests: slt,bne,sll,jr,mul,addiu
int foo(int x, int y){
  int z = x * y;
  if(x > y)
    return x;
  else
    return z;
}


/*
strange asm using xor and no branch
int foo(int x, int y){
  int z = x * y;
  if(x == y)
    return x;
  else
    return z;
}
*/

int main()
{
  return 33;
}
