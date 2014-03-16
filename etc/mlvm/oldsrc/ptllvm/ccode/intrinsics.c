
extern float mytest(float,int) asm("llvm.mytest");

int main()
{
  float i = 11.11;
  float t = mytest(i, 44);
  return 0;
}
