

#include <stdio.h>

extern float sqrt2(float) asm("llvm.sqrt.f32");

int main()
{
  float i = sqrt2(10.0);
  printf("Hello %f\n", i);
  return 0;
}
