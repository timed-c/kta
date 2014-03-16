

#include "pret.h"

int main(int args, char **argv)
{
  
  int i;
  int k = 2;
  
  int period = 10000;
  long x = get_time();
  x += period;
  mtfd_begin(period);
  
  for(i=0; i<args; i++)
    k *= i;

  mtfd_end();
  delay_until(x);

  return k;
}



