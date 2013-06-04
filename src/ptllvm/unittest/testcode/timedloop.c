

#include "pret.h"

extern int sensing();
extern int computation(int);
extern void actuating(int);



int main(int args, char **argv)
{
  
  int v;
  int i;
  int first = 1;
  int k = 2;
  
  int period = 10000;

  long x = get_time();
 
  for(;;){

    if(!first){
      delay_until(x);
      mtfd_end();
    }

    mtfd_begin(period);    
    if(!first)
      actuating(k);  
    v = sensing();  
    x += period;    
    k = computation(v);      
    first = 0;
  }

  // The interesting thing is that the final code
  // includes to call to pret_mt

  return k;
}
