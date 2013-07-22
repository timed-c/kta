

#include "ptc.h"
#include "stdio.h"

int global =5;

//We do not want the tasks to be functions. They can be more
//specific (WCET) if we know more about the input.
volatile int ext = 2;

int sensing(){
  int k=ext;
  for(int i=0; i< global; i++)
    k = k * global;
  return 5+k;
}
void actuating(int val){
  ext = val;
  return;
}
int computation(int val)
{
  return ext + val;
}


int main()
{
  /*
    global = 10;
    int k1 = sensing();
    int k2 = computation(k1);
    actuating(k2);
    global = 5;
  */

    //Prefix, precision bounds, period 
    int first = 1;
    int u, y;
    begin_periodic_task(6,10,100);
      if(!first){
        first = 0;
        actuating(y);  
      }
      u = sensing();
      y = computation(u);    
      first = 0;
    end_periodic_task();

  return 0;

}
  
/*
begin_task

  begin_task 
  sensedtata  max time 
  if newdata then
    end_task
  end_task
*/









