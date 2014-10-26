

#include <stdio.h>
#include "absint_interval.h"


void test_add_instr()
{
  aint32(x);
  aint32(y);
  aint32(z);
    
  /* Positive add */
  aint_set_interval(x,3,100,32);
  aint_set_interval(y,10,200,23);
  aint_add(z,x,y,32);
  test_aint(z,13,300);

  /* Negative add lower */
  aint_set_interval(x,3,20,32);
  aint_set_interval(y,-10,40,32);
  aint_add(z,x,y,32);
  test_aint(z,-7,60);

  /* Negative add lower and high */
  aint_set_interval(x,3,25,32);
  aint_set_interval(y,-50,-10,32);
  aint_add(z,x,y,32);
  test_aint(z,-47,15);

  /* Negative add lower and high from neg val*/
  aint_set_interval(x,-30,-5,32);
  aint_set_interval(y,-50,-10,32);
  aint_add(z,x,y,32);
  test_aint(z,-80,-15);
  
  /* Adding where max val goes over max (8 bits val)*/
  aint_set_interval(x,30,100,8);
  aint_set_interval(y,50,156,8);
  aint_add(z,x,y,8);
  test_aint(z,80,0);

  /* 3-bits ord sum with ord (case 1)*/
  aint_set_interval(x,2,3,3);
  aint_set_interval(y,1,2,3);
  aint_add(z,x,y,3);
  test_aint(z,3,5);

  /* 3-bits ord sum with ord (case 2)*/
  aint_set_interval(x,5,6,3);
  aint_set_interval(y,1,3,3);
  aint_add(z,x,y,3);
  test_aint(z,6,1);

  /* 3-bits ord sum with ord (case 3)*/
  aint_set_interval(x,7,7,3);
  aint_set_interval(y,7,7,3);
  aint_add(z,x,y,3);
  test_aint(z,6,6);

  /* 3-bits ord sum with ord (case 4)*/
  aint_set_interval(x,0,7,3);
  aint_set_interval(y,7,7,3);
  aint_add(z,x,y,3);
  test_aint(z,7,6);

  /* 3-bits ord sum with swap (case 1)*/
  aint_set_interval(x,1,3,3);
  aint_set_interval(y,6,1,3);
  aint_add(z,x,y,3);
  test_aint(z,7,4);

  /* 3-bits ord sum with swap (case 2)*/
  aint_set_interval(x,7,7,3);
  aint_set_interval(y,6,1,3);
  aint_add(z,x,y,3);
  test_aint(z,0,7);

  /* 3-bits ord sum with swap (case 3)*/
  aint_set_interval(x,1,7,3);
  aint_set_interval(y,6,1,3);
  aint_add(z,x,y,3);
  test_aint(z,0,7);

  /* 3-bits ord sum with swap (case 4)*/
  aint_set_interval(x,3,6,3);
  aint_set_interval(y,6,1,3);
  aint_add(z,x,y,3);
  test_aint(z,1,7);

  /* 3-bits swap sum with ord (case 1)*/
  aint_set_interval(x,6,1,3);
  aint_set_interval(y,1,3,3);
  aint_add(z,x,y,3);
  test_aint(z,7,4);

  /* 3-bits swap sum with ord (case 2)*/
  aint_set_interval(x,6,1,3);
  aint_set_interval(y,1,7,3);
  aint_add(z,x,y,3);
  test_aint(z,0,7);

  /* 3-bits swap sum with ord (case 3)*/
  aint_set_interval(x,6,1,3);
  aint_set_interval(y,3,6,3);
  aint_add(z,x,y,3);
  test_aint(z,1,7);

  /* 3-bits swap sum with swap */
  aint_set_interval(x,2,1,3);
  aint_set_interval(y,3,2,3);
  aint_add(z,x,y,3);
  test_aint(z,0,7);

  /* Adding where min and max val goes over max for 32 bit width */
  aint_set_interval(x,-20,-10,32);
  aint_set_interval(y,20,100,32);
  aint_add(z,x,y,32);
  test_aint(z,0,90);
  
}

void test_mul_instr()
{
  aint32(x);
  aint32(y);
  aint32(z);

  /* Legal positive values */
  aint_set_interval(x,20,40,32);
  aint_set_interval(y,10,100,32);
  aint_mul(z,x,y,32);
  test_aint(z,200,4000);  

  /* Multiply too large values */
  aint_set_interval(x,20,40,32);
  aint_set_interval(y,10,0xffffff,32);
  aint_mul(z,x,y,32);
  test_aint(z,0,0xffffffff);  

  /* Multiply negative number with interval*/
  aint_set_interval(x,20,40,32);
  aint_set_interval(y,-2,-2,32);
  aint_mul(z,x,y,32);
  test_aint(z,0,0xffffffff);  

  /* Multiply negative without interval */
  aint_set_interval(x,40,40,32);
  aint_set_interval(y,-2,-2,32);
  aint_mul(z,x,y,32);
  test_aint(z,-80,-80);  
}





int main()
{
  ptver_startup_check();
 
  test_add_instr();
  test_mul_instr();

  return 0;
}



