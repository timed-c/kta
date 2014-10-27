



#include <stdio.h>
#include "absint_interval.h"


void test_add()
{
  aint(x,32);
  aint(y,32);
  aint(z,32);
  

  set_interval(x,3,100,32);
  set_interval(y,10,200,32);
  add(z,x,y,32);
  test_interval("add: positive values (32-bit)",z,13,300);

  set_interval(x,3,20,32);
  set_interval(y,-10,40,32);
  add(z,x,y,32);
  test_interval("add: negative add lower (32-bit)",z,-7,60);

  set_interval(x,3,25,32);
  set_interval(y,-50,-10,32);
  add(z,x,y,32);
  test_interval("add: negative add lower and high (32-bit)",z,-47,15);

  set_interval(x,-30,-5,32);
  set_interval(y,-50,-10,32);
  add(z,x,y,32);
  test_interval("add: lower and high from neg val (32-bit)",z,-80,-15);
    
  set_interval(x,30,100,8);
  set_interval(y,50,156,8);
  add(z,x,y,8);
  test_interval("add: max val goes over max (8-bit)",z,777,-777);

  set_interval(x,2,3,3);
  set_interval(y,1,2,3);
  add(z,x,y,3);
  test_interval("add: 3-bits ord sum with ord (case 1)",z,777,-777);

  set_interval(x,0,1,3);
  set_interval(y,2,2,3);
  add(z,x,y,3);
  test_interval("add: 3-bits ord sum with ord (case 2)",z,2,3);

  set_interval(x,-3,1,3);
  set_interval(y,-1,2,3);
  add(z,x,y,3);
  test_interval("add: 3-bits ord sum with ord (case 3)",z,-4,3);

  set_interval(x,-3,1,3);
  set_interval(y,-2,2,3);
  add(z,x,y,3);
  test_interval("add: 3-bits ord sum with ord (case 4)",z,777,-777);

}


void test_merge()
{
  aint(x,32);
  aint(y,32);
  aint(z,32);

  set_interval(x,100,1000,32);
  set_interval(y,50,3000,32);
  merge(z,x,y,32);
  test_interval("merge: postive values (32-bit)",z,50,3000);

  set_interval(x,-400,1000,32);
  set_interval(y,-9000,-2000,32);
  merge(z,x,y,32);
  test_interval("merge: negative values (32-bit)",z,-9000,1000);

  set_interval(x,100,100,32);
  set_interval(y,500,500,32);
  merge(z,x,y,32);
  test_interval("merge: two numbers (32-bit)",z,100,500);  
}


int main()
{
    ptver_startup_check();
 
    test_add();
    test_merge();
  return 0;
}



