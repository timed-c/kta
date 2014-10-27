



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


void test_icmp_slt_mtc()
{
  aint(x,32);
  aint(y,32);
  int z;
  aint(tx,32);
  aint(ty,32);

  set_interval(x,10,100,32);
  set_interval(y,101,500,32);
  icmp_slt_mtc(z,tx,ty,x,y,32);
  test_case("icmp_slt_mtc: clear true case", z, 1);

  set_interval(x,10,100,32);
  set_interval(y,100,500,32);
  icmp_slt_mtc(z,tx,ty,x,y,32);
  test_case("icmp_slt_mtc: not a clear true case", z, 2);

  set_interval(x,-100,-50,32);
  set_interval(y,-500,-101,32);
  icmp_slt_mtc(z,tx,ty,x,y,32);
  test_case("icmp_slt_mtc: clear false case", z, 0);

  set_interval(x,-100,-50,32);
  set_interval(y,-500,-99,32);
  icmp_slt_mtc(z,tx,ty,x,y,32);
  test_case("icmp_slt_mtc: not a clear false case", z, 2);

  //case 1:
  //0123456789  
  //    xxx
  //  yyyyyyyy
  set_interval(x,4,6,32);
  set_interval(y,2,9,32);
  icmp_slt_mtc(z,tx,ty,x,y,32);
  test_case(    "icmp_slt_mtc: smaller domain (case 1)", z, 2);
  test_interval("              tx output",tx,4,6);  
  test_interval("              ty output",ty,5,9);  

  //case 2:
  //0123456789  
  //    xxxx
  //  yyyy
  set_interval(x,4,7,32);
  set_interval(y,2,5,32);
  icmp_slt_mtc(z,tx,ty,x,y,32);
  test_case(    "icmp_slt_mtc: smaller domain (case 2)", z, 2);
  test_interval("              tx output",tx,4,4);  
  test_interval("              ty output",ty,5,5);  


  //case 3:
  //0123456789  
  //    xxx
  //     yyyy
  set_interval(x,4,6,32);
  set_interval(y,5,8,32);
  icmp_slt_mtc(z,tx,ty,x,y,32);
  test_case(    "icmp_slt_mtc: smaller domain (case 3)", z, 2);
  test_interval("              tx output",tx,4,6);  
  test_interval("              ty output",ty,5,8);  


  //case 4:
  //0123456789  
  //     xxx
  //     y
  set_interval(x,5,7,32);
  set_interval(y,5,5,32);
  icmp_slt_mtc(z,tx,ty,x,y,32);
  test_case(    "icmp_slt_mtc: smaller domain (case 4)", z, 0);


}


int main()
{
  ptver_startup_check();
 
  test_add();
  test_merge();
  test_icmp_slt_mtc();
  return 0;
}



