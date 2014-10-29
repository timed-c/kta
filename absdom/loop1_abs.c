

#include <stdio.h>
#include "absint_interval.h"

// This test function is not finished. First cur 2014-10-29.

 void foo(int max_l, int max_h, int k_l, int k_h, int *res_l, int *res_h)
{

  aint(i1,32);
  aint(i2,32);
  aint(i3,32);

  aint(r1,32);
  aint(r2,32);
  aint(r3,32);

  aint(max,32);
  aint(k,32);

  aint(t_max,32);
  aint(t_i2,32);

  aint(m_r2,32);
  
  int c2;

  int last;

  set_interval(max,max_l,max_h,32);
  set_interval(k,k_l,k_h,32);

b1:
  last = 1;
  set_interval(i1,0,0,32);  
  set_interval(r1,0,0,32);  
  goto b2;
  //%i1 = add i32 0, 0 
  //%r1 = add i32 0, 0
  //br label %b2
b2: 
  if(last == 1){  // comes from b1
     move(i2,i1);
     move(r2,r1);
  }
  else{ //comes from b3
     move(i2,i3);
     move(r2,r3);
  }
  icmp_slt_mtc(c2,t_i2,t_max,i2,max,32);
  merge(m_r2,m_r2,r2,32);
  if(c2 == 0){
    // Comment. Decide how to merge this. For which variables should
    // we introduce merge variables.
 
  }
  else{ //Either true or true/false

  }
  //%i2 = phi i32 [%i1, %b1], [%i3, %b3]  
  //%r2 = phi i32 [%r1, %b1], [%r3, %b3]
  //%c2 = icmp slt i32 %i2, %max
  //br i1 %c2, label %b3, label %b4

b3:
  //  %r3 = add i32 %r2, %i2
  //%i3 = add i32 %i2, %k
  //br label %b2

b4:
  //ret i32 %r2


  //int i;
  //int r = 0;
  //for(i=0; i < max_h; i += k_h){
  //  r += i;
  //}
  *res_l = 0;  //Should be changed to get high and low
  *res_h = 0;
}

int main(){
  int res_l, res_h;

  foo(1000, 1000, 3, 3, &res_l, &res_h);
  printf("Result: [%d,%d]\n", res_l, res_h);
  return 0;
}
