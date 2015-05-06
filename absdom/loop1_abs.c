

#include <stdio.h>
#include "absint_interval.h"

// This test function is not finished. First cur 2014-10-29.

 void foo(int max_l, int max_h, int k_l, int k_h, int *res_l, int *res_h)
{

  aint(i1_0,32);
  aint(i2_1,32);
  aint(i3_1,32);

  aint(r1_0,32);
  aint(r2_0,32);
  aint(r2_1,32);
  aint(r3_1,32);

  aint(max,32);
  aint(k,32);

  aint(t_max,32);
  aint(t_i2_1,32);

  int c2;
  int e1_2, e2_3;

  set_interval(max,max_l,max_h,32);
  set_interval(k,k_l,k_h,32);


b1:

  set_interval(i1_0,0,0,32);  
  set_interval(r1_0,0,0,32);  
  e1_2 = 1;
  e2_3 = 0;  

  // goto b2;
  //%i1 = add i32 0, 0 
  //%r1 = add i32 0, 0
  //br label %b2

b2: 
  if(e1_2){  
     move(i2_1,i1_0);
     move(r2_1,r1_0);
  }
  else{ //comes from b3
     move(i2_1,i3_1);
     move(r2_1,r3_1);
  }
  //printf("i2_1: %ld\n", get_low(i2_1));

  //Issue: We should not always merge.
  e1_2 = 0;
  e2_3 = 1; //Not really needed in this case

  //Issue. What do we do with t_max? Can it safely not be used?
  //       Can it be safe to ignore since max can only be smaller?
  //Issue. The true or true or false case must both store the 
  //       new values for i2_1 in t_? 
  icmp_slt_mtc(c2,t_i2_1,t_max,i2_1,max,32);
  if(c2 == 0){
    move(r2_0, r2_1);
    goto b4;
  }
  else if(c2 == 2){
    merge(r2_0,r2_0,r2_1,32);
  }


  //printf("t_i2_1: %ld\n", get_low(t_i2_1));
  //printf("i2_1: %ld\n", get_low(i2_1));
 
  //%i2 = phi i32 [%i1, %b1], [%i3, %b3]  
  //%r2 = phi i32 [%r1, %b1], [%r3, %b3]
  //%c2 = icmp slt i32 %i2, %max
  //br i1 %c2, label %b3, label %b4

//b3:
  add(r3_1, r2_1, i2_1, 32);
  add(i3_1, t_i2_1, k, 32);
  //printf("i3_1: %ld\n", get_low(i3_1));
  goto b2; 

  //  %r3 = add i32 %r2, %i2
  //%i3 = add i32 %i2, %k
  //br label %b2

b4:
  //ret i32 %r2

  *res_l = get_low(r2_0);  //Should be changed to get high and low
  *res_h = get_high(r2_0);
}

int main(){
  int res_l, res_h;

  foo(1000, 1000, 3, 3, &res_l, &res_h);
  printf("Result: [%d,%d]\n", res_l, res_h);
  return 0;
}
