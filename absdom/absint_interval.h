

#include <stdio.h>
#include <stdlib.h>

#ifndef _ABS_INTERVAL_DOM
#define _ABS_INTERVAL_DOM

/*
  The following library implements a very simple integer interval domain on
  fixed size integers. 

  For each instruction, the user specifies the bitwith of the operation. All integers
  are overapproximated to be assumed to represent signed integers. If an operation
  results in signed overflow, the result will be TOP. 

  Each value in the domain are represented by two values: high and low. 
  If high < low, then the value is a TOP element. We ususally use the interval
  [777,-777] to indicate this (just to make it easy to spot when we print the numbers).

  The max and min values with witdth w are 
   maxval = (2 << (w-1)) - 1
   minval = (2 << (w-1)) * -1

  Example 3-bit value. Signed numbers are represented using two's complement

  Bin  Unsigned   Signed
  000  0          0
  001  1          1
  010  2          2
  011  3          3
  100  4         -4
  101  5         -3
  110  6         -2
  111  7         -1

  00   0          0
  01   1          1
  10   2         -2
  11   3         -1

  0    0          0
  1    1         -1

  In this case, MAX = 3 and MIN = -4

  In the following, the semantics of each operation is described briefly
  
  ADD: z = x + y
   - If x = TOP or y = TOP then z = TOP 
   - Let th = x.high + y.high and tl = x.low + y.low
       if th > maxval or tl < minval then z = TOP
       else z.high = th and z.low = tl
   - Note that we do not need to compare th < minval because 
        if th < minval, then tl < minval.
*/


#define intbits ((sizeof(int))*8)

#define mask(w) ((w) == sizeof(long)*8 ? 0xffffffff : (1 << (w)) - 1)

#define maxval(w) ((1l << (w-1)) - 1)
#define minval(w) ((1l << (w-1)) * -1)
#define aint(name,w) long name##low, name##high

#define is_top(x) (x##high < x##low)

//#define sign_extend(x,w) ((((int)(x)) << (intbits - (w))) >> (intbits - (w)))

/* Set a variable to an interval. w is the word bith length  */
#define set_interval(name,l,h,w) \
  name##low = l; \
  name##high = h

/* Adds x and y and stores into z. Assumes that 
   all abstract values have w bit width */
#define add(z,x,y,w)\
  if(is_top(x) || is_top(y))\
    {z##high = -777; z##low = 777;}  \
  else\
    {if(x##high + y##high > maxval(w) || x##low + y##low < minval(w))   \
       {z##high = -777; z##low = 777;} \
     else{\
         z##high = x##high + y##high;  \
         z##low = x##low + y##low; \
       }\
    }
  


/* Merges the abstract values x and y and saves the result
   in z. Parameter w is the bit width. This macro assumes
   that both values x and y have the same bitwith w. */
//#define merge(z,x,y,w)                        \

#define icmp_sgt(z,x,y,w)\
if((sign_extend(x##high) >= sign_extend(x##low) &&\
    (sign_extend(y##high) >= sign_extend(y##low))\
  {\
    if((sign_extend(x##low) > sign_extend(y##high))\
    { \
      z##low = 1;\
      z##high = 1;\
    }\
    else if((sign_extend(x##high) <= sign_extend(y##low))\
    {\
      z##low = 0;\
      z##high = 0;\
    }\
    else\
    {\
      z##low = 0;\
      z##high = 1;\
    }\
  else\
  {\
    z##low = 0;\
    z##high = 1;\
  }


   


#define get_low(name) name##low
#define get_high(name) name##high


#define test_interval(desc,name,exp_low,exp_high)           \
  if(name##low != exp_low || name##high != exp_high){   \
    printf("ERROR! Incorrect value for test '%s' Current: [%ld,%ld] " \
           "Expected: [%ld,%ld]\n"\
           "       Line %d, File: %s\n", desc, name##low, name##high,  \
           (long)exp_low, (long)exp_high,  __LINE__, __FILE__);               \
    exit(1);\
  }\
  else\
    printf("Test '%s' is OK.\n", desc);

void ptver_startup_check(){
  if(sizeof(int) != 4){
    printf("Integers are not 64 bits.\n");
    exit(1);
  }
}




#endif

