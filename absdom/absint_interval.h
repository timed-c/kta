

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
   - if x = TOP or y = TOP then z := TOP 
     else
       let th = x.high + y.high and tl = x.low + y.low
        if th > maxval or tl < minval then z := TOP
        else z.high := th and z.low := tl
   - Note that we do not need to compare th < minval because 
        if th < minval, then tl < minval.

  MERGE: z = merge(x,y)
   - if x = TOP or y = TOP then z := TOP 
     else 
       z.high := max(x.high,y.high)
       z.low := min(x.low,y.low)
       

  ICMP SLT MTC: z,tx,ty = icmp_slt(x,y)   (signed less than)
   - MT stands for "minimize true case". That is, if this results in both a
     true and false case, this function tries to minize the abstact value for the
     true branch. We want to minimize the in-loop edges.
   - z is 0 if false, 1 if true, and 2 if true and false
   - in the case if z = 2, then
         tx and ty are the values for x and y, respectively, if z is true, 
   - if x = TOP or y = TOP then z := TOP else
     if x.high < y.low then z := 1 else  // only true (1)
     if x.low >= y.high then z := 0      // only false  (2)
     else                               
       z := 2                            // both true and false
       ty.high = y.high                  // fact from above
       tx.low = x.low                    // fact from above
    
       if y.low <= x.low                 // can we make ty.low higher
         ty.low := x.low + 1             // case:
       else                              //        xxxxxxxx
         ty.low := y.low                 //   yyyyyyy

       if x.high >= y.high               // can we make tx.high lower
         tx.high = y.high - 1            // case:
       else                              //       xxxxxxxxxx
         tx.high = x.high                //    yyyyyyy
         
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
#define merge(z,x,y,w)                        \
  if(is_top(x) || is_top(y))\
    {z##high = -777; z##low = 777;}  \
  else{\
     z##high = (x##high > y##high) ? x##high : y##high;  \
     z##low = (x##low < y##low) ? x##low : y##low;         \
    }

 
#define move(z,x)                              \
  z##high = x##high;\
  z##low = x##low


#define icmp_slt_mtc(z,tx,ty,x,y,w) \
  if(is_top(x) || is_top(y))\
    {z = 2;\
     tx##high = -777; tx##low = 777;\
     tx##high = -777; ty##low = 777;}\
  else{\
    if(x##high < y##low)\
      z = 1;\
    else{\
      if(x##low >= y##high)\
        z = 0;\
      else{\
        z = 2;\
        ty##high = y##high;\
        tx##low = x##low;\
        ty##low = (y##low <= x##low) ? x##low + 1 : y##low;\
        tx##high = (x##high >= y##high) ? y##high - 1 : x##high;\
      }\
    }\
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

#define test_case(desc,var,val)\
  if(var != val) \
    printf("ERROR. Incorrect result for test '%s'. Current: %d Expected: %d\n",\
           desc, var, val);\
  else\
    printf("Test '%s' is OK.\n", desc)
     


void ptver_startup_check(){
  if(sizeof(int) != 4){
    printf("Integers are not 64 bits.\n");
    exit(1);
  }
}




#endif

