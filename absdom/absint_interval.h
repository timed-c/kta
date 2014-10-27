

#include <stdio.h>
#include <stdlib.h>

#ifndef _ABS_INTERVAL_DOM
#define _ABS_INTERVAL_DOM

#define intbits ((sizeof(int))*8)

#define mask(w) ((w) == sizeof(int)*8 ? 0xffffffff : (1 << (w)) - 1)
#define minval(w) 0
#define maxval(w) (-1 & mask(w))
#define aint32(name) unsigned int name##low, name##high

#define sign_extend(x,w) ((((int)(x)) << (intbits - (w))) >> (intbits - (w)))

/* Set a variable to an interval. w is the word bith length  */
#define set_interval(name,l,h,w) \
  name##low = l & mask(w); \
  name##high = h & mask(w)

/* Adds x and y and stores into z. Assumes that 
   all abstract values have w bit width */
#define add(z,x,y,w) \
  z##high = (x##high + y##high) & mask(w);\
  z##low = (x##low + y##low) & mask(w);\
  if(x##low > x##high && z##high < x##high ||\
     y##low > y##high && z##high < y##high ||\
     x##low > x##high && y##low > y##high)\
  {\
     z##low = minval(w);                         \
     z##high = maxval(w);\
  }  

/* Multiplies x and y and stores it in z. 
   Must check how good this works for negative numbers. 
   This abstracted instruction can be improved.*/
#define mul(z,x,y,w)\
  if(!(x##low == x##high && y##low == y##high) &&\
    (x##high > maxval(w/2) || y##high > maxval(w/2) || \
     x##low > x##high || y##low > y##high)) \
  {\
     z##low = minval(w);\
     z##high = maxval(w);\
  }\
  else\
  {\
    z##high = (x##high * y##high) & mask(w);\
    z##low = (x##low * y##low) & mask(w);\
  }


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
    printf("ERROR! Incorrect value for test '%s' Current: [%u,%u] " \
           "Expected: [%u,%u]\n"\
           "       Line %d, File: %s\n", desc, name##low, name##high,    \
           exp_low, exp_high,  __LINE__, __FILE__); \
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

