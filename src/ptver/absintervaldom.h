

#include <stdio.h>
#include <stdlib.h>

#ifndef _ABS_INTERVAL_DOM
#define _ABS_INTERVAL_DOM


#define mask(w) ((w) == sizeof(int)*8 ? 0xffffffff : (1 << (w)) - 1)
#define aint32(name) unsigned int name##low, name##high

#define aint_set(name,l,h,w) \
  name##low = l & mask(w); \
  name##high = h & mask(w)

/* Adds x and y and stores into z. Assumes that 
   all abstract values have w bit width */
#define aint_add(z,x,y,w) \
  z##high = (x##high + y##high) & mask(w);\
  z##low = (x##low + y##low) & mask(w);\
  if(x##low > x##high && z##high < x##high ||\
     y##low > y##high && z##high < y##high ||\
     x##low > x##high && y##low > y##high)\
  {\
     z##low = 0;\
     z##high = -1 & mask(w);\
  }  
  
 
#define aint_get_low(name) name##low
#define aint_get_high(name) name##high



#define test_aint(name,exp_low,exp_high) \
  if(name##low != exp_low || name##high != exp_high){   \
    printf("ERROR! Incorrect interval. Current: [%u,%u] " \
           "Expected: [%u,%u]\n"\
           "       Line %d, File: %s\n", name##low, name##high, \
           exp_low, exp_high,  __LINE__, __FILE__); \
    exit(1);\
  } 

void ptver_startup_check(){
  if(sizeof(int) != 4){
    printf("Integers are not 64 bits.\n");
    exit(1);
  }
}




#endif

