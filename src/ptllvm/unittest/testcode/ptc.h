


/* Macros defining Precistion Timed LLVM instructions */

/*
  Parameters
  1. Precision 9 means nano seconds, and 1 means seconds
  2. Delay
  3. Release time
  4. Upper bound
  5. Lower bound
 */
extern float begin_task(long,long,long,long,long) asm("llvm.begin_task");
extern float begin_hardtask(long,long,long,long,long) asm("llvm.begin_hardtask");
extern float end_task() asm("llvm.end_task");

extern void begin_periodic_task(int,int,int) asm("llvm.begin_periodic_task");
extern void end_periodic_task() asm("llvm.end_periodic_task");


// We would like to make a difference between deadline and period. We want perhaps

// MTFD should perhaps have an identifier.





