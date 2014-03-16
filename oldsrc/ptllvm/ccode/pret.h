
#ifndef __PRET_H__
#define __PRET_H__

extern long get_time() asm("llvm.pret_gt");
extern void mtfd_begin(long) asm("llvm.pret_mt");
extern void mtfd_end() asm("llvm.pret_fd");
extern void delay_until(long) asm("llvm.pret_du");




#endif
