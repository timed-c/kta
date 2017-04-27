

//TO run it using GCC, execute
//>> gcc test.c -D PRINT && a.out

//To run directly using kta
//>> kta wcet test.c square -optimize 3 -args a0=5

//Generate cpscode
//>>kta wcet test.c square -optimize 0 -cpsocaml >> a.ml

//Run using the compiled program
//>>ocamlbuild -lib=str a.native -- -args a0=5

//Print out the disasm code
//>>kta disasm test.c -compile

#ifdef PRINT
#include <stdio.h>
#endif



int square(int x){
  return x*x;
}

int nested(int x, int y, int z){
  int ret = 0;
  int i,j;
  for(i=0; i<y; i++){
    for(j=0; j<x; j++){
      ret += x + y + z;
    }
  }
  return ret;
}



int main(){
  #ifdef PRINT
  printf("Square of %d is %d\n", 10, square(10));
  printf("Nested test 1: %d\n", nested(10,10,11));
  #endif
}
