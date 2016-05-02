
//#include <stdio.h>

unsigned exper(unsigned start, unsigned end){
  unsigned retval = 0;
  do{
    retval += start;
    start--;    
  }
  while(start != end);
  return retval;
}


int main(){
  unsigned v = exper(100,50);  //expected output: 3775
  //  printf("%d\n",v);
}
