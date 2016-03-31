


unsigned exp1(unsigned start, unsigned end){
  unsigned retval = 0;
  do{
    retval += start;
    start--;    
  }
  while(start != end);
  return retval;
}


int main(){
  unsigned v = exp1(100,50);
}
