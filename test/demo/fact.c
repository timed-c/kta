

unsigned int fact(unsigned int n){
  int r = 1;
  while(n > 1){
    r = r * n;
    n--;
  }
  return r;
}

