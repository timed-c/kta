


//#include <stdio.h>

int bintext2int(char* text){

  //Count the number of characters
  int n=0;
  while(text[n] != 0)
    n++;

  //Compute the integer value
  int k = 0;
  int res = 0;
  int i;
  for(i=n-1; i >= 0; i--){
    res += (text[i] - '0') << k;
    k++;
  }
  
  return res;
}

int main(){
  //  printf("%d,%d\n", (int)'0', (int)'1');
  //  printf("%d\n", bintext2int("111"));
  //printf("%d\n", bintext2int("01101"));
  return 0;
}

