
#include <stdio.h>

char* str = "Test ";

int main(){
  int j = 2;
  int i;
  for(i=1; i< 10; i++){
    j = j+i;
    printf("%s %d\n", str, j);
  }
  return j;
}
