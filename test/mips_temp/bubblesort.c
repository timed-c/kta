



void sort(int* d, int s){
  int t,i,j;
  for(i = 0; i < s-1; i++){
    for(j = i+1; j < s; j++){
      if(d[i] > d[j]){
        t = d[i];
        d[i] = d[j];
        d[j] = t; 
      }
    }
  }
}


int main(){
  int p[] = {12, 8, 2, 58, 3, 99, 32}; 
  int len = sizeof(p)/sizeof(int);
  sort(p,len);
  return 0;
}

