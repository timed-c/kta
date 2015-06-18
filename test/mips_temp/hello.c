


int v;
int k = 100;

int foo1(int x){
  return x + 1;
}

int foo2(int x){
  return x + k + v;
}

int foo3(int x, int y, int z){
  return foo1(x) * y * z;
}

int main(){
  k = 5;
  v = 3;
  return foo2(4);
}


