
//#define PRINT_MAIN

#ifdef PRINT_MAIN
#include <stdio.h>
#endif

int k;
int v = 7000;
int v2 = 8000;
int p[] = {3,4,5,6,7,8,9,10,2,9,3,4,5,6,7,8,9,10,2,9,3,4,5,6,7,8,9,10,2,9};

char str[] = "\xf0Hello my name is David.";
int res[20];


//--------------------------------------------
// Tests instructions j and beql
void sort(int *a, int len){
  int i;                     
  int swapped = 1;           
  while(swapped){            
    swapped = 0;             
    for(i=0; i<len-1; i++){  
      if(a[i+1] < a[i]){     
        int tmp = a[i+1];    
        a[i+1] = a[i];       
        a[i] = tmp;
        swapped = 1;        
      }          
    } 
  }
}
int foo(int x){

  int t1[] = {3,5,7,1,9,10,2};
  int len = sizeof(t1)/sizeof(int);
  sort(t1,len);

  
  #ifdef PRINT_MAIN
  int i;
  for(int i=0; i<len; i++){
    printf("%d,", t1[i]);
  }
  printf("\n");
  #endif

  int i;
  for(i=0; i<len;i++)
    res[i] = t1[i];
  return (int)&res;
}
//-----------------------------------------------


/*
int foo(int x){
  int a[] = {1,3,6,7};
  int i;
  for(i=0; i<4;i++)
    res[i] = a[i];
  return (int)&res;
}
*/

/*
// Test init and use of stack pointer. --------
void boo(int *v, int n){
  int i;
  for(i=0; i<n; i++)
    v[i] += i*p[i]; 
}

int foo(int x){
  int a[30];
  int i;
  for(i=0;i<30;i++)
    a[i] = p[i];
  int sum = 0;
  boo(a,8);
  boo(a+4,8);
  sum += a[x];
  sum += a[x+7];
  return sum;
}
//-----------
*/






/*
// Tests: lb sb
int foo(int x){
  int i;
  int k;
  while(str[i] != 0){
    str[i] = str[i] + x;
    i++;
  }
  return &str;
}
*/

/*
// Tests: lw, sw
int foo(int x, int y){
  v = -10000 + v;
  return &v;
}
*/

/*
// Tests: slt,bne,sll,jr,mul,addiu
int foo(int x, int y){
  int z = x * y;
  if(x > y)
    return x;
  else
    return z;
}
*/

/*
// Tests: blez, subu, sll, addiu, bne
int foo(int x){
  int i;
  int r = 23;
  for(i=0; i < x; i++){
    r += (i+100) * 7;
  }  
  return r;
}
*/


/*
// mult, mfhi, teq, div (reminder part)
int foo(int x){
  int i;
  int r = 23;
  for(i=0; i < x; i++){
    r += (i+100) % 7;
  }  
  return r;
}
*/


/*
// mult mflo
int foo(int x, int y){
  int z = x * y;
  if(x == y)
    return x;
  else
    return z;
}
*/

/*
 0000 0000 1000 0101   0001 0000 0000 1010

 */

int main()
{
  #ifdef PRINT_MAIN
  int x = foo(2);
  printf("Result: 0x%x  %d\n", x, x);
  #endif
  return 0;
}
