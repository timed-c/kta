

#include <stdio.h>


typedef struct{
  int  age;
  int  sex;
  char name[20];
  int  numbers[10];
} person;

void update_person(person* p)
{
  p->age = 35;
  p->numbers[7] = 77;
}

int simple_struct_access(int k)
{
  person p;
  update_person(&p);
  return p.numbers[k];                         
}


int main()
{
  printf("simple_struct_access(7) = %d\n", simple_struct_access(7)); 
  return 0;
}
