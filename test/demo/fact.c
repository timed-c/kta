/* 
   KTA is licensed under the MIT license.  
   Copyright (C) David Broman. See file LICENSE.txt
*/

unsigned int fact(unsigned int n){
  int r = 1;
  while(n > 1){
    r = r * n;
    n--;
  }
  return r;
}

