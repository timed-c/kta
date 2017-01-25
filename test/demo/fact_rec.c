/* 
   KTA is licensed under the MIT license.  
   See file LICENSE.txt
*/
unsigned int fact(unsigned int n){
  if (n == 1) 
	return 1;

  return n * fact(n-1);
}

