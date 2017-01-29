/* 
   KTA is licensed under the MIT license.  
   See file LICENSE.txt
*/
/* Should work for n>=1 */
unsigned int fact(unsigned int n){
  if (n == 1) 
	return 1;

  return n * fact(n-1);
}

