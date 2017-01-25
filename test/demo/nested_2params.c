/* 
   KTA is licensed under the MIT license.  
   See file LICENSE.txt
*/
unsigned int nested_loop(unsigned int n1, unsigned int n2){
  int i, j, res;
  res = 1;
  for(i=1; i<n1; i++) 
	for(j=1; j<n2; j++) 
		res += i*j;  	
}

