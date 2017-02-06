/* 
   KTA is licensed under the MIT license.  
   See file LICENSE.txt
*/

unsigned int a[128][128];
unsigned int b[128][128];
unsigned int c[128][128];
/*
Array multiplication a[N][K]*b[K][N]
*/
unsigned int array_mul(unsigned int N, unsigned int K, unsigned int M){
  int i, j, k;
  for(i=0; i<N; i++) 
	for(j=0; j<M; j++) {
		c[i][j] = 0;
		for(k=0; k<K; k++) 
			c[i][j] += a[i][k] * b[k][j];	
	}
  return 0;
}

