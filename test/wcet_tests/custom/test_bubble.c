
int array[64] = {10,1,2,3,4,5,6,7,
		 10,1,2,3,4,6,7,8,
		 1,2,3,4,5,6,7,8,
		 1,2,3,4,5,6,7,8,
		 10,1,2,3,4,5,6,7,
		 10,1,2,3,4,6,7,8,
		 1,2,3,4,5,6,7,8,
		 1,2,3,4,5,6,7,8};


volatile int k = 0;
volatile int l = 0;
volatile int l2 = 0;
volatile int ll = 0;



int swap(int i, int j) {

  int temp = array[i];
  array[i] = array[j];
  array[j] = temp;
}

int test_1 (int N) {

  int i,j;
  for (i=0; i<N; i++)
    for (j=0; j<N; j++) {
      if (array[i] < array[j])
	swap(i,j);
    }
  return array[1];
}


int test_2 (int N) {

  int i,j;
  for (i=0; i<N; i++)
    for (j=i+1; j<N; j++) {
      if (array[i] < array[j])
	swap(i,j);
    }
  return array[1];
}

// read array; write to k
int test_3 (int N) {

  int i,j;
  for (i=0; i<N; i++)
    for (j=i+1; j<N; j++) {
      if (array[i] < array[j])
	k = array[i];
    }
  return k;
}

// read array; read k
int test_4 (int N) {

  int i,j;
  for (i=0; i<N; i++)
    for (j=i+1; j<N; j++) {
      if (array[i] < array[j])
	l = k;
    }
  return l;
}


// read array; read k
int test_5 (int N) {
  int l2;
  int i,j;
  for (i=0; i<N; i++)
    for (j=i+1; j<N; j++) {
      if (array[i] < array[j])
	ll = k;
    }
  return ll;
}
