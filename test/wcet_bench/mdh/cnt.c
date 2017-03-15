/* $Id: cnt.c,v 1.3 2005/04/04 11:34:58 csg Exp $ */

/* sumcntmatrix.c */

//#include <sys/types.h>
//#include <sys/times.h>

// #define WORSTCASE 1
// #define MAXSIZE 100 Changed JG/Ebbe
#define MAXSIZE 10

// Typedefs
typedef int matrix [MAXSIZE][MAXSIZE];

// Forwards declarations
int main(void);
int test(matrix);
int initialize(matrix);
int initSeed(void);
void sum(matrix);
int randomInteger(void);

// Globals
int Seed;
matrix Array;
int Postotal, Negtotal, Poscnt, Negcnt;

// The main function
int main (void)
{
   initSeed();
   //printf("\n   *** MATRIX SUM AND COUNT BENCHMARK TEST ***\n\n");
   //printf("RESULTS OF THE TEST:\n");
   test(Array);
   return 1;
}


int test(matrix Array)
{
#ifdef CALCTIME
   long StartTime, StopTime;
   float TotalTime;
#endif

   initialize(Array);

#ifdef CALCTIME
   StartTime = 1000.0; //ttime();
#endif
   sum(Array);
#ifdef CALCTIME
   StopTime = 1500.0; //ttime();

   TotalTime = (StopTime - StartTime) / 1000.0;
#endif

   //printf("    - Size of array is %d\n", MAXSIZE);
   //printf("    - Num pos was %d and sum was %d\n", Poscnt, Postotal);
   //printf("    - Num neg was %d and sum was %d\n", Negcnt, Negtotal);
   //printf("    - Num neg was %d\n", Negcnt);
   //printf("    - Total sum time is %3.3f seconds\n\n", TotalTime);
   return 0;
}


// Intializes the given array with random integers.
int initialize(matrix Array)
{
   register int OuterIndex, InnerIndex;

   for (OuterIndex = 0; OuterIndex < MAXSIZE; OuterIndex++) //100 + 1
      for (InnerIndex = 0; InnerIndex < MAXSIZE; InnerIndex++) //100 + 1
         Array[OuterIndex][InnerIndex] = randomInteger();

   return 0;
}


// initializes the seed used in the random number generator.
int initSeed (void)
{
   Seed = 0;
   return 0;
}

void sum(matrix Array)
{
  register int Outer, Inner;

  int Ptotal = 0; /* changed these to locals in order to drive worst case */
  int Ntotal = 0;
  int Pcnt = 0;
  int Ncnt = 0;

  for (Outer = 0; Outer < MAXSIZE; Outer++) //Maxsize = 100
    for (Inner = 0; Inner < MAXSIZE; Inner++)
#ifdef WORSTCASE
      if (Array[Outer][Inner] >= 0) {
#else
	if (Array[Outer][Inner] < 0) {
#endif
	  Ptotal += Array[Outer][Inner];
	  Pcnt++;
	}
	else {
	  Ntotal += Array[Outer][Inner];
	  Ncnt++;
	}

  Postotal = Ptotal;
  Poscnt = Pcnt;
  Negtotal = Ntotal;
  Negcnt = Ncnt;
}


// This function returns in milliseconds the amount of compiler time
//int ttime()
//{
//  struct tms buffer;
//int utime;

//times(&buffer);
//utime = (buffer.tms_utime / 60.0) * 1000.0;
//return (utime);
//}


// Generates random integers between 0 and 8095
int randomInteger(void)
{
   Seed = ((Seed * 133) + 81) % 8095;
   return Seed;
}





