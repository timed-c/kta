# Design Overview
by David Broman, 2017


### Test Suite
From the root path, the regression test suite is executed by the command

	>>make test
	


## WCET of the factorial demo

**Note: This text describes how to perform WCET analysis using abstract search manually. This is not the optimal way for an end user, but is a good way to learn the internals of the analysis tool.** 

Before you follow this tutorial, please go through the [getting started guide](gettingstarted.md).

Go to directory `/runtime`, which contains all OCaml files that will be compiled together with the generated analysis file. 

The first step is to generate the continuation passing style OCaml assembly code. We compile the file `test/demo/fact.c` using KTA, but write the output file to the runtime directory. 

The KTH command that we will use is called `wet`. By executing command

	>>kta wcet ../test/demo/fact.c fact -compile -optimize -cpsocaml > tmp.ml
	
we generate a file `tmp.ml` that contains the continuation passing style code for performing abstract execution of the factorial function. 

Please take a look at `tmp.ml`. Compare the generated code with the MIPS code that you get using the `disasm` command.

If we now want to perform WCET analysis on the factorial function, we need to compile and execute `tmp.ml`. We can do this using the following command

	>>ocamlbuild tmp.native -lib Str -- a0=[5,5]

The output is as follows

	Counter: 0
	BCET:  25 cycles
	WCET:  25 cycles
	at = Any          v0 = 120          v1 = 1            a0 = 1            
	a1 = Any          a2 = Any          a3 = Any          t0 = Any          
	t1 = Any          t2 = Any          t3 = Any          t4 = Any          
	t5 = Any          t6 = Any          t7 = Any          s0 = Any          
	s1 = Any          s2 = Any          s3 = Any          s4 = Any          
	s5 = Any          s6 = Any          s7 = Any          t8 = Any          
	t9 = Any          k0 = Any          k1 = Any          gp = Any          
	sp = 2147483640   fp = Any          ra = Any  
 
 Note that the analysis was executed with the argument interval `[5,5]`, which means that it was executed with the concrete value 5. Hence, it is correct that the output value for the factorial of 5 is 120. Note also that the best-case execution time (BCET) and the worst-case execution time (WCET) are both 25 cycles.
 
 If we instead execute the program as follows
 
	>>ocamlbuild tmp.native -lib Str -- a0=[3,5]
 
 it means that the input value is an abstract interval value, between values 3 and 5. 
 
 The output is then
 
	Counter: 0
	BCET:  15 cycles
	WCET:  25 cycles
	at = Any          v0 = [6,120]      v1 = 1            a0 = 1            
	a1 = Any          a2 = Any          a3 = Any          t0 = Any          
	t1 = Any          t2 = Any          t3 = Any          t4 = Any          
	t5 = Any          t6 = Any          t7 = Any          s0 = Any          
	s1 = Any          s2 = Any          s3 = Any          s4 = Any          
	s5 = Any          s6 = Any          s7 = Any          t8 = Any          
	t9 = Any          k0 = Any          k1 = Any          gp = Any          
	sp = 2147483640   fp = Any          ra = Any        
 
 Note now that the WCET is 25 cycles, but the BCET is 15 cycles. Note also that we got an abstract interval value for the return value. Depending on the input value (3, 4, or 5), we will get an output value between 6 and 120.