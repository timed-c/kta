# Design Overview
by David Broman, 2017


### Test Suite
From the root path, the regression test suite is executed by the command

	>>make test
	
The regression test is not very developed and should include a large set of program tests.


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
 
 
# Code structure

The KTA code that generates the CPS (continuation-passing style) OCAML code is located in directory `src/`. The actual wet analysis takes place executing the CPS code, which is linked together with the runtime files located in directory `runtime/`. 


## Directory src/
To be updated
 
## Directory runtime/

### CPS OCaml file

The CPS OCaml Code file is structured as follows:

* A list of block identifier names and numbers. These basic block names correspond to the names of the basic block in the machine code. Compare with the disassembled code. The actual numbers are used for fast access into the array `bblocks`, which is defined at the end of the file.

* The actual program code. Each basic block is represented as a function (a co-routine) which is involved using continuation passing style. Note that each basic block contains function calls to functions that correspond to one assembly instruction. 

* The basic block table `bblocks` includes the following information:
 	1. `func`: the function defined above. This is used when calling another co-routine.
 	2. `name`: used for pretty printing.
 	3. `nextid`: identifier number to the next basic block, if no branch occurs. Note that the `next` call in the basic blocks is used to actually perform the jump. The distance number is computed by KTA by performing a graph search on the control flow graph. 
 	4. `list`: distance to the final basic block. This distance number is used when the runtime system should choose when to jump.
 	5. `addr`: start (code) address for the basic block.  
	6. `caller`: used for function calls.
	
* The `main` function starts the analysis process by calling function `analyse`, located in module `AbstractMIPS`.

Note that the CPS file is automatically generated by KTA.

### Module: AbstractMIPS 
 
This module contains the main functionality of the analysis runtime system. 

Some of the main data structures:


* `mstate`: Contains the main state. This structure is passed around in all function calls. 
	* `cblock`: Is the current block id that is being processed.
	* `pc`: current program counter
	* `pstate`: the current program state (containing registers, memory etc.)
	* `batch`: this is batch of program states that should be processed for the current block id. This is why the block id and the program counter is not part of the program state. All the processes in the batch (the list) are processed in turn, until it is empty. See function `dequeue`. 
	* `bbtable`: this is just a pointer to the bbtable that is define in the CPS file. 
	* `prio`: This is the main priority queue. When there are no more program states to process in a batch, the `dequeue` function finds the next batch of program states to process. 
	* `returnid`: used when returning from a function call. 
	* `cstack`: when a function call takes place, the whole queue of program states are pushed on the stack. Hence, it always finishes the analysis of the deepest function call first.
	* `branch`: special branch handling for `slt` and `beq`. This is the tricky thing that `slt` needs to cut abstract intervals. 
	
* `progstate`: Contains an abstract state of the registers, the memory, BCET, and WCET values.

Here are some of the essential functions:

* `analyze`: located at the end of the file and is the start of the analysis. It calls `analyze_main` which starts up the actual analysis.

* `analyze_main`: Initiates the main state and starts the process.

* `enqueue`: One of the central functions. When the analysis is finished with a basic block, it enqueues the basic block again into the priority queue (see for instance function `next`). The `enqueue` function inserts the basic block in the right place in the priority queue (which is for simplicity represented as a list). This is pretty fast, since often it will be placed on top of the list.

* `dequeue`: is called to get the next basic block to process. First, it checks if there is anything to do on the batch. If not, it checks if it has finished the analysis, or if it is returning from a function call. If not, it dequeues one program state. 

### Module: Aregsimple

Immutable representation of an abstract interval register. As it turns out, this is in general faster than using a lazy variant. 


### Module: Aint32relint
Contain the interval arithmetic functions. 


### Module: Amemory

Contains the abstract memory definitions. The `nomem` flag is used for testing. This abstract memory module is using immutable Maps and a lazy join, where elements are just jointed when they are read. 




 
 
 
 
 
 
 
 
 