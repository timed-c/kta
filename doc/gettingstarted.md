
# Getting Started
by David Broman, 2017
***

The following short tutorial shows how to perform timing analysis on a simple C program. The tutorial assumes that all commands are executed in an terminal that shows `[MCB32]` before the command prompt, and that the `kta` command is compiled and accessible. If not, please follow the installation instructions [available here](../README.md).

## Display KTA commands

You can show the list of available KTA commands by simply typing 

	>>kta
	
To get help for a command, write `kta help` followed by the command. For instance,

	>>kta help exec
	
will display all options that can be used together with the command `exec`, which performs concrete execution of a function.


## Compiling and disassembling a program
In this tutorial, we will use a simple demo C program `fact.c` that is available in the directory `/test/demo`. The file contains a function `fact` that computes the factorial numbers using an imperative while loop. 

	unsigned int fact(unsigned int n){
	  int r = 1;
	  while(n > 1){
	    r = r * n;
	    n--;
	  }
	  return r;
	}	

We can use `kta` to compile and disassemble the program using the command `disasm`. To get the help for the command, write 

	>>kta help disasm
	
which displays


	Usage: kta disasm [<files>] [<options>]

	Description:
	  	Disassembles the .text section and pretty prints it to stdio.

	Options: 
	  -addr      Output the address of each instruction. 
	  -compile   Assume that the input files are C files. 
	  			 These files are then compiled and used as input 
	  			 instead of a binary ELF executable. 
	  -optimize  Compile the C program with optimization enabled. 
	  -verbose   Print out extra information, such as invocation of
	  	 		 external compilers. 

If we run

	>>kta disasm fact.c -compile 
	
the tool outputs the MIPS assembler code (try it!). The flag `-compile` assumes that the input file is a C program and automatically compiles it into MIPS machine code.

The assembler code is compiled without optimisations by default. Add compiler flag `-optimize` to enable optimisation. Also, by adding flag `-addr`, the address of each instruction is also displayed.

	>>kta disasm fact.c -compile -optimize -addr
	
which outputs
	
	0x00400018 fact:
	             sltiu   $v0,$a0,2
	0x0040001c   bne     $v0,$0,l1
	0x00400020   addiu   $v1,$0,1
	0x00400024   addiu   $v0,$0,1
	0x00400028   mult    $a0,$v0
	0x0040002c l2:
	             addiu   $a0,$a0,-1
	0x00400030   mflo    $v0
	0x00400034   sll     $0,$0,0
	0x00400038   bne     $a0,$v1,l2
	0x0040003c   mult    $a0,$v0
	0x00400040   jr      $ra
	0x00400044   sll     $0,$0,0
	0x00400048 l1:
	             jr      $ra
	0x0040004c   addiu   $v0,$0,1
	
If you add flag `-verbose`, you will see all the compilation commands that are performed.

## Executing a function

The C program in the previous section was first compiled from C to a MIPS binary file. The MIPS binary (ELF file) was then disassembled and printed to the standard output. It is also possible to execute a function. The KTA tool then compiles the C files, parses the MIPS binary, and then interprets the machine code. To execute a function, use command `exec`. As usual, run `>>kta help exec` to get the available options.

To execute the factorial function, we can use the following command

	kta exec fact.c -compile -func fact -args 5

The command states that file `fact.c` should be used. Flag `-compile` indicates that the C program should be compiled. Option `-func` is followed by the name of the function that should be executed, in this case function `fact`. Option `-args` is followed by a space separated list of arguments to the function. The arguments can only be integer values. Hence, in this case, we try to compute the factorial of 5, which should be `5! = 120`.  

If we execute the above command, it outputs:

	Execution of function 'fact' successfully terminated after
	executing 66 clock cycles.

Hence, we know the execution time, but we have no information about the result of the execution. 

By adding option `-regend`, KTA displays the MIPS register content after finishing the execution. We then get the output

	PC  0x00000000 
	$0  0x00000000      $t0 0x00000000      $s0 0x00000000      $t8 0x00000000      
	$at 0x00000000      $t1 0x00000000      $s1 0x00000000      $t9 0x00000000      
	$v0 0x00000078      $t2 0x00000000      $s2 0x00000000      $k0 0x00000000      
	$v1 0x0000003c      $t3 0x00000000      $s3 0x00000000      $k1 0x00000000      
	$a0 0x00000005      $t4 0x00000000      $s4 0x00000000      $gp 0x00409070      
	$a1 0x00000000      $t5 0x00000000      $s5 0x00000000      $sp 0x7ffffff8      
	$a2 0x00000000      $t6 0x00000000      $s6 0x00000000      $fp 0x00000000      
	$a3 0x00000000      $t7 0x00000000      $s7 0x00000000      $ra 0x00000000      

	Execution of function 'fact' successfully terminated after executing 66 clock cycles.

Note that the MIPS return register `$v0` has the hexadecimal number `0x00000078` which is the same as the decimal number `120`. 






