# Design Overview
by David Broman, 2017


### Test Suite
From the root path, the regression test suite is execued by the command

	>>make test
	


## WCET of the factorial demo

** Note: This text describes how to performe WCET analysis using anstract search manually. This is not the optimal way for an end user, but is a good way to learn the internals of the analysis tool.** 

Before you follow this tutoral, please go through the [getting started guide](gettingstarted.md).

Go to directory `test/demo`. The command that we will use is called `wcet`. By executing command

	>>kta wcet fact.c fact -compile -optimize -cpsocaml > tmp.ml
	
we generate a file `tmp.ml` that contains the continuation passing style code for performing abstract execution of the factorial function. 