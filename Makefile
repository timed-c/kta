



.PHONY: all clean test 

all:
	(cd src/utools; make)
	(cd src/ptllvm; make)
	(cd src/riscv; make)

clean:	
	(cd src/utools; make clean)
	(cd src/ptllvm; make clean)
	(cd src/riscv; make clean)

test:	all
	(cd src/utools; make test) 
	(cd src/ptllvm; make test)
	(cd src/riscv; make test) 


