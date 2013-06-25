



.PHONY: all clean test 

all:
	(cd src/utools; make)
	(cd src/extlib; make)
	(cd src/target; make)
	(cd src/ptllvm; make)

clean:	
	(cd src/extlib; make clean)
	(cd src/utools; make clean)
	(cd src/target; make clean)
	(cd src/ptllvm; make clean)

test:	
	(cd src/utools; make test)
	(cd src/target; make test)
	(cd src/ptllvm; make test)


