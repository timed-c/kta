


all:
	clang -E utest/testabsdom.c -o testabsdom.pp
	clang utest/testabsdom.c -o testabsdom
	clang -S -O1 -emit-llvm utest/testabsdom.c -o testabsdom.ll

test:	all
	testabsdom

clean:
	rm -f testabsdom *.pp *.ll
