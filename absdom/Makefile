


all:
	clang test_absint_interval.c -o test_absint_interval
	clang loop1.c -o loop1
	clang -S -O1 -emit-llvm loop1.c -o loop1.ll
	clang loop1_abs.c -o loop1_abs
	clang loop1_llvm.ll -o loop1_llvm
	clang loop2.c -o loop2
	clang -S -O1 -emit-llvm loop2.c -o loop2.ll
	clang loop2_llvm.ll -o loop2_llvm

test:	all
	test_absint_interval

clean:
	rm -f test_absint_interval 
	rm -f loop1 loop1.ll loop1_llvm loop1_abs 
	rm -f loop2 loop2.ll loop2_llvm
