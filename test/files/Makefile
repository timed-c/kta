

mac:
	clang -S -O1 -m32 -emit-llvm doubleif.c -o doubleif.ll
	./ptc parse-test.ta


clean:
	rm -f *.ll *.out
