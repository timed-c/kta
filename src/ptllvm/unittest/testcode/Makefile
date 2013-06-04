


# LLVM=~/bin/llvm32/bin/

LLVM=

# all:
#	$(LLVM)clang -S -emit-llvm -O3 intrinsics.c -o intrinsics.ll
#	$(LLVM)llvm-as intrinsics.ll -o intrinsics.bc
#	$(LLVM)llvm-dis intrinsics.bc -o intrinsics2.ll

all:

%:
	$(LLVM)clang -S -O3 -emit-llvm $@.c -o $@.ll
	$(LLVM)llvm-as $@.ll -o $@.bc
	$(LLVM)llvm-dis $@.bc -o $@2.ll


clean:
	rm -f *.ll *.bc
