


include files.mk

F_LL = $(addsuffix .ll, $(FILES))
F_C = $(addsuffix .c, $(FILES))


all: $(F_LL)

$(F_LL): $(F_C)
	clang -S -O1 -m32 -emit-llvm $(subst .ll,.c,$@) -o $@
	llvm-as $@ -o $(subst .ll,.bc,$@)

.PHONY: clean

clean:
	rm -f *.ll *.bc
