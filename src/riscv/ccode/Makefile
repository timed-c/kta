


include files.mk

F_LL = $(addsuffix .ll, $(FILES))
F_GCC_BIN = $(addsuffix .riscv-gcc.bin, $(FILES))
F_C = $(addsuffix .c, $(FILES))


all: $(F_LL)

$(F_LL): $(F_C)
	clang -S -O1 -m32 -emit-llvm $(subst .ll,.c,$@) -o $@
	llvm-as $@ -o $(subst .ll,.bc,$@)

riscv-gcc: $(F_GCC_BIN)

$(F_GCC_BIN): $(F_C)
	riscv-gcc -O3 $(subst .riscv-gcc.bin,.c,$@) -o $@
	riscv-objdump -D $@ > $(subst .riscv-gcc.bin,.riscv-gcc.asm,$@)


.PHONY: clean

clean:
	rm -f *.ll *.bc *.riscv-gcc.asm *.riscv-gcc.bin
