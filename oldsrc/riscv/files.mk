
LIB_NAME = target
FILES = riscvPPrint riscvBin riscvAsm riscvSim riscvInstSelect
TEST_FILES = testBinEncodeDecode testBinDecodePrint testInstSelect
ONLY_MLI_FILES = riscvISA riscvSSA 
INCLUDE_DIR = ../utools/ ../ptllvm/ /usr/local/llvm32/lib/ocaml/
DEP_LIBS = llvm.cmxa llvm_executionengine.cmxa llvm_bitreader.cmxa ../utools/utools.cmxa ../ptllvm/ptllvm.cmxa
UNITTEST_DIR = utest

