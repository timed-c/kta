
LIB_NAME = target
FILES = riscvBin riscvAsm riscvSim riscvInstSelect
TEST_FILES = testBinEncodeDecode testBinDecodePrint
ONLY_MLI_FILES = riscvISA riscvSSA 
INCLUDE_DIR = ../utools/
DEP_LIBS = ../utools/utools.cmxa
UNITTEST_DIR = utest

