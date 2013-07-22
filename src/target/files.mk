
LIB_NAME = target
FILES = riscvBin riscvAsm
TEST_FILES = testBinEncodeDecode testBinDecodePrint
ONLY_MLI_FILES = riscvISA
DIRS = ../utools/
DEP_LIBS = ../utools/utools.cmxa
UNITTEST = utest
