
LIB_NAME = target
FILES = riscvBin riscvAsm riscvSSA riscvSim
TEST_FILES = testBinEncodeDecode testBinDecodePrint
ONLY_MLI_FILES = riscvISA
INCLUDE_DIR = ../utools/
DEP_LIBS = ../utools/utools.cmxa
UNITTEST_DIR = utest
