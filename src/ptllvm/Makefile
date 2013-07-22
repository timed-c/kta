

all: 
	(cd utest/testcode; make)
# (cd unittest/mdhwcet; make)
	make -f local.mk

clean:
	(cd utest/testcode; make clean)
# (cd unittest/mdhwcet; make clean)
	make -f local.mk clean

test:
	(cd utest/testcode; make)
# (cd unittest/mdhwcet; make)
	make -f local.mk test


