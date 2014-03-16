
DIRS = src,test

.PHONY: all test clean

all:    ext libs 
	ocamlbuild -Is $(DIRS) mlvm.cma
	ocamlbuild -Is $(DIRS) mlvm.cmxa
	@cp _build/src/mlvm.cma libs/.
	@cp _build/src/mlvm.cmxa libs/.
	@echo "--------------------------------------"	
	@echo "Finished building mlvm."	
	@echo "The new libraries are availble in 'libs/'."
	@echo "Generated API documenation is available in 'doc/api'."

libs:	
	mkdir libs


# If ucamlib content does not exist, init and update submodules
ext:
	@mkdir ext
	@mkdir ext/ucamlib
	git submodule init
	git submodule update
	cd ext/ucamlib; git checkout master

test:
	ocamlbuild -Is $(DIRS) maintest.byte --
	@rm -f maintest.byte

apidoc:
	ocamlbuild -Is $(DIRS) doc/mlvm.docdir/index.html
	@mv mlvm.docdir api; mv api doc/.

# Update git sub modules
update:
	@cd ext/ucamlib; git checkout master; git pull

clean:
	ocamlbuild -clean
	@rm -rf libs
	@rm -rf doc/api
	@echo ""
	@echo "Finished cleaning project."







