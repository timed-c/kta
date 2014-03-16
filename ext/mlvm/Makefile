
DIRS = src,test,ext/ucamlib/src,ext/extlib

.PHONY: all test clean

all:    libs 
	@ocamlbuild -Is $(DIRS) mlvm.cma
	@ocamlbuild -Is $(DIRS) mlvm.cmxa
	@cp _build/src/mlvm.cma libs/.
	@cp _build/src/mlvm.cmxa libs/.
	@echo "----------------------------------------------"	
	@echo "Finished building mlvm."	
	@echo "The new libraries are availble in 'libs/'."
	@echo "Generated API documenation is available in 'doc/api'."

libs:	
	@mkdir libs


add_subtrees:
	git subtree add --prefix ext/ucamlib https://github.com/david-broman/ucamlib.git master --squash


test:
	ocamlbuild -Is $(DIRS) maintest.byte --
	@rm -f maintest.byte

apidoc:
	ocamlbuild -Is $(DIRS) doc/mlvm.docdir/index.html
	@mv mlvm.docdir api; mv api doc/.



clean:
	@ocamlbuild -clean
	@rm -rf libs
	@rm -rf doc/api
	@echo ""
	@echo "Finished cleaning project."







