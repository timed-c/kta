

DIRS = src,ext/ucamlib/src

.PHONY: all clean

# Init submodules if needed and make native version. 
# The resulting executable can be found under /bin and /library (symlinks)
all:    native 


# Compile native version
native: 
	@ocamlbuild -Is $(DIRS) ktc.native 
	@rm -f ktc.native
	@rm -rf bin; mkdir bin; cd bin; cp ../_build/src/ktc.native ktc 

# Compile byte code version
byte: 	
	@ocamlbuild -Is $(DIRS) ktc.byte	
	@rm -f ktc.byte
	@rm -rf bin; mkdir bin; cd bin; cp ../_build/src/ktc.byte ktc 

# Generate all documentation
gendoc: doc/user/manual.html
	@ocamlbuild -Is $(DIRS) doc/main.docdir/index.html
	@rm -f main.docdir 
	@cd doc; rm -f api; ln -s ../_build/doc/main.docdir api

# Generate doc for the userguide
doc/user/manual.html: doc/user/manual.txt
	@cd doc/user/; asciidoc manual.txt


# Handling subtree for ext/mlvm
MLVM_GIT = https://gitr.sys.kth.se/dbro/code-mlvm.git
MLVM_MSG = 'Updated mlvm'
add_mlvm:
	git subtree add --prefix ext/mlvm $(MLVM_GIT) master --squash
pull_mlvm:
	git subtree pull --prefix ext/mlvm $(MLVM_GIT) master --squash -m $(MLVM_MSG)
push_mlvm:
	git subtree push --prefix ext/mlvm $(MLVM_GIT) master --squash


# Handling subtree for ext/ucamlib
UCAMLIB_GIT = https://github.com/david-broman/ucamlib.git
UCAMLIB_MSG = 'Updated ucamlib'
add_ucamlib:
	git subtree add --prefix ext/ucamlib $(UCAMLIB_GIT) master --squash 
pull_ucamlib:
	git subtree pull --prefix ext/ucamlib $(UCAMLIB_GIT) master --squash -m $(UCAMLIB_MSG)
push_ucamlib:
	git subtree push --prefix ext/ucamlib $(UCAMLIB_GIT) master --squash



# Clean all submodules and the main Modelyze source
clean:
	@ocamlbuild -clean	
	@rm -rf bin
	@rm -rf doc/api
	@rm -f doc/userguide/*.html
	@echo " Finished cleaning up."


