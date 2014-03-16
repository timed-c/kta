

DIRS = src,ext/ucamlib/src

.PHONY: all clean

# Init submodules if needed and make native version. 
# The resulting executable can be found under /bin and /library (symlinks)
all:    ext/ucamlib/Makefile native 


# Compile native version
native: bin 
	@ocamlbuild -Is $(DIRS) ptc.native 
	@mv -f ptc.native bin/ptc

# Compile byte code version
byte: 	bin 
	@ocamlbuild -Is $(DIRS) ptc.byte	
	@mv -f ptc.byte bin/ptc


# If ucamlib content does not exist, init and update submodules
ext/ucamlib/Makefile:
	@git submodule init
	@git submodule update
	@cd ext/ucamlib; git checkout master

# Handling subtree for ext/mlvm
MLVM_GIT = /Users/broman/Dropbox/ptcrepo/mlvm.git
add_mlvm:
	git subtree add --prefix ext/mlvm $(MLVM_GIT) master --squash
pull_mlvm:
	git subtree pull --prefix ext/mlvm $(MLVM_GIT) master --squash
push_mlvm:
	git subtree push --prefix ext/mlvm $(MLVM_GIT) master --squash




bin:	
	@mkdir bin


# Generate all documentation
gendoc: doc/user/manual.html
	@ocamlbuild -Is $(DIRS) doc/main.docdir/index.html
	@rm -f main.docdir 
	@cd doc; rm -f api; ln -s ../_build/doc/main.docdir api

# Generate doc for the userguide
doc/user/manual.html: doc/user/manual.txt
	@cd doc/user/; asciidoc manual.txt



# Clean all submodules and the main Modelyze source
clean:
	@ocamlbuild -clean	
	@rm -rf bin
	@rm -rf doc/api
	@rm -f doc/userguide/*.html
	@echo " Finished cleaning up."

