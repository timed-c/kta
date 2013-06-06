



F_MLI = $(addsuffix .mli, $(FILES) $(ONLY_MLI_FILES))
F_ML =  $(addsuffix .ml, $(FILES))
F_CMX = $(addsuffix .cmx, $(FILES))
F_LIB = $(addsuffix .cmxa, $(LIB_NAME))
INCL_DIRS = $(addprefix -I ,$(DIRS))
F_TEST = $(addprefix unittest/,$(addsuffix .ml, $(TEST_FILES)))

$(F_LIB): $(F_CMX)
	ocamlopt -a $(INCL_DIRS) -o $@ $(F_CMX) 

.SUFFIXES: .ml .mli .cmi .cmx

.mli.cmi:
	ocamlopt $(INCL_DIRS) -c $<

.ml.cmx:
	ocamlopt $(INCL_DIRS) -c $<


.PHONY: test
test: unittest/main $(F_LIB)
	unittest/main

unittest/main: $@ $(F_CMX) $(F_TEST)  
	ocamlopt -cc g++ -o $@ $(INCL_DIRS) $(DEP_LIBS) $(F_CMX) $(F_TEST)

# Clean up 
.PHONY: clean
CFILES = *.cmi *.cmx *.cmxa *.o *.a
clean:
	rm -rf $(CFILES) $(addprefix unittest/,$(CFILES)) .depend unittest/main

.depend: $(BUILD) $(F_MLI) $(F_ML)
	ocamldep -native $(F_MLI) $(F_ML) > .depend

include .depend
