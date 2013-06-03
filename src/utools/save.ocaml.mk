

BUILD = _build
F_MLI = $(addsuffix .mli, $(FILES) $(ONLY_MLI_FILES))
F_ML =  $(addsuffix .ml, $(FILES))
F_CMI = $(addprefix $(BUILD)/, $(addsuffix .cmi, $(FILES) $(ONLY_MLI_FILES)))
F_CMX = $(addprefix $(BUILD)/, $(addsuffix .cmx, $(FILES)))
F_LIB = $(addprefix $(BUILD)/, $(addsuffix .cmxa, $(LIB_NAME)))

all: $(BUILD) $(F_LIB)

# Compile all .mli interfaces
$(F_CMI): $(F_MLI)
	ocamlopt -c -o $@ -I $(BUILD)/ $(subst .cmi,.mli,$(@F)) 

# Compile all .ml files
#$(F_CMX): $(F_ML) $(F_CMI)
#	ocamlopt -c -o $@ -I $(BUILD)/ $(subst .cmx,.ml,$(@F)) 

$(F_CMX): $(subst .cmx,.ml,$(@F)) $(F_CMI)
	ocamlopt -c -o $@ -I $(BUILD)/ $(subst .cmx,.ml,$(@F)) 

# Build library
$(F_LIB): $(F_CMX)
	ocamlopt -a -o $@ $(F_CMX)

$(BUILD):
	mkdir $(BUILD)

# Clean up 
.PHONY: clean
clean:
	rm -rf $(BUILD)
