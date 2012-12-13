TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=ffi


TARGET2=ffi
TARGET3=gen_code

SRC= meta_ast_php_tof.ml \
     meta_ast_php_ofv.ml \
     gen_ml_code.ml \
     test_meta_php.ml \

# now in parsing_php/:     meta_ast_php_vof.ml

LIBS=$(TOP)/commons/lib.cma \
  $(TOP)/h_program-lang/lib.cma \
  ../parsing/lib.cma

SYSLIBS=nums.cma bigarray.cma str.cma unix.cma

INCLUDEDIRS=$(TOP)/commons $(TOP)/commons/ocamlextra \
  $(TOP)/h_program-lang \
  ../parsing \
  ../analyze

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma $(TARGET2) $(TARGET3)
all.opt:: $(TARGET).cmxa


$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $@ $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $@ $(OPTOBJS)



$(TARGET2): $(LIBS) $(EXTRALIBS) $(OBJS) main_ffi.cmo 
	$(OCAMLC) -custom -o $@ $(SYSLIBS) $^ 

$(TARGET3): $(LIBS) $(EXTRALIBS) $(OBJS) main_$(TARGET3).cmo
	$(OCAMLC) -custom -o $@ $(SYSLIBS) $^ 

clean:: 
	rm -f $(TARGET2) $(TARGET3)



##############################################################################
# Generic rules
##############################################################################
##############################################################################
# Literate Programming rules
##############################################################################
