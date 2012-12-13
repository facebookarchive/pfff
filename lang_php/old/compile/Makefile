TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= \
  runtime_php.ml \
  lib_builtins_php.ml \
  interpret_php.ml \
  compile_php.ml \


LIBS=../commons/lib.cma \
     ../parsing/lib.cma \
     ../mini/lib.cma \

INCLUDEDIRS= $(TOP)/commons \
   ../mini \
   ../parsing \


TARGET1=phpcc

PROGS=$(TARGET1)

SYSLIBS=nums.cma bigarray.cma unix.cma str.cma graphics.cma

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma $(PROGS)
all.opt:: $(TARGET).cmxa  $(PROGS:=.opt)
opt: all.opt

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $@ $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $@ $(OPTOBJS)


$(TARGET1): $(LIBS) $(EXTRALIBS) $(OBJS) main_compile_php.cmo 
	$(OCAMLC) -custom -o $@ $(SYSLIBS) $^ 

$(TARGET1).opt: $(LIBS:.cma=.cmxa) $(EXTRALIBS:.cma=.cmxa) $(OBJS:.cmo=.cmx) main_compile_php.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBSOPT) $(SYSLIBS:.cma=.cmxa) $^ 

clean::
	rm -f $(PROGS)
