TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= patch.ml lib_vcs.ml  \
     git.ml  cvs.ml  mercurial.ml \
     version_control.ml \
     test_version_control.ml


LIBS=../commons/commons.cma
INCLUDEDIRS= ../commons ../commons/lib-sexp

TARGET1=vcs

PROGS=$(TARGET1)

SYSLIBS1=nums.cma bigarray.cma str.cma unix.cma 
SYSLIBS= $(SYSLIBS1)

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma $(PROGS)
all.opt:: $(TARGET).cmxa  $(PROGS:=.opt)

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $@ $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $@ $(OPTOBJS)


$(TARGET1): $(LIBS) $(EXTRALIBS) $(OBJS) main_vcs.cmo 
	$(OCAMLC) -custom -o $@ $(SYSLIBS) $^ 

$(TARGET1).opt: $(LIBS:.cma=.cmxa) $(EXTRALIBS:.cma=.cmxa) $(OBJS:.cmo=.cmx) main_vcs.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^ 

clean::
	rm -f $(PROGS)
