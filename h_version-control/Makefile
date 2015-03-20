TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC= flag_version_control.ml patch.ml lib_vcs.ml  \
     version_control.ml \
     git.ml  cvs.ml  mercurial.ml \
     generic_vcs.ml \
     test_version_control.ml unit_version_control.ml

#todo: empty for now
SRC+=subversion.ml


LIBS=../commons/lib.cma ../globals/lib.cma
INCLUDEDIRS= ../commons ../commons/ocamlextra ../globals

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

##############################################################################
# install
##############################################################################
LIBNAME=pfff-h_version-control
EXPORTSRC=\
  cvs.mli \
  generic_vcs.mli \
  git.mli \
  lib_vcs.mli \
  mercurial.mli \
  patch.mli \
  subversion.mli \
  test_version_control.mli \
  unit_version_control.mli \
  version_control.mli \

install-findlib: all all.opt
	ocamlfind install $(LIBNAME) META \
          lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi)
