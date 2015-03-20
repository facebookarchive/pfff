##############################################################################
# Variables
##############################################################################

SRC= json_type.ml \
     json_out.ml \
     netconversion2.ml \
     json_parser.ml \
     json_lexer.ml \
     json_in.ml \
     json_io.ml

TARGET=lib

INCLUDES=
#-I +camlp4
SYSLIBS= str.cma unix.cma bigarray.cma num.cma

##############################################################################
# Generic variables
##############################################################################

#dont use -custom, it makes the bytecode unportable.
OCAMLCFLAGS= -g -dtypes $(OCAMLCFLAGS_EXTRA)
#-for-pack Sexplib

# This flag is also used in subdirectories so don't change its name here.
OPTFLAGS=

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES)  $(SYSINCLUDES) -thread
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES) $(SYSINCLUDES) -thread
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES) -thread

#-ccopt -static
STATIC=

##############################################################################
# Top rules
##############################################################################

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

all: $(TARGET).cma
all.opt: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top

#pad: we include in the git repo already the generated file
#json_lexer.ml: json_lexer.mll
#	$(OCAMLLEX) $<
#dist_clean::
#	rm -f json_lexer.ml
#beforedepend:: json_lexer.ml

#json_parser.ml json_parser.mli: json_parser.mly
#	$(OCAMLYACC) $<
#dist_clean::
#	rm -f json_parser.ml json_parser.mli json_parser.output
#beforedepend:: json_parser.ml json_parser.mli

##############################################################################
# install
##############################################################################
LIBNAME=pfff-external-jsonwheel
EXPORTSRC=json_io.mli json_parser.mli json_type.mli

install-findlib:
	ocamlfind install $(LIBNAME) META   lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
	  $(SRC) json_in.cmi json_out.cmi

uninstall-findlib::
	ocamlfind remove $(LIBNAME)


##############################################################################
# Generic rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.mldepend:
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot *.cmt *.cmti
clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

distclean::
	rm -f .depend

-include .depend
