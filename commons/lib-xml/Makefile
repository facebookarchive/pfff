##############################################################################
# Variables
##############################################################################

# you may have to duplicate this list in ../Makefile under the 
# ifeq ($(FEATURE_JSON_BUILTIN_COMMONS),1)

SRC= \
     xml_types.ml \
     dtd_types.ml \
     xml_parser.ml \
     xml_lexer.ml \
     xmlParser.ml \
     dtd.ml \
     xml_parse.ml \
     xml_utils.ml \


TARGET=lib

INCLUDES=
#-I +camlp4
SYSLIBS= str.cma unix.cma bigarray.cma num.cma

##############################################################################
# Generic variables
##############################################################################

#dont use -custom, it makes the bytecode unportable.
OCAMLCFLAGS= -g -dtypes # -w A
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

xml_lexer.ml: xml_lexer.mll
	$(OCAMLLEX) $<
clean::
	rm -f xml_lexer.ml
beforedepend:: xml_lexer.ml

xml_parser.ml xml_parser.mli: xml_parser.mly
	$(OCAMLYACC) $<
clean::
	rm -f xml_parser.ml xml_parser.mli xml_parser.output
beforedepend:: xml_parser.ml xml_parser.mli

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
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

distclean::
	rm -f .depend

-include .depend
