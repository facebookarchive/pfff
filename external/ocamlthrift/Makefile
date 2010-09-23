
##############################################################################
# Variables
##############################################################################

SRC=Thrift.ml \
  TBinaryProtocol.ml \
  TChannelTransport.ml \
  TSocket.ml \
  TServerSocket.ml \
  TServer.ml \
   TSimpleServer.ml \
   TThreadedServer.ml \


TARGET=thrift

##############################################################################
# Generic variables
##############################################################################

INCLUDES=

SYSLIBS= unix.cma threads.cma

##############################################################################
# Generic variables
##############################################################################

#dont use -custom, it makes the bytecode unportable.
OCAMLCFLAGS= -g -dtypes $(PP) -thread

# This flag is also used in subdirectories so don't change its name here.
OPTFLAGS=

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES)  $(SYSINCLUDES) -thread
OCAMLOPT=ocamlopt$(OPTBIN) $(PP) $(OPTFLAGS) $(INCLUDES) $(SYSINCLUDES) -thread
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

all: $(TARGET).cma $(TARGET2)
all.opt: $(TARGET).cmxa
opt: all.opt

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top


$(TARGET2): $(TARGET).cma ircbot.cmo 
	$(OCAMLC) -o $@ $(SYSLIBS) $^

clean::
	rm -f $(TARGET2)

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
	$(OCAMLDEP) $(PP) *.mli *.ml    > .depend

distclean:
	rm -f .depend

-include .depend

