TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC=jBasics.ml jBasicsLow.ml jCode.ml jSignature.ml \
 jClass.ml jClassLow.ml jDumpBasics.ml jParseCode.ml	\
 jInstruction.ml jUnparseSignature.ml jDumpLow.ml jHigh2Low.ml jUnparse.ml \
 jParseSignature.ml jLow2High.ml jParse.ml

# jFile jManifest jPrint javalib
# jDumpJasmin jUnparse		

SYSLIBS= str.cma unix.cma 

LIBS=

INCLUDEDIRS= \
 $(TOP)/external/extlib \
 $(TOP)/external/ptrees

##############################################################################
# Generic variables
##############################################################################

WARNING_FLAGS=-w -32
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top


PP=-pp camlp4o

jParseSignature.cmo: jParseSignature.ml
	$(OCAMLC) $(PP) -c $<

jParseSignature.cmx: jParseSignature.ml
	$(OCAMLOPT) $(PP) -c $<

#php_serialize.cmx: php_serialize.ml
#	$(OCAMLOPT) $(PP) -c $<


jManifest.ml: jManifest.mll
	$(OCAMLLEX) $<
clean::
	rm -f jManifest.ml
beforedepend:: jManifest.ml


##############################################################################
# Generic rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################
