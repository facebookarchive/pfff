# This file is part of JavaLib
# Copyright (c)2004 Nicolas Cannasse
# Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
# Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
# Copyright (c)2009 Nicolas Barre (INRIA)
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see 
# <http://www.gnu.org/licenses/>.


DOCDIR := doc/api

-include ../Makefile.config

DOCDIR:=../$(DOCDIR)

CSS=\"style.css\"

# ------ 
MODULES=jBasics jBasicsLow jCode jSignature jClass jDumpBasics jParseCode	\
jInstruction jUnparseSignature jDumpLow jHigh2Low jDumpJasmin jUnparse		\
jParseSignature jLow2High jParse jFile jManifest jPrint javalib

MODULE_INTERFACES=jBasics jBasicsLow jCode jClassLow jSignature jClass		\
jDumpBasics jUnparseSignature jDumpLow jDumpJasmin jParseCode jInstruction	\
jUnparse jParseSignature jParse jLow2High jHigh2Low jFile jManifest javalib

#FOR_PACK= -for-pack Javalib_pack

.SUFFIXES : .cmo .cmx .cmi .ml .mli
.PHONY : all install clean cleanall cleandoc doc remove
.DELETE_ON_ERROR : .depend

all: lib.cma lib.cmxa $(SHARED)

install: META javalib.cm[ia] javalib.cmxa $(MODULE_INTERFACES:=.mli) javalib_pack.cm[iox] javalib_pack.o 
	$(INSTALL) javalib $^ javalib.a
remove:
	$(FINDER) remove javalib

ocaml:javalib.cma
	$(OCAMLMKTOP) $(INCLUDE) -linkpkg -o $@ ptrees.cma javalib.cma

# compilation
jParseSignature.cmo:jParseSignature.ml
	$(OCAMLC) $(INCLUDE) $(PP) -c $<
jParseSignature.cmx jParseSignature.o:jParseSignature.ml
	$(OCAMLOPT) $(INCLUDE) $(PP) $(FOR_PACK) -c $<

.ml.cmo:
	$(OCAMLC) $(INCLUDE) -c $<
%.cmx %.o:%.ml
	$(OCAMLOPT) $(INCLUDE) $(FOR_PACK) -c $<
.mli.cmi:
	$(OCAMLC) $(INCLUDE) -c $<
%.ml: %.mll
	$(OCAMLLEX) $<

#javalib.cma: javalib_pack.cmo
#	$(OCAMLC) $(INCLUDE) -a -o $@ $^
lib.cma: $(MODULES:=.cmo)
	$(OCAMLC) $(INCLUDE) -a -o $@ $^

#javalib.cmxa: javalib_pack.cmx
#	$(OCAMLOPT) $(INCLUDE) -a -o $@ $^

lib.cmxa: $(MODULES:=.cmx)
	$(OCAMLOPT) $(INCLUDE) -a -o $@ $^

# javalib.cmxs: javalib.cmxa
# 	$(OCAMLOPT) $(INCLUDE) -linkpkg -shared -o $@ $^

javalib_pack.cmo: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmo)
	$(OCAMLC) $(INCLUDE) -pack $(MODULES:=.cmo) -o $@

javalib_pack.cmx: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.cmx)
	$(OCAMLOPT) $(INCLUDE) -pack $(MODULES:=.cmx) -o $@


../ptrees/ptrees.cma ../ptrees/ptrees.cmxa:$(wildcard ../ptrees/*.ml ../ptrees/*.mli)
	$(MAKE) -C $(@D) $(@F)

# doc
doc: $(MODULE_INTERFACES:=.cmi) $(MODULES:=.ml) ../intro.ocamldoc
	mkdir -p $(DOCDIR)
	$(OCAMLDOC) $(RECODE) $(INCLUDE) -d $(DOCDIR) -html -stars	\
		-colorize-code -intro ../intro.ocamldoc -t Javalib	\
		$(MODULE_INTERFACES:=.mli) ../ptrees/ptmap.mli		\
		 ../ptrees/ptset.mli
	echo "</body></html>" >> $(DOCDIR)/index.html # workaround for a bug in ocamldoc

# cleaning
clean:
	$(RM) .depend *.cmi *.cmo *.cmx *.annot *.obj *.o *.a *~ \
	jManifest.ml lib.*

cleanall: clean
	$(RM) -r ocaml *.cmi	\
		*.cma *.cmxa *.cmxs 
cleandoc:
	$(RM) -r $(DOCDIR)

# Dependencies
.depend:../Makefile.config $(MODULE_INTERFACES:=.mli) $(MODULES:=.ml)
	$(OCAMLDEP) $(PP) $^ > $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),cleanall)
ifneq ($(MAKECMDGOALS),cleandoc)
-include .depend
endif
endif
endif
