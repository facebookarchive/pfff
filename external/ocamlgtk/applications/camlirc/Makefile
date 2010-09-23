# Makefile for IRC Client
# $Id: Makefile 1354 2007-07-20 04:18:38Z garrigue $
ARCH=$(shell uname) 

ifeq ($(ARCH), FreeBSD)
EXTRALIB=-cclib -lxpg4
endif

THFLAGS = -thread 
THLIBS = unix.cma threads.cma gtkInit.cmo gtkThread.cmo

CAMLC = ocamlc.opt
CAMLOPT = ocamlopt.opt
CAMLLEX = ocamllex
COMPILER = $(CAMLC) $(MLFLAGS) -g -w msy -labels -c
LINKER = $(CAMLC) $(MLFLAGS) -custom $(EXTRALIB)
COMPOPT = $(CAMLOPT) $(MLFLAGS) -w msy -labels -c
LINKOPT = $(CAMLOPT) $(MLFLAGS) 

SETLIB = 1
LIBDIR = $(shell ocamlc -where)
MLFLAGS = $(THFLAGS) -I +lablgtk2 
THOPTLIBS1 = $(THLIBS:.cma=.cmxa)
THOPTLIBS = $(THOPTLIBS1:.cmo=.cmx)

LIBRARIES = lablgtk.cma str.cma 
LIBOBJECTS = property.cmo constants.cmo eucjp.cmo \
	xml_lexer.cmo xml.cmo \
	server.cmo prefix.cmo ircArg.cmo reply.cmo message.cmo \
	parser.cmo message_utils.cmo message_handler.cmo ctcp.cmo \
	members.cmo general_channel.cmo cf_manager.cmo \
	channel.cmo channelview.cmo global.cmo \
	control.cmo entry.cmo 

OBJECTS =  $(LIBOBJECTS) newmain.cmo

LIBIRCOBJECTS = $(LIBOBJECTS) irc_widget.cmo
IRCLIB = irc.cma

all: camlirc

install: camlirc
	cp camlirc /usr/local/bin

camlirc: $(OBJECTS)
	$(LINKER) $(LIBRARIES) $(THLIBS) $(OBJECTS) -g -o $@ 

camlirc.opt: $(OBJECTS:.cmo=.cmx)
	$(LINKOPT) -o $@ $(LIBRARIES:.cma=.cmxa) $(THOPTLIBS) \
		$(OBJECTS:.cmo=.cmx) -o $@ 

$(IRCLIB): $(LIBIRCOBJECTS)
	$(CAMLC) -a -custom  $(MLFLAGS) -o $@ $(LIBRARIES) $(LIBIRCOBJECTS)

$(IRCLIB:.cma=.cmxa): $(LIBIRCOBJECTS:.cmo=.cmx)
	$(CAMLOPT) -a $(MLFLAGS) -o $@  \
	$(LIBIRCOBJECTS:.cmo=.cmx)


.SUFFIXES: .ml .mli .mll .cmo .cmi .cmx .c .o
.c.o:
	$(CCOMPILER) $<
.ml.cmo:
	$(COMPILER) $<
.mli.cmi:
	$(COMPILER) $<
.ml.cmx:
	$(COMPOPT) $<
.mll.ml:
	$(CAMLLEX) $<

clean:
	rm -f *.cm* *.o camlirc *~ xml_lexer.ml

depend:
	ocamldep *.ml *.mli > .depend

xml_lexer.ml: xml_lexer.mll

checkin:
	cvs commit 
	cvs commit -m "update date field" -f constants.ml

include .depend

