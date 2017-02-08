include make/Makefile.dirs


DESTDIR ?= /usr/local


DIRS = piqirun piqic-ocaml


.PHONY: install uninstall clean distclean depend


install:
	$(MAKE) -C piqirun install
	-install -d $(DESTDIR)/bin
	install piqic-ocaml/piqic-ocaml $(DESTDIR)/bin

depend::

uninstall:
	$(MAKE) -C piqirun uninstall
	rm -f $(DESTDIR)/bin/piqic-ocaml


distclean: clean
