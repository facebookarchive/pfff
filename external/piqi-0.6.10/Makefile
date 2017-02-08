ifeq ($(MAKECMDGOALS),distclean)
-include Makefile.config
-include $(PIQI_ROOT)/make/Makefile.dirs
else
include Makefile.config
include $(PIQI_ROOT)/make/Makefile.dirs
endif


DIRS = piqilib src


.PHONY: deps build-dir install strip distclean \
	ocaml ocaml-install ocaml-uninstall \
	doc piqi


# export installation and search path for third-party OCaml dependencies
ifeq ($(MAKECMDGOALS),deps)
OCAMLFIND_DESTDIR := $(PIQI_BUILD)/lib/ocaml
ifeq ($(SYSTEM),$(filter $(SYSTEM),mingw mingw64))
OCAMLFIND_DESTDIR := $(shell cygpath -w $(OCAMLFIND_DESTDIR))
endif
export OCAMLFIND_DESTDIR
endif


# export installation path for third-party deps and piqilib
ifneq ($(findstring ocaml-,$(MAKECMDGOALS)),)
ifneq ($(PIQI_OCAML_DESTDIR),)
export OCAMLFIND_DESTDIR = $(PIQI_OCAML_DESTDIR)
endif
endif


deps: build-dir
	$(MAKE) -C deps download
	$(MAKE) -C deps
	$(MAKE) -C deps uninstall install


build-dir:
	mkdir -p build/lib/ocaml


doc:
	$(MAKE) -C doc


install:
	-install -d $(DESTDIR)$(PIQI_PREFIX)/bin
	install src/piqi $(DESTDIR)$(PIQI_PREFIX)/bin
	-install -d $(DESTDIR)$(PIQI_PREFIX)/share/man/man1
	install -m 644 doc/piqi.1 $(DESTDIR)$(PIQI_PREFIX)/share/man/man1


# especially useful for cross-compilation
STRIP ?= strip
strip:
	$(STRIP) src/piqi


# re-generate _piqi.ml from .piqi and copy the latest version of piqilib.ml  --
# called manually as "make piqi" when there were changes in the .piqi files or
# to take advantage of new versions of piqic-ocaml and the piqirun.pb runtime
# library
piqi:
	$(MAKE) -C piqilib piqi
	$(MAKE) -C src piqi


# build bytecode version of the library (it is not built by default to avoid
# unnecessary slowdown of the main build)
ocaml:
	$(MAKE) -C piqilib bcl


ocaml-install: ocaml-uninstall
	test -d $(PIQI_OCAML_DESTDIR) || mkdir -p $(PIQI_OCAML_DESTDIR)
	$(MAKE) -C deps install
	$(MAKE) -C piqilib install


ocaml-uninstall:
	$(MAKE) -C deps uninstall
	$(MAKE) -C piqilib uninstall


clean::
	$(MAKE) -C tests clean


distclean:
	if [ -f Makefile.config ]; then \
		$(MAKE) -C deps clean; \
		$(MAKE) clean; \
		rm -rf $(PIQI_BUILD); \
		rm Makefile.config; \
	fi

