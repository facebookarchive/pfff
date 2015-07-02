PIQI ?= piqi


all: prep test


prep:
	$(PIQI) cc > piqi.piqi
	$(PIQI) convert --add-defaults -t pb piqi.piqi


test:
	$(MAKE) -f Makefile.ocaml
	./otest
	cmp *.pb


clean:
	$(MAKE) -f Makefile.ocaml clean
	rm -f piqi.piqi piqi.piqi.pb* t.*

