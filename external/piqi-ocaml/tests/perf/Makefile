.PHONY: all ocaml test clean


PIQI ?= piqi


all: ocaml test


ocaml: piqi-obj.piqi
	$(MAKE) -f Makefile.ocaml


piqi-obj.piqi:
	$(PIQI) cc | sed -e 's/\.module .*//;s/\.code .*//' > piqi.piqi
	ln -sf ../../piqic-ocaml/piqi.ocaml.piqi
	$(PIQI) expand -e ocaml piqi.piqi > $@


test: ocaml
	$(PIQI) convert -t pb addressbook.piq

	echo ":piqi-obj/piqi [" > piqi.piq
	cat piqi-obj.piqi >> piqi.piq
	echo "]" >> piqi.piq

	$(PIQI) convert --no-warnings --add-defaults -t pb piqi.piq
	#./test


clean:
	$(MAKE) -f Makefile.ocaml clean
	rm -f addressbook.piq.pb piqi-expanded.piqi piqi-obj.* piqi.ocaml.piqi piqi.piq piqi.piq.pb piqi.piqi

