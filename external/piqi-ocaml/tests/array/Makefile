PIQI ?= piqi


all:
	$(MAKE) -f Makefile.ocaml
	$(PIQI) convert -t pb test-all.piq
	./test
	cmp test-all.piq.pb test-all.piq.pb.array


clean:
	$(MAKE) -f Makefile.ocaml clean
	rm -f test-all.piq.* test-all-lists.piq.*
