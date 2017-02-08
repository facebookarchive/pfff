.PHONY: all ocaml test ocaml_ext test_ext clean


all: ocaml test ocaml_ext test_ext


ocaml:
	GOAL=add_person $(MAKE) -f Makefile.ocaml
	GOAL=list_people $(MAKE) -f Makefile.ocaml


test:
	./test


ocaml_ext:
	rm -f addressbook_piqi.cm? # forcing make to rebuild it
	$(MAKE) -f Makefile.ocaml_ext


test_ext: ocaml_ext
	./io_json_xml_pb addressbook.piq.pb


clean:
	GOAL=add_person $(MAKE) -f Makefile.ocaml clean
	GOAL=list_people $(MAKE) -f Makefile.ocaml clean
	rm -f addressbook.piq.pb addressbook.pb.piq l1 l2
	$(MAKE) -f Makefile.ocaml_ext clean

