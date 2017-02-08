include ../Makefile.config


all: test_builtin test_complex

.PHONY: test_builtin test_complex

test_builtin:
	set -e; \
	for i in comment bool int float string empty; \
	do \
		./test_piq $$i; \
	done


test_complex:
	./test_piqi empty
	./test_piqi_proto empty

	./test_piqi piq-format
	./test_piq piq-word
	./test_piq piq-text

	./test_piq complex
	./test_piqi complex
	./test_piqi_proto complex

	./test_piq person
	./test_piqi person
	./test_piqi_proto person

	./test_piqi def
	./test_piqi_proto def
	./test_piq list

	./test_piq flag
	./test_piq piqi-any

	./test_piqi record-variant-list
	./test_piqi_proto record-variant-list
	./test_piq record-variant-list

	./test_piqi function
	./test_piq function
	./test_piqi_proto function

	./test_getopt

	test ! `which protoc` || ($(PIQI) of-proto --normalize addressbook.proto; \
	./test_piqi addressbook.proto; \
	./test_piq addressbook;)


clean:
	rm -f *.pb *.pib *.json *.xml *.piqi.proto* addressbook.proto.piqi
