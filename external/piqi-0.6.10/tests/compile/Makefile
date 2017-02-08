include ../../Makefile.config


test: init test_cc test_compile


init:
	$(PIQI) cc -o piqi.piqi


test_cc:
	$(PIQI) cc > p.piqi
	$(PIQI) cc -o p1.piqi p.piqi
	cmp p.piqi p1.piqi

	$(PIQI) cc -t piqi >/dev/null
	$(PIQI) cc -t json >/dev/null
	$(PIQI) cc -t xml >/dev/null
	$(PIQI) cc -t pb >/dev/null

	$(PIQI) cc -t piqi piqi.piqi >/dev/null
	$(PIQI) cc -t json piqi.piqi >/dev/null
	$(PIQI) cc -t xml piqi.piqi >/dev/null
	$(PIQI) cc -t pb piqi.piqi >/dev/null


test_compile:
	$(PIQI) cc -e piqi-ocaml -t pb -o self-spec.pb piqi.piqi

	$(PIQI) compile --strict --self-spec self-spec.pb piqi.piqi >p.piqi
	$(PIQI) compile --strict --self-spec self-spec.pb -t json piqi.piqi >/dev/null
	$(PIQI) compile --strict --self-spec self-spec.pb -t xml piqi.piqi >/dev/null
	$(PIQI) compile --strict --self-spec self-spec.pb -t pb piqi.piqi >/dev/null

	$(PIQI) compile --strict --self-spec self-spec.pb -e ocaml ../../src/descriptor.proto.piqi >d.piqi
	$(PIQI) compile --strict --self-spec self-spec.pb -e ocaml -t json ../../src/descriptor.proto.piqi >/dev/null
	$(PIQI) compile --strict --self-spec self-spec.pb -e ocaml -t xml ../../src/descriptor.proto.piqi >/dev/null
	$(PIQI) compile --strict --self-spec self-spec.pb -e ocaml -t pb ../../src/descriptor.proto.piqi >/dev/null


clean:
	rm -f piqi.piqi p.piqi d.piqi p1.piqi self-spec.pb

