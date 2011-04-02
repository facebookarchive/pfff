## HERE IS THE SECTION CONCERNING .eliom SOURCE FILE COMPILATION

## type inference #######

${TEMP}:
	mkdir -p $@

TYPELIB := \
   -package ocsigen.ext.eliom,ocsigen.ext.eliom.syntax.type \
   -I ${TEMP}/server ${SERVERLIB}

.SECONDARY: ${${wildcard *.eliom}:%.eliom=${TEMP}/%.type.mli}
${TEMP}/%.type.mli: %.eliom ${TEMP}
	${OCAMLFIND} ocamlc \
	    -syntax camlp4o -ppopt -impl \
	    -thread ${TYPELIB} -i -impl $< > $@

## server side ##########

SERVERCMOS := $(SERVERFILES:%.eliom=${TEMP}/server/%.cmo)
SERVERCMOS := $(SERVERCMOS:%.ml=${TEMP}/server/%.cmo)
SERVERCMXS := $(SERVERFILES:%.eliom=${TEMP}/server/%.cmx)
SERVERCMXS := $(SERVERCMXS:%.ml=${TEMP}/server/%.cmx)

SERVERLIB  := -package ocsigen.ext.eliom -I ${TEMP}/server ${SERVERLIB}
ESERVERLIB := -package ocsigen.ext.eliom.syntax.server ${SERVERLIB}

${TEMP}/server:
	mkdir -p $@

${TEMP}/server/%.cmi: %.mli ${TEMP}/server
	${OCAMLFIND} ocamlc \
	     -syntax camlp4o \
	     ${SERVERLIB} \
	     -thread -o $@ -c $<
${TEMP}/server/%.cmo: %.ml ${TEMP}/server
	${OCAMLFIND} ocamlc \
	     -syntax camlp4o \
	     ${SERVERLIB} \
	     -thread -o $@ -c $<
${TEMP}/server/%.cmx: %.ml ${TEMP}/server
	${OCAMLFIND} ocamlopt \
	     -syntax camlp4o \
	     ${SERVERLIB} \
	     -thread -o $@ -c $<

${TEMP}/server/%.cmo: %.eliom ${TEMP}/%.type.mli ${TEMP}/server
	${OCAMLFIND} ocamlc \
	    ${ESERVERLIB} \
	    -syntax camlp4o \
	    -ppopt -type -ppopt ${<:%.eliom=${TEMP}/%.type.mli} -ppopt -impl \
	    -thread -o $@ -c -impl $<

${TEMP}/server/%.cmx: %.eliom  ${TEMP}/%.type.mli ${TEMP}/server
	${OCAMLFIND} ocamlopt \
	    ${ESERVERLIB} \
	    -syntax camlp4o \
	    -ppopt -type -ppopt ${<:%.eliom=${TEMP}/%.type.mli} -ppopt -impl \
	    -thread -o $@ -c -impl $<

$(PROJECTNAME).cma: $(SERVERCMOS)
	$(OCAMLFIND) ocamlc -a -o $@ -thread -I ${TEMP}/server $(ESERVERLIB) $^

$(PROJECTNAME).cmxa: $(SERVERCMXS)
	$(OCAMLFIND) ocamlopt -a -o $@ -thread -I ${TEMP}/server $(ESERVERLIB) $^

$(PROJECTNAME).cmxs: $(PROJECTNAME).cmxa
	$(OCAMLFIND) ocamlopt -linkall -shared -thread -o $@ $^

## client side ##########

CLIENTCMOS := $(CLIENTFILES:%.eliom=${TEMP}/client/%.cmo)
CLIENTCMOS := $(CLIENTCMOS:%.ml=${TEMP}/client/%.cmo)

CLIENTLIB  := -package ocsigen.ext.eliom_client -I ${TEMP}/client ${CLIENTLIB}
ECLIENTLIB := -package ocsigen.ext.eliom_client.syntax ${CLIENTLIB}

${TEMP}/client:
	mkdir -p $@
$(STATICDIR):
	mkdir -p $@

${TEMP}/client/%.cmi: %.mli ${TEMP}/client
	${OCAMLFIND} ocamlc \
	     -syntax camlp4o \
	     ${CLIENTLIB} \
	     -o $@ -c $<
${TEMP}/client/%.cmo: %.ml ${TEMP}/client
	${OCAMLFIND} ocamlc \
	     -syntax camlp4o \
	     ${CLIENTLIB} \
	     -o $@ -c $<

${TEMP}/client/%.cmo: %.eliom ${TEMP}/%.type.mli ${TEMP}/client
	${OCAMLFIND} ocamlc \
	    ${ECLIENTLIB} \
	    -syntax camlp4o \
	    -ppopt -type -ppopt ${<:%.eliom=${TEMP}/%.type.mli} -ppopt -impl \
	    -o $@ -c -impl $<

${TEMP}/client/${PROJECTNAME}: ${CLIENTCMOS}
	${OCAMLFIND} ocamlc $(ECLIENTLIB) -linkpkg $^ -o $@

$(STATICDIR)/$(PROJECTNAME).js: $(STATICDIR) \
		$(ELIOMCLIENTDIR)/eliom_client.js \
		${TEMP}/client/${PROJECTNAME}
	${JS_OF_OCAML} -pretty $^ -o $@

## oclosure #############

$(STATICDIR)/$(PROJECTNAME)_req.js: $(STATICDIR)/$(PROJECTNAME).js
	${OCAML} str.cma $(OCLOSUREDIR)/requirements.ml $^
