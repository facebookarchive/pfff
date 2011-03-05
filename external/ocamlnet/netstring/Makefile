TOP_DIR=..

OBJECTS  = netconst.cmo netstring_pcre.cmo netstring_str.cmo netbuffer.cmo \
	   netaux.cmo netchannels.cmo netdate.cmo \
	   netdb.cmo netmappings.cmo netconversion.cmo netulex.cmo \
           netencoding.cmo netstream.cmo \
	   mimestring.cmo \
           nethtml_scanner.cmo nethtml.cmo \
	   neturl.cmo \
	   netaddress.cmo \
	   netmime.cmo netsendmail.cmo nethttp.cmo \
	   netpagebuffer.cmo netfs.cmo netglob_lex.cmo netglob.cmo

PKGNAME  = netstring

#pad: was when using ocamlfind
#REQUIRES = pcre bigarray
INCLUDES += -I ../../ocamlpcre/lib

INCLUDES += $(INC_NETSYS)

ISO_MAPPINGS   = mappings/iso*.unimap
JP_MAPPINGS    = mappings/jis*.*map
KR_MAPPINGS    = mappings/ks*.*map
OTHER_MAPPINGS = mappings/cp*.unimap \
                 mappings/adobe*.unimap \
                 mappings/koi*.unimap \
                 mappings/mac*.unimap \
                 mappings/windows*.unimap

PMAP = netmappings_iso.pmap netmappings_other.pmap 
#pad: netmappings_jp.pmap netmappings_kr.pmap

# netconversion.cmx, netbuffer.cmx: 
# Allow cross-library inlining for these modules

#pad: netmappings_jp netmappings_kr
INSTALL_EXTRA_CMO = netmappings_iso  \
		    netmappings_other netmappings_min \
		    netstring_top \
		    netaccel_link
#pad: netmappings_jp netmappings_kr
INSTALL_EXTRA_CMX = netmappings_iso \
                    netmappings_other netmappings_min \
		    netconversion \
		    netbuffer
INSTALL_EXTRA = $(INSTALL_EXTRA_CMO:=.cmo) \
		$(INSTALL_EXTRA_CMX:=.cmx) $(INSTALL_EXTRA_CMX:=.p.cmx) \
		$(INSTALL_EXTRA_CMX:=.o) $(INSTALL_EXTRA_CMX:=.p.o) \
		netdb-packlist \
		dllnetaccel_c.*

DOBJECTS = netconversion.mli netchannels.mli netstream.mli mimestring.mli \
	netmime.mli netsendmail.mli neturl.mli netaddress.mli netbuffer.mli \
	netdate.mli netencoding.mli netulex.mli netaccel.mli \
	netaccel_link.mli nethtml.mli netstring_str.mli netstring_pcre.mli \
	netmappings.mli netaux.mli nethttp.mli netpagebuffer.mli \
	netfs.mli netglob.mli \
	netchannels_tut.txt netmime_tut.txt netsendmail_tut.txt \
	netulex_tut.txt neturl_tut.txt

#PP_OPTIONS = -package camlp4 -syntax camlp4o

INSTOTHER = install-netdb
PACKLIST = netdb-packlist

BUILD_EXTRA = $(PMAP:.pmap=.cmo) netmappings_min.cmo

ALL_EXTRA = $(BUILD_EXTRA) netstring_top.cmo netaccel.cma netaccel_link.cmo
OPT_EXTRA = $(BUILD_EXTRA:.cmo=.cmx)

all::

netaccel.cma: netaccel_c.o netaccel.cmo
	$(OCAMLMKLIB) -o netaccel -oc netaccel_c netaccel_c.o netaccel.cmo

#netaccel.cmxa: netaccel_c.o netaccel.cmx
#	$(OCAMLMKLIB) -o netaccel -oc netaccel_c netaccel_c.o netaccel.cmx

netdb.ml: netdb.mlp netdb.cmi
	sed -e 's|@NET_DB_DIR@|$(NET_DB_DIR)|' netdb.mlp >netdb.ml

netdb_done: tools_done $(PMAP)
	mkdir -p netdb
	cd netdb && \
		../tools/unimap_to_ocaml/unimap_to_ocaml -netdb ../*.pmap
	touch netdb_done

netmappings_iso.pmap: tools_done
	test ! -d mappings || tools/unimap_to_ocaml/unimap_to_ocaml \
		-o netmappings_iso.pmap -pmap $(ISO_MAPPINGS)

netmappings_iso.ml: netmappings_iso.pmap netdb.cmi
	tools/unimap_to_ocaml/unimap_to_ocaml \
		-o netmappings_iso.ml netmappings_iso.pmap

#pad:netmappings_jp.pmap: tools_done
#pad:	test ! -d mappings || tools/unimap_to_ocaml/unimap_to_ocaml \
#pad:		-o netmappings_jp.pmap -pmap $(JP_MAPPINGS)
#pad:
#pad:#	test ! -f mappings/Unihan.txt.bz2 || \
#pad:#	   bzip2 -c -d mappings/Unihan.txt.bz2 | \
#pad:# 	   tools/unimap_to_ocaml/unihan_extract \
#pad:#	       -o netmappings_jp.pmap -jis0208 -jis0212
#pad:
#pad:netmappings_jp.ml: netmappings_jp.pmap netdb.cmi
#pad:	tools/unimap_to_ocaml/unimap_to_ocaml \
#pad:		-o netmappings_jp.ml netmappings_jp.pmap
#pad:
#pad:netmappings_kr.pmap: tools_done
#pad:	test ! -d mappings || tools/unimap_to_ocaml/unimap_to_ocaml \
#pad:		-o netmappings_kr.pmap -pmap $(KR_MAPPINGS)
#pad:
#pad:netmappings_kr.ml: netmappings_kr.pmap netdb.cmi
#pad:	tools/unimap_to_ocaml/unimap_to_ocaml \
#pad:		-o netmappings_kr.ml netmappings_kr.pmap

netmappings_other.pmap: tools_done
	test ! -d mappings || tools/unimap_to_ocaml/unimap_to_ocaml \
		-o netmappings_other.pmap -pmap $(OTHER_MAPPINGS)

netmappings_other.ml: netmappings_other.pmap netdb.cmi
	tools/unimap_to_ocaml/unimap_to_ocaml \
		-o netmappings_other.ml netmappings_other.pmap

netdb-packlist: netdb_done
	test -d netdb
	(cd netdb && ls *.netdb) | \
	{ while read f; do echo "$(NET_DB_DIR)/$$f"; done; } >netdb-packlist

.PHONY: install-netdb
install-netdb:
	mkdir -p "$(DESTDIR)$(NET_DB_DIR)"
	cp netdb/*.netdb "$(DESTDIR)$(NET_DB_DIR)"
	@echo "Installed .netdb files into $(DESTDIR)$(NET_DB_DIR)"

tools_done:
	cd tools && $(MAKE)
	touch tools_done

netdate.ml: netdate.mlp
	$(CAMLP4) pa_o.cmo pa_op.cmo pr_o.cmo -impl netdate.mlp > netdate.ml

nethttp.ml: nethttp.mlp
	$(CAMLP4) pa_o.cmo pa_op.cmo pr_o.cmo -impl nethttp.mlp > nethttp.ml

netdate.cmo: netdate.cmi
netdate.cmx: netdate.cmi

netconst.ml: netconst.mlp
	sed -e 's/@VERSION@/$(VERSION)/' netconst.mlp >netconst.ml

netconst.cmo: netconst.cmi
netconst.cmx: netconst.cmi

include $(TOP_DIR)/Makefile.rules

CLEAN_LIST += tools_done netdb_done netdb.ml netdate.ml $(PMAP:.pmap=.ml)
CLEAN_LIST += netconst.ml

clean::
	test ! -d mappings || rm -f $(PMAP)
	rm -rf netdb
	cd tools && $(MAKE) clean

distclean::
	$(MAKE) clean


-include depend
