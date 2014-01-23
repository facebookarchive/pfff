
# the perl below is because ocaml INT_MAX is smaller than PHP_INT_MAX
# and this causes the json parsing of the file to fail.
update:
	rm -rf hni/ idl/ system/
	mkdir -p hni idl system system/facebook
	~/pfff/pfff_test -generate_php_stdlib ~/local/fbcode/hphp/system/idl idl/
	~/pfff/pfff_test -generate_php_stdlib ~/local/fbcode/hphp/facebook/extensions idl/facebook/
	./stdlib_from_hni.sh ~/local/fbcode/hphp hni
	cp -rf ~/local/fbcode/hphp/system/php/* system/
	cp -rf ~/local/fbcode/hphp/facebook/system/php/* system/facebook/
	cp -f ~/local/fbcode/hphp/system/idl/constants.idl.json /tmp/
	perl -p -i -e 's/9223372036854775807/0/g;' /tmp/constants.idl.json
	~/pfff/pfff_test -builtins_of_idl /tmp/constants.idl.json > idl/builtins_constants.idl.php

stdlib: update
	rm -rf stdlib/
	mkdir -p stdlib
	cp -r hni idl stdlib/
	for file in `find stdlib/ -name '*.php'`; do nn=`echo $$file | sed s/php/hhi/`; mv $$file $$nn; done
	# For one reason or another, we hand-roll these.
	rm stdlib/idl/builtins_asio.idl.hhi
	rm stdlib/idl/builtins_collections.idl.hhi
	rm stdlib/idl/builtins_continuation.idl.hhi
	rm stdlib/idl/builtins_domdocument.idl.hhi
	rm stdlib/idl/builtins_xmlreader.idl.hhi
	rm stdlib/idl/facebook/builtins_string_buffer.idl.hhi

check:
	~/pfff/codegraph -lang php -build .
	cat pfff.log

for_pad:
	cd ~/local/fbcode/; git pull
	make update
	make check
