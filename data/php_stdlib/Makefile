
# the perl below is because ocaml INT_MAX is smaller than PHP_INT_MAX
# and this causes the json parsing of the file to fail.
update:
	cd ~/local/fbcode/; git pull
	mkdir -p idl facebook
	rm -f idl/* facebook/*
	~/pfff/pfff_test -generate_php_stdlib ~/local/fbcode/hphp/system/idl idl/
	~/pfff/pfff_test -generate_php_stdlib ~/local/fbcode/hphp/facebook/extensions facebook/
	cp -rf ~/local/fbcode/hphp/system/php/* .
	cp -rf ~/local/fbcode/hphp/facebook/system/php/* facebook/
	cp -f ~/local/fbcode/hphp/system/idl/constants.idl.json /tmp/
	perl -p -i -e 's/9223372036854775807/0/g;' /tmp/constants.idl.json
	~/pfff/pfff_test -builtins_of_idl /tmp/constants.idl.json > builtins_constants.idl.php
	make check

check:
	~/pfff/codegraph -lang php -build .
	cat pfff.log
