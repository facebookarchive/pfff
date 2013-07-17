update:
	cd ~/local/fbcode/; git pull
	~/pfff/pfff_test -generate_php_stdlib ~/local/fbcode/hphp/system/idl idl/
	~/pfff/pfff_test -generate_php_stdlib ~/local/fbcode/hphp/facebook/extensions/async_mysql idl/
	make check

check:
	~/pfff/codegraph -lang php -build .
	cat pfff.log