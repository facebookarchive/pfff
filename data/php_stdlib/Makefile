update:
	cd ~/local/fbcode/; git pull
	~/pfff/pfff_test -generate_php_stdlib ~/local/fbcode/hphp/system/idl .

check:
	~/pfff/codegraph -lang php -build .
	cat pfff.log