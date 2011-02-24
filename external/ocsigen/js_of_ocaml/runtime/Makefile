FILES= 	mlString.js ieee_754.js int64.js md5.js marshall.js \
	lexing.js parsing.js \
	stdlib.js jslib.js jslib_js_of_ocaml.js

runtime.js: $(FILES)
	cat $(FILES) > runtime.js

clean:
	rm -f runtime.js
