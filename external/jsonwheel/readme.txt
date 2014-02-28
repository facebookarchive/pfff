This is an OCaml library which reads and writes data in the JSON format
(JavaScript Object Notation).
This format can be used as a light-weight replacement for XML.
Visit http://www.json.org for more information about JSON.

The documentation for this library is located at
  http://martin.jambon.free.fr/json-wheel/


Installation
============

Requirements:
- OCaml
- GNU make
- the findlib library manager (ocamlfind command)
- the netstring library

From the source directory, do:

  make
  make install

If you want to remove the package do:

  make uninstall


Standard compliance
===================

The JSON parser, in the default mode, conforms to the specifications 
of RFC 4627, with only some limitations due to the implementation 
of the corresponding OCaml types:

* ints that are too large to be represented with the OCaml int type
  cause an error. The limit depends whether it is a 32-bit or 64-bit
  platform (see min_int and max_int).

* floats may be represented with reduced precision as they must fit
  into the 8 bytes of the "double" format.

* The size of OCaml strings is limited to about 16MB on 32-bit
  platforms, and much more on 64-bit platforms (see Sys.max_string_length).


RFC 4627: http://www.ietf.org/rfc/rfc4627.txt?number=4627


The UTF-8 encoding is supported, however no attempt is made at
checking whether strings are actually valid UTF-8 or not. Therefore, other
ASCII-compatible encodings such as the ISO 8859 series are supported
as well.


Tests
=====

Json.org provides a test suite. You can download the file (test.zip),
unzip it in the parent directory, and run "make test".
Look for ERROR messages, which indicate that a file that should fail
actually passes or that a file that should pass fails the test.

../test/fail18.json doesn't pass: this is only because an int which is
too large for the OCaml int type on a 32-bit platform.

../test/fail18.json passes: it is marked as "should fail" because is
has a high number of nesting. Although the standard allows such
restrictions, there are not mandatory at all. Our parser does not have
such a restriction.
