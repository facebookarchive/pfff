                        The CamlZip library

DESCRIPTION:

This Objective Caml library provides easy access to compressed files in ZIP
and GZIP format, as well as to Java JAR files.  It provides functions
for reading from and writing to compressed files in these formats.

REQUIREMENTS:

- Objective Caml 3.06 or up.

- The Zlib C library, version 1.1.3 or up.  If it is not installed on
  your system (look for libz.a or libz.so), get it from
  http://www.gzip.org/.  If you are running Linux or BSD, chances
  are that your distribution provides precompiled binaries for this
  library.

INSTALLATION:

- Edit the three variables at the beginning of the Makefile to reflect
  the location where Zlib is installed on your system.  The defaults
  are OK for Linux.

- Do "make all".

- If the Objective Caml native-code compiler is available on your platform
  (look for the "ocamlopt" executable), do "make allopt".

- Become super-user if necessary and do
         make install
         make installopt        # if you did "make allopt" earlier
     or  make install-findlib   # to install using ocamlfind
  This installs the library in the standard Objective Caml library directory.



DOCUMENTATION:

See the comments in files zip.mli and gzip.mli.

Compilation options:      -I +zip
ocamlc linking options:   -I +zip zip.cma
ocamlopt linking options: -I +zip zip.cmxa

The directory test/ contains examples of using this library.

LICENSING:

This library is copyright 2001, 2002, 2006, 2007, 2008
Institut National de Recherche en Informatique et en Automatique (INRIA),
and distributed under the terms of the GNU Lesser General Public
License (LGPL) version 2.1 or above, with a special exception
concerning static linking.  See the file LICENSE for the exact
licensing terms.

BUG REPORTS AND USER FEEDBACK:

Please e-mail Xavier.Leroy@inria.fr

