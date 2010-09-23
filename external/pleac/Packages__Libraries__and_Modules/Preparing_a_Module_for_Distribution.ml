(* ********************************************************************** *)
(* Preparing a Module for Distribution *)
(* ********************************************************************** *)
let pleac_Preparing_a_Module_for_Distribution () = 
  (* The easiest way to prepare a library for distribution is to build with
     OCamlMakefile and include a META file for use with findlib.
  
     OCamlMakefile is available here:
     http://www.ocaml.info/home/ocaml_sources.html#OCamlMakefile
  
     findlib is available here:
     http://projects.camlcity.org/projects/findlib.html *)
  
  (* Put the following in a file called "Makefile" and edit to taste: *)
  
  OCAMLMAKEFILE = OCamlMakefile
  
  RESULT = mylibrary
  SOURCES = mylibrary.mli mylibrary.ml
  PACKS = pcre
  
  all: native-code-library byte-code-library
  install: libinstall
  uninstall: libuninstall
  
  include $(OCAMLMAKEFILE)
  
  (* Put the following in a file called "META" and edit to taste: *)
  
  name = "mylibrary"
  version = "1.0.0"
  description = "My library"
  requires = "pcre"
  archive(byte) = "mylibrary.cma"
  archive(native) = "mylibrary.cmxa"
  
  (* Now you can build bytecode and native libraries with "make" and
     install them into the standard library location with "make install".
     If you make a change, you will have to "make uninstall" before you
     can "make install" again. Once a library is installed, it's simple
     to use: *)
  
  $ ledit ocaml
          Objective Caml version 3.10.2
  
  # #use "topfind";;
  - : unit = ()
  Findlib has been successfully loaded. Additional directives:
    #require "package";;      to load a package
    #list;;                   to list the available packages
    #camlp4o;;                to load camlp4 (standard syntax)
    #camlp4r;;                to load camlp4 (revised syntax)
    #predicates "p,q,...";;   to set these predicates
    Topfind.reset();;         to force that packages will be reloaded
    #thread;;                 to enable threads
  
  - : unit = ()
  # #require "mylibrary";;
  /usr/lib/ocaml/3.10.2/pcre: added to search path
  /usr/lib/ocaml/3.10.2/pcre/pcre.cma: loaded
  /usr/local/lib/ocaml/3.10.2/mylibrary: added to search path
  /usr/local/lib/ocaml/3.10.2/mylibrary/mylibrary.cma: loaded
  
  (* To compile against your new library, use the "ocamlfind" tool as a
     front-end to "ocamlc" and "ocamlopt": *)
  
  $ ocamlfind ocamlc -package mylibrary myprogram.ml -o myprogram
  $ ocamlfind ocamlopt -package mylibrary myprogram.ml -o myprogram
  

