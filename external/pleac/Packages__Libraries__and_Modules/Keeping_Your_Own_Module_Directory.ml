(* ********************************************************************** *)
(* Keeping Your Own Module Directory *)
(* ********************************************************************** *)
let pleac_Keeping_Your_Own_Module_Directory () = 
  (* To add a directory to the module include path, pass the "-I" option
     to any of the compiler tools. For example, if you have a module in
     ~/ocamllib called Utils with a filename of utils.cmo, you can build
     against this module with the following: *)
  
  $ ocamlc -I ~/ocamllib utils.cmo test.ml -o test
  
  (* Within the toplevel interpreter, and from ocaml scripts, you can use
     the "#directory" directive to add directories to the include path: *)
  
  #directory "/home/myuser/ocamllib";;
  #load "utils.cmo";;
  
  (* In both cases, prefixing the include directory with a '+' indicates
     that the directory should be found relative to the standard include
     path. *)
  
  #directory "+pcre";;
  #load "pcre.cma";;
  
  (* If you have findlib installed, you can print out the include path by
     typing "ocamlfind printconf path" at the command line. *)
  
  $ ocamlfind printconf path
  /usr/local/lib/ocaml/3.10.2
  /usr/lib/ocaml/3.10.2
  /usr/lib/ocaml/3.10.2/METAS
  
  (* Instead of keeping a directory of ".cmo" (or ".cmx") files, you may
     prefer to build a library (".cma" for bytecode, ".cmxa" for native).
     This will pack all of your modules into a single file that is easy to
     use during compilation: *)
  
  $ ocamlc -a slicer.cmo dicer.cmo -o tools.cma
  $ ocamlc tools.cma myprog.ml -o myprog
  

