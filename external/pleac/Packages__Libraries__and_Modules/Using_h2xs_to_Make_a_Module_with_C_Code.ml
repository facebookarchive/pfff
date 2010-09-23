(* ********************************************************************** *)
(* Using h2xs to Make a Module with C Code *)
(* ********************************************************************** *)
let pleac_Using_h2xs_to_Make_a_Module_with_C_Code () = 
  (* Building libraries with C code is much easier with the aid of
     OCamlMakefile. The following Makefile is all it takes to build
     the "time" library from the previous recipe: *)
  
  OCAMLMAKEFILE = OCamlMakefile
  
  RESULT = time
  SOURCES = time.idl
  NOIDLHEADER = yes
  
  all: byte-code-library native-code-library
  
  include $(OCAMLMAKEFILE)
  
  (* Now, a simple "make" will perform the code generation with camlidl
     and produce static and dynamic libraries for bytecode and native
     compilation. Furthermore, "make top" will build a custom toplevel
     interpreter called "time.top" with the Time module built in: *)
  
  $ ./time.top
          Objective Caml version 3.10.2
  
  # Time.gettimeofday None;;
  - : int * Time.timeval =
  (0, {Time.tv_sec = 1217483550l; Time.tv_usec = 645204l})
  
  (* With the addition of a "META" file combined with the "libinstall"
     and "libuninstall" targets, this library can be installed to the
     standard location for use in other projects. See recipe 12.8,
     "Preparing a Module for Distribution", for an example. *)
  

