(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  (* When an OCaml source file is compiled, it becomes a module. The name
     of the module is the capitalized form of the filename. For example,
     if the source file is "my_module.ml", the module name is "My_module".
  
     Modules can also be created explicitly within a source file. If
     "my_module.ml" contains "module Foo = struct ... end", a module named
     "My_module.Foo" will be created.
  
     Here is an example of the definition and use of two modules within a
     single source file: *)
  
  module Alpha = struct
    let name = "first"
  end
  
  module Omega = struct
    let name = "last"
  end
  
  let () =
    Printf.printf "Alpha is %s, Omega is %s.\n"
      Alpha.name Omega.name
  
  (* Alpha is first, Omega is last. *)
  
  (*-----------------------------*)
  
  (* The "#use" and "#load" commands are known as toplevel directives.
     They can only be used while interacting with the interpreter or from
     scripts that are run using the "ocaml" program. *)
  
  (* "#use" loads a source file into the current scope. *)
  #use "FileHandle.ml";;
  
  (* "#load" loads a module from a compiled bytecode file. This has the
     same effect as including this file during bytecode compilation. *)
  #load "FileHandle.cmo";;
  
  (* "#load" can be used with libraries as well as modules. Bytecode
     libraries use an extension of ".cma". *)
  #load "library.cma";;
  
  (* The "open" statement can be used in any source file. It allows any
     values defined within a module to be used without being prefixed by
     the module name. *)
  open FileHandle
  
  (* Modules form a hierarchy; submodules can be opened in a similar
     fashion by prefixing them with the parent module's name. *)
  open Cards.Poker
  
  (* It is often convenient to use Gerd Stolpmann's "findlib" system,
     which makes it considerably easier to load libraries into the
     interpreter. *)
  
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
  # #require "extlib";;
  /usr/lib/ocaml/3.10.2/extlib: added to search path
  /usr/lib/ocaml/3.10.2/extlib/extLib.cma: loaded
  
  (* The above use of "#require" has the same effect as typing the
     following: *)
  
  #directory "+extlib";;
  #load "extLib.cma";;
  
  (* More information on the "findlib" system is available here:
     http://projects.camlcity.org/projects/findlib.html
  
     The "#directory" directive above is built into OCaml and allows
     you to add additional directories to the path that is searched
     when loading modules. You can use a prefix of '+' to indicate that
     the directory is under the standard library path, which is usually
     something like "/usr/lib/ocaml/3.10.2/".
  
     Modules can be easily aliased using assignment. This will also
     cause the interpreter to output the module's signature, which
     can be used as a quick reference. *)
  
  # module S = ExtString.String;;
  module S :
    sig
      val init : int -> (int -> char) -> string
      val find : string -> string -> int
      val split : string -> string -> string * string
      val nsplit : string -> string -> string list
      val join : string -> string list -> string
      ...
    end
  # S.join;;
  - : string -> string list -> string = <fun>
  
  (* Many useful libraries can be found at The Caml Hump:
     http://caml.inria.fr/cgi-bin/hump.cgi *)
  

