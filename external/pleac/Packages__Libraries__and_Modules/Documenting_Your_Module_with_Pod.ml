(* ********************************************************************** *)
(* Documenting Your Module with Pod *)
(* ********************************************************************** *)
let pleac_Documenting_Your_Module_with_Pod () = 
  (** Documentation for OCaml programs can be generated with the ocamldoc
      tool, included in the standard distribution. Special comments like
      this one begin with two asterisks which triggers ocamldoc to
      include them in the documentation. The first special comment in a
      module becomes the main description for that module. *)
  
  (** Comments can be placed before variables... *)
  val version : string
  
  (** ...functions... *)
  val cons : 'a -> 'a list -> 'a list
  
  (** ...types... *)
  type choice = Yes | No | Maybe of string
  
  (* ... and other constructs like classes, class types, modules, and
     module types. Simple comments like this one are ignored. *)
  
  (** {2 Level-two headings look like this} *)
  
  (** Text in [square brackets] will be formatted using a monospace font,
      ideal for identifiers and other bits of code. Text written in curly
      braces with a bang in front {!Like.this} will be hyperlinked to the
      corresponding definition. *)
  
  (* To generate HTML documentation, use a command like the following: *)
  
  $ ocamldoc -html -d destdir Module1.mli Module1.ml ...
  
  (* To generate Latex documentation, use a command like the following: *)
  
  $ ocamldoc -latex -d destdir Module1.mli Module1.ml ...
  
  (* If you use OCamlMakefile, you can type "make doc" to build HTML and
     PDF documentation for your entire project. You may want to customize
     the OCAMLDOC and DOC_FILES variables to suit your needs. *)
  

