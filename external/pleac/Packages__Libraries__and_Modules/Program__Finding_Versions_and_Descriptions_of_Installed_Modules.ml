(* ********************************************************************** *)
(* Program: Finding Versions and Descriptions of Installed Modules *)
(* ********************************************************************** *)
let pleac_Program__Finding_Versions_and_Descriptions_of_Installed_Modules () = 
  (* Use "findlib". You can use the "ocamlfind" program to get a list of
     installed libraries from the command line: *)
  
  $ ocamlfind list
  benchmark           (version: 0.6)
  bigarray            (version: [distributed with Ocaml])
  cairo               (version: n/a)
  cairo.lablgtk2      (version: n/a)
  calendar            (version: 2.0.2)
  camlimages          (version: 2.2.0)
  camlimages.graphics (version: n/a)
  camlimages.lablgtk2 (version: n/a)
  camlp4              (version: [distributed with Ocaml])
  camlp4.exceptiontracer (version: [distributed with Ocaml])
  camlp4.extend       (version: [distributed with Ocaml])
  ...
  
  (* You can also use the "#list" directive from the interpreter: *)
  
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
  # #list;;
  benchmark           (version: 0.6)
  bigarray            (version: [distributed with Ocaml])
  cairo               (version: n/a)
  cairo.lablgtk2      (version: n/a)
  ...
  
  

