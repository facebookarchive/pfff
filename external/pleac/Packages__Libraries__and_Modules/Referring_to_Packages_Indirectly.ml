(* ********************************************************************** *)
(* Referring to Packages Indirectly *)
(* ********************************************************************** *)
let pleac_Referring_to_Packages_Indirectly () = 
  (* Generally, it is best to use tables of functions, possibly with
     Dynlink, to delay the choice of module and function until runtime.
     It is however possible--though inelegant--to (ab)use the toplevel
     for this purpose. *)
  
  open Printf
  
  (* Toplevel evaluator. Not type-safe. *)
  let () = Toploop.initialize_toplevel_env ()
  let eval text = let lexbuf = (Lexing.from_string text) in
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    ignore (Toploop.execute_phrase false Format.std_formatter phrase)
  let get name = Obj.obj (Toploop.getvalue name)
  let set name value = Toploop.setvalue name (Obj.repr value)
  
  (* Some module and value names, presumably not known until runtime. *)
  let modname = "Sys"
  let varname = "ocaml_version"
  let aryname = "argv"
  let funcname = "getenv"
  
  (* Use the toplevel to evaluate module lookups dynamically. *)
  let () =
    eval (sprintf "let (value : string) = %s.%s;;" modname varname);
    print_endline (get "value");
    eval (sprintf "let (values : string array) = %s.%s;;" modname aryname);
    Array.iter print_endline (get "values");
    eval (sprintf "let (func : string -> string) = %s.%s;;" modname funcname);
    print_endline ((get "func") "HOME");
  

