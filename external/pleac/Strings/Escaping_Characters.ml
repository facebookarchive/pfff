(* ********************************************************************** *)
(* Escaping Characters *)
(* ********************************************************************** *)
let pleac_Escaping_Characters () = 
  (*
  ** The Str module is deistributed with the standard Ocaml compiler
  ** suit but it is not automatically pulled in by the command line
  ** interpreter or the compilers.
  **
  ** The "#load" line is only needed if you are running this in the 
  ** command interpretter.
  **
  ** If you are using either of the ocaml compilers, you will need 
  ** to remove the "#load" line and link in str.cmxa in the final 
  ** compile command.
  *)
  
  #load "str.cma" ;;
  
  open Str
  
  let escape charlist str =
  	let rx = Str.regexp ("\\([" ^ charlist ^ "]\\)") in
  	Str.global_replace rx "\\\\\\1" str
  
  let text = "Mom said, \"Don't do that.\"" ;;
  print_endline text ;;
  
  let text = escape "'\"" text ;;
  print_endline text ;;
  

