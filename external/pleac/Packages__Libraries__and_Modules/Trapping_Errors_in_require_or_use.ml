(* ********************************************************************** *)
(* Trapping Errors in require or use *)
(* ********************************************************************** *)
let pleac_Trapping_Errors_in_require_or_use () = 
  (* Due to static typing, missing modules are detected at compilation
     time, so this is not normally an error you can catch (or need to).
     When using ocaml interactively or as an interpreter, the "#load"
     directive can fail, resulting in a message like the following:
  
         Cannot find file <filename>.
  
     being printed to standard output. This is also not an error you can
     catch, and its occurrence will not stop the script from executing.
     It is possible to dynamically load modules and detect the failure of
     this operation with Dynlink. An example is given in the next recipe. *)
  

