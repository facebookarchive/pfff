(* ********************************************************************** *)
(* Indenting Here Documents *)
(* ********************************************************************** *)
let pleac_Indenting_Here_Documents () = 
  #load "str.cma";;
  let var =  Str.global_replace (Str.regexp "^[\t ]+") "" "\
      your text
      goes here
  ";;
  

