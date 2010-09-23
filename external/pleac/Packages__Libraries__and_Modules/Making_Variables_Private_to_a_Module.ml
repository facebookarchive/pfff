(* ********************************************************************** *)
(* Making Variables Private to a Module *)
(* ********************************************************************** *)
let pleac_Making_Variables_Private_to_a_Module () = 
  #load "str.cma";;
  module Flipper :
  sig
    val flip_boundary : string -> string
    val flip_words : string -> string
  end =
  struct
    let separatrix = ref " "  (* hidden by signature *)
    let flip_boundary sep =
      let prev_sep = !separatrix in
      separatrix := sep;
      prev_sep
    let flip_words line =
      let words = Str.split (Str.regexp_string !separatrix) line in
      String.concat !separatrix (List.rev words)
  end
  

