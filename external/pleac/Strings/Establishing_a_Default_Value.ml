(* ********************************************************************** *)
(* Establishing a Default Value *)
(* ********************************************************************** *)
let pleac_Establishing_a_Default_Value () = 
  
  (* Because OCaml doesn't have the same notion of truth or definedness as Perl,
   * most of these examples just can't be done as they are in Perl.  Some can be
   * approximated via the use of options, but remember, unbound variables are not
   * automatically assigned the value of None -- the variable has to have been
   * explicitly bound to None (or Some x) beforehand.
  *)
  
  (* use b if b is not None, else use c *)
  let a = match b with None -> c | _ -> b;;
  
  (* set x to y if x is currently None *)
  let x = match x with None -> y | _ -> x;;
  
  (* Note that these are much closer to Perls notion of definedness than truth *)
  
  (* We can set foo to either bar or "DEFAULT VALUE" in one of two ways *)
  (* keep foo as a string option *)
  let foo = match bar with Some x -> bar | _ -> Some "DEFAULT VALUE";;
  
  (* Use foo as a string *)
  let foo = match bar with Some x -> x | _ -> "DEFAULT VALUE";;
  
  let dir = if Array.length Sys.argv > 1 then argv.(1) else "/tmp";;
  
  (* None of the other examples really make sense in OCaml terms... *)
  

