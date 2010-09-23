(* ********************************************************************** *)
(* Generating Attribute Methods Using AUTOLOAD *)
(* ********************************************************************** *)
let pleac_Generating_Attribute_Methods_Using_AUTOLOAD () = 
  (* Use Jacques Garrigue's pa_oo syntax extension, available at:
     http://www.math.nagoya-u.ac.jp/~garrigue/code/ocaml.html *)
  
  (*pp camlp4o pa_oo.cmo *)
  class person () = object (self)
    val mutable name = "" with accessor
    val mutable age = 0 with accessor
    val mutable parent = None with reader
    method spawn () = {< parent = Some self >}
  end
  
  let () =
    let dad = new person () in
    dad#name <- "Jason";
    dad#age <- 23;
    let kid = dad#spawn () in
    kid#name <- "Rachel";
    kid#age <- 2;
    Printf.printf "Kid's parent is %s\n"
      (match kid#parent with
         | Some parent -> parent#name
         | None -> "unknown")
  

