(* ********************************************************************** *)
(* Matching Letters *)
(* ********************************************************************** *)
let pleac_Matching_Letters () = 
  (* Str can do a simple character range match, but it isn't very
     practical for matching alphabetic characters in general. *)
  #load "str.cma";;
  let () =
    if Str.string_match (Str.regexp "^[A-Za-z]+$") var 0
    then print_endline "var is purely alphabetic"
  
  (* With Pcre, you can use UTF8 support and match characters with
     the letter property. *)
  #directory "+pcre";;
  #load "pcre.cma";;
  let () =
    if Pcre.pmatch ~rex:(Pcre.regexp ~flags:[`UTF8] "^\\pL+$") var
    then print_endline "var is purely alphabetic"
  

