(* ********************************************************************** *)
(* Checking Whether a String Is a Valid Number *)
(* ********************************************************************** *)
let pleac_Checking_Whether_a_String_Is_a_Valid_Number () = 
  
  (* Something like this must be done differently in OCaml because of its
  * type-safety.  Some of the tests will use regular expressions, but most won't *)
  let has_NonDigits s = 
    try ignore (search_forward (regexp "[^0-9]") s); true
    with Not_found -> true;;
  let is_NaturalNumber s =
    try let n = int_of_string s in n > 0 with Failure _ -> false;;
  let is_Integer s =
    try ignore(int_of_string s); true with Failure _ -> false;;
  let is_DecimalNumber s =
    try ignore(int_of_string s); true with Failure _ ->
      try let n = float_of_string s in (abs_float f) >= 1. 
      with Failure _ -> false;;
  let is_CFloat s = 
    try ignore(float_of_string s); true 
    with Failure _ -> false;;
  
  (* One of the above predicates can then be used as needed *)
  if predicate s then
    (* is a number *)
  else
    (* is not a number *)
  

