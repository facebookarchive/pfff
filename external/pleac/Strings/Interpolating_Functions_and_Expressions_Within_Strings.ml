(* ********************************************************************** *)
(* Interpolating Functions and Expressions Within Strings *)
(* ********************************************************************** *)
let pleac_Interpolating_Functions_and_Expressions_Within_Strings () = 
  
  (* Again, because of OCaml's type-safe nature, actual interpolation cannot be
   * done inside of strings -- one must use either string concatenation or sprintf
   * to get the results we're looking for *)
  
  let phrase = "I have " ^ (string_of_int (n+1)) ^ " guanacos.";;
  let prhase = sprintf "I have %d guanacos." (n+1);;
  

