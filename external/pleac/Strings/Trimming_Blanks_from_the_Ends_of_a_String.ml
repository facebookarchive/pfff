(* ********************************************************************** *)
(* Trimming Blanks from the Ends of a String *)
(* ********************************************************************** *)
let pleac_Trimming_Blanks_from_the_Ends_of_a_String () = 
  
  let trim s =
    let s' = Str.replace_first (Str.regexp "^[ \t\n]+") "" s in
    Str.replace_first (Str.regexp "[ \t\n]+$") "" s';;
  
  let chop s =
    if s = "" then s else String.sub s 0 (String.length s - 1);;
  
  let chomp ?(c='\n') s =
    if s = "" then s else
      let len = String.length s - 1 in
      if s.[len] = c then String.sub s 0 len else s;;
  

