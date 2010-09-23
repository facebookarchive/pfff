(* ********************************************************************** *)
(* Matching Words *)
(* ********************************************************************** *)
let pleac_Matching_Words () = 
  #load "str.cma";;
  
  (* Str's regexps lack a whitespace-matching pattern.
     Here is a substitute. *)
  let whitespace_chars =
    String.concat ""
      (List.map (String.make 1)
         [
           Char.chr 9;  (* HT *)
           Char.chr 10; (* LF *)
           Char.chr 11; (* VT *)
           Char.chr 12; (* FF *)
           Char.chr 13; (* CR *)
           Char.chr 32; (* space *)
         ])
  let space = "[" ^ whitespace_chars ^ "]"
  let non_space = "[^" ^ whitespace_chars ^ "]"
  
  (* as many non-whitespace characters as possible *)
  let regexp = Str.regexp (non_space ^ "+")
  
  (* as many letters, apostrophes, and hyphens *)
  let regexp = Str.regexp "[A-Za-z'-]+"
  
  (* usually best *)
  let regexp = Str.regexp "\\b\\([A-Za-z]+\\)\\b"
  
  (* fails at ends or w/ punctuation *)
  let regexp = Str.regexp (space ^ "\\([A-Za-z]+\\)" ^ space)
  

