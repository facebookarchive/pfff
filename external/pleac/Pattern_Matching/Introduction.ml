(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  (* We will use the Str library distributed with OCaml for regular expressions.
   * There are two ways to use the str library, building a top or passing it to ocaml.
   * Under Unix, you can create a new toplevel which has the Str module:
   *      $ ocamlmktop -o strtop str.cma 
   *      $ ./strtop
   *   Now you don't need to prefix the contents of the str module with Str.
   * The alternative is to pass str.cma as a parameter:
   *     $ ocaml str.cma
   *   Now you may refer to the contents of the str module by using Str. 
   * Under Windows, if you are using ocamlwin.exe you can simply load Str:
   *      # load "str.cma";;
   *)
  (* Str.search_forward returns an int or throws an exception if the pattern isn't found.
   * In Perl, the =~ operator returns null. Since these two values have different
   * types in OCaml, we cannot copy this behaviour directly.
   * Instead, we return an impossible index, -1 using try ... with.
   * Another method would be to define an =~ operator and use that directly:
  # let (=~) s re = Str.string_match (Str.regexp re) s 0;; 
  val ( =~ ) : string -> string -> bool = <fun>
  # "abc" =~ "a";;
  - : bool = true
  # "bc" =~ "a";;
  - : bool = false
   * Don't underestimate the power of this. Many of the following examples could be 
   * simplified by defining infix operators.
   *)
  try Str.search_forward (Str.regexp pattern) string 0;
  with Not_found -> -1;;
  
  try Str.replace_first (Str.regexp pattern) replacement string;
  with Not_found -> "";;
  (*-----------------------------*)
  try (Str.search_forward (Str.regexp "sheep") meadow 0) > -1;
  with Not_found -> false;; (* true if meadow contains "sheep" *)
  
  try not ((Str.search_forward (Str.regexp "sheep") meadow 0) > -1);
  with Not_found -> true;; (* true if meadow doesn't contain "sheep" *)
  
  let meadow = 
      try Str.replace_first (Str.regexp "old") "new" meadow;
      with Not_found -> meadow;; (* Replace "old" with "new" in meadow *)
  (*-----------------------------*)
  try 
      let temp = Str.search_forward (Str.regexp "\\bovines?\\b") meadow 0 in
          print_string "Here be sheep!";
  with Not_found -> ();;
  (*-----------------------------*)
  let string = "good food" in
      try
          Str.replace_first (Str.regexp "o*") "e" string;
      with Not_found -> string;;
  (*-----------------------------*)
  (* There is no way to take command line parameters to ocaml that I know of. 
   * You would first have to compile your OCaml program using ocamlc.
   *)
  (*-----------------------------*)
  let rec match_num s start=
      if String.length s > 0 then
          try 
              let temp = Str.search_forward (Str.regexp "[0123456789]+") s start in
                  print_string (String.concat "" ("Found number " :: Str.matched_string s :: ["\n"]));
                  match_num s (temp + 1);
          with Not_found -> ();
      else
          ();;
  (*-----------------------------*)
  let rec match_group s start numbers=
      if String.length s > 0 then
          try 
              let temp = (Str.search_forward (Str.regexp "[0123456789]+") s start) in
                  let numbers = Str.matched_string s :: numbers in 
                      match_group s (temp + 1) numbers;
          with Not_found -> numbers;
      else
          numbers;;
  (*-----------------------------*)
  let (=+) s re = 
      let result = ref [] in
      let offset = ref 0 in
      while ((String.length s) > !offset) do
          try
              offset := 1 + (Str.search_forward (Str.regexp re) s !offset);
              result := !result @ [Str.matched_string s] @ [];
          with Not_found -> ignore (offset := String.length s)
      done;
      result;;
      
  let (=-) s re = 
      let result = ref [] in
      let offset = ref 0 in
      while ((String.length s) > !offset) do
          try
              ignore (Str.search_forward (Str.regexp re) s !offset);
              offset := Str.match_end ();
              result := !result @ [Str.matched_string s] @ [];
          with Not_found -> ignore (offset := String.length s)
      done;
      result;;
  
  let digits = "123456789";;
  let yeslap = digits =+ "[1234567890][1234567890][1234567890]";;
  let nonlap = digits =- "[1234567890][1234567890][1234567890]";;
  
  print_string "Non-overlapping: ";
  List.iter (fun v -> print_string (v ^ " ")) !nonlap;
  print_string "\n";;
  (* Non-overlapping: 123 456 789 *)
  
  print_string "Overlapping: ";
  List.iter (fun v -> print_string (v ^ " ")) !yeslap;
  print_string "\n";;
  (* Overlapping: 123 234 345 456 567 678 789 *)
  (*-----------------------------*)
  let index = ref 0;;
  let string = "And little lambs eat ivy";;
  try
      index := Str.search_forward (Str.regexp "l[^s]*s") string 0;
  with Not_found -> ();;
      
  print_string ("(" ^ (String.sub string 0 !index) ^ ") ");
  print_string ("(" ^ (Str.matched_string string) ^ ") ");
  print_string ("(" ^ (Str.string_after string 16) ^ ")\n");;
  (* (And ) (little lambs) ( eat ivy) *)
  

