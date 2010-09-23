(* ********************************************************************** *)
(* Accessing Substrings *)
(* ********************************************************************** *)
let pleac_Accessing_Substrings () = 
  let value = String.sub string offset count
  let value = String.sub string offset (String.length string - offset)
  (* or *)
  let value = sub_end string offset
  (* using *)
  let sub_end string offset = String.sub string offset (String.length string - offset)
  
  (*-----------------------------*)
  (* get a 5-byte string, skip 3, then grab 2 8-byte strings, then the rest*)
  
  (* split at 'sz' byte boundaries *)
  let rec split_every_n_chars sz = function
    | "" -> []
    | s -> 
        try
  	let (beg, rest) = String.sub s 0 sz, sub_end s sz in
  	beg :: split_every_n_chars sz rest
        with _ -> [s]
  
  let fivers = split_every_n_chars 5 string
  
  (* chop string into individual characters *)
  let chars = List.map (fun x -> x.[0]) (split_every_n_chars 1 string)
  
  (*-----------------------------*)
  let string = "This is what you have";;
  (* Indexes are left to right. There is no possibility to index *)
  (* directly from right to left *)
  (* "T" *)
  let first  = String.sub string 0 1   
  (* "is" *)
  let start  = String.sub string 5 2 
  (* "you have" *)
  let rest   = String.sub string 13 (String.length string - 13)   
  (* "e" *)
  let last   = String.sub string (String.length string - 1) 1
  (* "have" *)
  let theend = String.sub string (String.length string - 4) 4  
  (* "you" *)
  let piece  = String.sub string (String.length string - 8) 3
  (*-----------------------------*)
  let string = "This is what you have";;
  Printf.printf "%s" string ;
  (*This is what you have*)
  
  (* Change "is" to "wasn't"*)
  let string = (String.sub string 0 5) ^ "wasn't" ^ sub_end string 7
  (*This wasn't what you have *)   
  
  (*This wasn't wonderous *)
  let string = (String.sub string 0 (String.length string -12)) ^
     "ondrous";;
  
  (* delete first character *)
  let string = String.sub string 1 (String.length string - 1)
  (*his wasn't wondrous*)
  
  (* delete last 10 characters *)
  let string = String.sub string 0 (String.length string -10)
  (*his wasn'*)
  (*-----------------------------*)
  

