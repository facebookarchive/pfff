(* ********************************************************************** *)
(* Expanding and Compressing Tabs *)
(* ********************************************************************** *)
let pleac_Expanding_and_Compressing_Tabs () = 
  
  let expand_tabs ?(spaces = 8) s =
    Str.global_replace (Str.regexp "\t") (String.make spaces ' ') s;;
  
  let compress_tabs ?(spaces = 8) s = 
    Str.global_replace (Str.regexp (String.make spaces ' ')) "\t" s;;
  
  (*
  # let st = "\tyo baby!\n\t\tWhat the shizzle?\t(Mack)";;
  val st : string = "\tyo baby!\n\t\tWhat the shizzle?\t(Mack)"
  # let etst = expand_tabs st;;
  val etst : string =
    "        yo baby!\n                What the shizzle?        (Mack)"
  # let etst = expand_tabs ~spaces:4 st;;
  val etst : string = "    yo baby!\n        What the shizzle?    (Mack)"
  # let etst = expand_tabs ~spaces:8 st;;
  val etst : string =
    "        yo baby!\n                What the shizzle?        (Mack)"
  # let rest = compress_tabs etst;;
  val rest : string = "\tyo baby!\n\t\tWhat the shizzle?\t(Mack)"
  # let rest = compress_tabs ~spaces:4 etst;;
  val rest : string = "\t\tyo baby!\n\t\t\t\tWhat the shizzle?\t\t(Mack)"
  # let rest = compress_tabs ~spaces:3 etst;;
  val rest : string =
    "\t\t  yo baby!\n\t\t\t\t\t What the shizzle?\t\t  (Mack)"
  *)
  

