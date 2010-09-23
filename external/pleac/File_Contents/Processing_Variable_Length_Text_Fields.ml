(* ********************************************************************** *)
(* Processing Variable-Length Text Fields *)
(* ********************************************************************** *)
let pleac_Processing_Variable_Length_Text_Fields () = 
  (* given "record" with field separated by "pattern",
     extract "fields". *)
  #load "str.cma";;
  let regexp = Str.regexp pattern
  let fields = Str.split_delim regexp record
  
  (* same as above using PCRE library, available at:
     http://www.ocaml.info/home/ocaml_sources.html#pcre-ocaml *)
  #directory "+pcre";;
  #load "pcre.cma";;
  let fields = Pcre.split ~pat:pattern record
  
  (*-----------------------------*)
  
  # Str.full_split (Str.regexp "[+-]") "3+5-2";;
  - : Str.split_result list =
  [Str.Text "3"; Str.Delim "+"; Str.Text "5"; Str.Delim "-"; Str.Text "2"]
  
  # Pcre.split ~pat:"([+-])" "3+5-2";;
  - : string list = ["3"; "+"; "5"; "-"; "2"]
  
  (*-----------------------------*)
  
  let fields = Str.split_delim (Str.regexp ":") record
  let fields = Str.split_delim (Str.regexp "[ \n\r\t]+") record
  let fields = Str.split_delim (Str.regexp " ") record
  
  let fields = Pcre.split ~pat:":" record
  let fields = Pcre.split ~pat:"\\s+" record
  let fields = Pcre.split ~pat:" " record
  

