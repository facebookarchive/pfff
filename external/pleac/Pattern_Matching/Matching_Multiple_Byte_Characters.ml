(* ********************************************************************** *)
(* Matching Multiple-Byte Characters *)
(* ********************************************************************** *)
let pleac_Matching_Multiple_Byte_Characters () = 
  #load "str.cma";;
  
  (* Regexp text for an EUC-JP character *)
  let eucjp =
    (String.concat "\\|"
       (* EUC-JP encoding subcomponents: *)
       [
         (* ASCII/JIS-Roman (one-byte/character) *)
         "[\x00-\x7F]";
  
         (* half-width katakana (two bytes/char) *)
         "\x8E[\xA0-\xDF]";
  
         (* JIS X 0212-1990 (three bytes/char) *)
         "\x8F[\xA1-\xFE][\xA1-\xFE]";
  
         (* JIS X 0208:1997 (two bytes/char) *)
         "[\xA1-\xFE][\xA1-\xFE]";
       ])
  
  (* Match any number of EUC-JP characters preceding Tokyo *)
  let regexp = Str.regexp ("\\(\\(" ^ eucjp ^ "\\)*\\)\\(\xC5\xEC\xB5\xFE\\)")
  
  (* Search from the beginning for a match *)
  let () =
    if Str.string_match regexp string 0
    then print_endline "Found Tokyo"
  
  (* Replace Tokyo with Osaka *)
  let () =
    let buffer = Buffer.create (String.length string) in
    let start = ref 0 in
    while Str.string_match regexp string !start do
      Buffer.add_string buffer (Str.matched_group 1 string);
      Buffer.add_string buffer osaka; (* Assuming osaka is defined *)
      start := Str.match_end ();
    done;
    if !start < String.length string
    then Buffer.add_substring buffer string
      !start (String.length string - !start);
    print_endline (Buffer.contents buffer)
  
  (* Split a multi-byte string into characters *)
  let () =
    (* One character per list element *)
    let chars =
      Array.map
        (function
           | Str.Delim c -> c
           | Str.Text c -> failwith ("invalid char: " ^ c))
        (Array.of_list
           (Str.full_split
              (Str.regexp eucjp)
              string)) in
    let length = Array.length chars in
    for i = 0 to length - 1 do
      if String.length chars.(i) = 1 then
        begin
          (* Do something interesting with this one-byte character *)
        end
      else
        begin
          (* Do something interesting with this multi-byte character *)
        end
    done;
    (* Glue list back together *)
    let line = String.concat "" (Array.to_list chars) in
    print_endline line
  
  (* Determine if an entire string is valid EUC-JP *)
  let is_eucjp s =
    Str.string_match
      (Str.regexp ("\\(" ^ eucjp ^ "\\)*$")) s 0
  
  (* Assuming a similar string has been defined for Shift-JIS *)
  let is_sjis s =
    Str.string_match
      (Str.regexp ("\\(" ^ sjis ^ "\\)*$")) s 0
  
  (* Convert from EUC-JP to Unicode, assuming a Hashtbl named
     euc2uni is defined with the appropriate character mappings *)
  let () =
    let chars =
      Array.map
        (function
           | Str.Delim c -> c
           | Str.Text c -> failwith ("invalid char: " ^ c))
        (Array.of_list
           (Str.full_split
              (Str.regexp eucjp)
              string)) in
    let length = Array.length chars in
    for i = 0 to length - 1 do
      if Hashtbl.mem euc2uni chars.(i)
      then
        begin
          chars.(i) <- (Hashtbl.find euc2uni chars.(i))
        end
      else
        begin
          (* deal with unknown EUC->Unicode mapping here *)
        end
    done;
    let line = String.concat "" (Array.to_list chars) in
    print_endline line
  

