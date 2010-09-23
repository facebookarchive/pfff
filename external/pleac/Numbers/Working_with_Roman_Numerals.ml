(* ********************************************************************** *)
(* Working with Roman Numerals *)
(* ********************************************************************** *)
let pleac_Working_with_Roman_Numerals () = 
  (* Based on Groovy version by Paul King. *)
  
  let roman_map =
    [1000, "M"; 900, "CM"; 500, "D"; 400, "CD"; 100, "C"; 90, "XC";
     50,   "L"; 40,  "XL"; 10,  "X"; 9,   "IX"; 5,   "V"; 4,  "IV"; 1, "I"]
  
  let roman arabic =
    let rec loop remains text map =
      match map with
        | (key, value) :: rest ->
            if remains >= key
            then loop (remains - key) (text ^ value) map
            else loop remains text rest
        | [] -> text in
    loop arabic "" roman_map
  
  let arabic roman =
    let rec loop text sum map =
      match map with
        | (key, value) :: rest ->
            if (String.length text >= String.length value
                && String.sub text 0 (String.length value) = value)
            then (loop
                    (String.sub
                       text
                       (String.length value)
                       (String.length text - String.length value))
                    (sum + key)
                    map)
            else loop text sum rest
        | [] -> sum in
    loop (String.uppercase roman) 0 roman_map
  
  (*-----------------------------*)
  
  (* Alternative version by Ken Wakita. *)
  let roman arabic =
    let nstr s n = String.concat "" (Array.to_list (Array.make n s)) in
    snd (List.fold_left
           (fun (arabic, roman) (arab, rom) ->
             arabic mod arab, roman ^ (nstr rom (arabic / arab)))
           (arabic, "")
           roman_map)
  
  (*-----------------------------*)
  
  let () =
    let roman_fifteen = roman 15 in
    Printf.printf "Roman for fifteen is %s\n" roman_fifteen;
    let arabic_fifteen = arabic roman_fifteen in
    Printf.printf "Converted back, %s is %d\n" roman_fifteen arabic_fifteen
  
  (* Roman for fifteen is XV
     Converted back, XV is 15 *)
  

