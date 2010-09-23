(* ********************************************************************** *)
(* Converting Between Binary and Decimal *)
(* ********************************************************************** *)
let pleac_Converting_Between_Binary_and_Decimal () = 
  
  (*-----------------------------*)
  (* 
   * Two versions in each direction -- one to deal with decimal strings,
   * and the other to deal with decimal integers.  Binary numbers will
   * always be strings 
   *)
  
  let binStr_of_decInt i =
    let rec strip_bits i s =
      match i with
        0 -> s
      | _ -> strip_bits (i lsr 1) ((string_of_int (i land 0x01)) ^ s) in
    strip_bits i "";;
  
  let binStr_of_decStr i =
    let rec strip_bits i s =
      match i with
        0 -> s
      | _ -> strip_bits (i lsr 1) ((string_of_int (i land 0x01)) ^ s) in
    strip_bits (int_of_string i) "";;
  (* Of course if you have binStr_of_decInt already, it's easier to just call
     binStr_of_decInt (int_of_string i) *)
  
  (*-----------------------------*)
  let decInt_of_binStr s =
    int_of_string ("0b" ^ s);;
  
  let decStr_of_binStr s =
    string_of_int (int_of_string ("0b" ^ s));;
  (*-----------------------------*)
  let numInt = decInt_of_binStr "0110110";; (* numInt = 54 *)
  let numInt = decStr_of_binStr "0110110";; (* numInt = "54" *)
  let bin1 = binStr_of_decInt 54;;   (* bin1 = "110110" *)
  let bin2 = binStr_of_decStr "54";; (* bin2 = "110110" *)
  (*-----------------------------*)
  

