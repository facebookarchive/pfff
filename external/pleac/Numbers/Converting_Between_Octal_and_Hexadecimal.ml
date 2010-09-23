(* ********************************************************************** *)
(* Converting Between Octal and Hexadecimal *)
(* ********************************************************************** *)
let pleac_Converting_Between_Octal_and_Hexadecimal () = 
  (* Since integers and strings are very different things in OCaml, we will
     represent both octal and hexidecimal values as strings *)
  
  let oct_of_hex h =
    Printf.sprintf "%0o" (int_of_string ("0x" ^ h));;
  let hex_of_oct o =
    Printf.sprintf "%0x" (int_of_string ("0o" ^ o));;
  
  (* One small problem is that OCaml integers are 31 (or 63) bit values, if you need
     something larger, you can use the following for a full 32 bits: *)
  let oct_of_hex32 h =
    Printf.sprintf "%0lo" (Int32.of_string ("0x" ^ h));;
  let hex_of_oct32 o =
    Printf.sprintf "%0lx" (Int32.of_string ("0o" ^ o));;
  
  (* Or this for 64 bits: *)
  let oct_of_hex64 h =
    Printf.sprintf "%0Lo" (Int64.of_string ("0x" ^ h));;
  let hex_of_oct64 o =
    Printf.sprintf "%0Lx" (Int64.of_string ("0o" ^ o));;
  
  (* For anything else you have to roll your own *)
  let chopn n s =
    (* Chops strings into list of n byte substrings *)
    match s with 
      "" -> [""] (* avoids wierd edge case *)
      | _ ->
        let ex = (String.length s) mod n in
        let ss = if ex = 0 then s else ((String.make (n-ex) '0') ^ s) in
        let rec schopn x s l =
          match x with
            0 -> (String.sub s 0 n)::l
            | _ -> schopn (x-n) s ((String.sub s x n)::l) in
        schopn (String.length ss - n) ss [];;
          
  let long_oct_of_hex h =
    let choppedH = chopn 6 h in
    let f x = int_of_string ("0x" ^ x) in
    String.concat "" (List.map (fun x -> Printf.sprintf "%08o" (f x)) choppedH);;
  
  let long_hex_of_oct o =
    let choppedO = chopn 8 o in
    let f x = int_of_string ("0o" ^ x) in
    String.concat "" (List.map (fun x -> Printf.sprintf "%06x" (f x)) choppedO);;
  (*-----------------------------*)
  (* Since octal, hex and decimal are all the same internally, we don't need to do
      any explicit conversion *)
  printf "Gimme a number in decimal, octal, or hex: ";;
  let num = read_int ();;
  printf "%d %x %o\n" num num num;;
  (*-----------------------------*)
  printf "Enter file permission in octal: ";;
  let permissions = try read_int ()
  with Failure message -> failwith "Exiting...\n";;
  printf "The decimal value is %d\n" permissions;;
  

