(* ********************************************************************** *)
(* Converting Between ASCII Characters and Values *)
(* ********************************************************************** *)
let pleac_Converting_Between_ASCII_Characters_and_Values () = 
  (*-----------------------------*)
  let num  = Char.code char
  let char = Char.chr num
  (*-----------------------------*)
  (* char and int are distinct datatypes in OCaml *)
  printf "Number %d is character %c\n" num (Char.chr num)
  (* Number 101 is character e *)
  (*-----------------------------*)
  (* convert string to list of chars *)
  let explode s =
    let rec f acc = function
      | -1 -> acc
      | k -> f (s.[k] :: acc) (k - 1)
    in f [] (String.length s - 1)
  
  (* convert list of chars to string *)
  let implode l =
    let s = String.create (List.length l) in
    let rec f n = function
      | x :: xs -> s.[n] <- x; f (n + 1) xs
      | [] -> s
    in f 0 l
  
  (* ascii is list of ints. *)
  let ascii = List.map Char.code (explode string)
  let string = implode (List.map Char.ord ascii)
  (*-----------------------------*)
  let ascii_value = Char.code 'e'    (* now 101 *)
  let character   = Char.chr 101     (* now 'e' *)
  (*-----------------------------*)
  printf "Number %d is character %c\n" 101 (Char.chr 101)
  (*-----------------------------*)
  let ascii_character_numbers = List.map Char.code (explode "sample");;
  List.iter (printf "%d ") ascii_character_numbers;
  printf "\n"
  115 97 109 112 108 101
  
  let word = implode (List.map Char.chr ascii_character_numbers)
  let word = implode (List.map Char.chr [115; 97; 109; 112; 108; 101]);; (* same *)
  printf "%s\n" word
  sample
  (*-----------------------------*)
  let hal = "HAL"
  let ascii = List.map Char.code (explode hal)
  let ascii = List.map (( + ) 1) ascii  (* add one to each ASCII value *)
  let ibm = implode (List.map Char.chr ascii);;
  printf "%s\n" ibm             (* prints "IBM" *)
  (*-----------------------------*)
  

