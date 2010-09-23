(* ********************************************************************** *)
(* Reading a String from a Binary File *)
(* ********************************************************************** *)
let pleac_Reading_a_String_from_a_Binary_File () = 
  let () =
    let in_channel = open_in_bin file in
    seek_in in_channel addr;
    let buffer = Buffer.create 0 in
    let ch = ref (input_char in_channel) in
    while !ch <> '\000' do
      Buffer.add_char buffer !ch;
      ch := input_char in_channel;
    done;
    close_in in_channel;
    let string = Buffer.contents buffer in
    print_endline string
  
  (*-----------------------------*)
  
  (* bgets - get a string from an address in a binary file *)
  open Printf
  
  let file, addrs =
    match Array.to_list Sys.argv with
      | _ :: file :: addrs when List.length addrs > 0 -> file, addrs
      | _ -> eprintf "usage: %s file addr ...\n" Sys.argv.(0); exit 0
  
  let () =
    let in_channel = open_in_bin file in
    List.iter
      (fun addr ->
         let addr = int_of_string addr in
         seek_in in_channel addr;
         let buffer = Buffer.create 0 in
         let ch = ref (input_char in_channel) in
         while !ch <> '\000' do
           Buffer.add_char buffer !ch;
           ch := input_char in_channel;
         done;
         printf "%#x %#o %d \"%s\"\n"
           addr addr addr (Buffer.contents buffer))
      addrs;
    close_in in_channel
  
  (*-----------------------------*)
  
  (* strings - pull strings out of a binary file *)
  #load "str.cma";;
  
  let find_strings =
    let pat = "[\040-\176\r\n\t ]" in
    let regexp = Str.regexp (pat ^ pat ^ pat ^ pat ^ "+") in
    fun f input ->
      List.iter
        (function Str.Delim string -> f string | _ -> ())
        (Str.full_split regexp input)
  
  let file =
    try Sys.argv.(1)
    with Invalid_argument _ ->
      Printf.eprintf "usage: %s file\n" Sys.argv.(0);
      exit 0
  
  let () =
    let in_channel = open_in_bin file in
    try
      while true do
        let buffer = Buffer.create 0 in
        let ch = ref (input_char in_channel) in
        while !ch <> '\000' do
          Buffer.add_char buffer !ch;
          ch := input_char in_channel;
        done;
        find_strings print_endline (Buffer.contents buffer)
      done
    with End_of_file ->
      close_in in_channel
  

