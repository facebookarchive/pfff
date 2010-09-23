(* ********************************************************************** *)
(* Processing a String One Character at a Time *)
(* ********************************************************************** *)
let pleac_Processing_a_String_One_Character_at_a_Time () = 
  
  (* One can split a string into an array of character, or corresponding ASCII
   * codes as follows, but this is not necessary to process the strings a
   * character at a time: *)
  
  let array_of_chars = Array.init (String.length s) (fun i -> s.[i]);;
  let array_of_codes = Array.init (String.length s) (fun i -> Char.code s.[i]);;
  
  (* or one can just use String.iter *)
  String.iter 
    (fun i -> (*do something with s.[i], the ith char of the string*)) s;;
  
  (* The following function can be used to return a list of all unique keys in a
   * hashtable *)
  
  let keys h =
    let k = Hashtbl.fold (fun k v b -> k::b) h [] in
    (* filter out duplicates *)
    List.fold_left (fun b x -> if List.mem x b then b else x::b) [] k;;
  
  (* and this function is a shorthand for adding a key,value pair to a hashtable
  *)
  
  let ( <<+ ) h (k,v) = Hashtbl.add h k v;;
  
  let seen = Hashtbl.create 13;;
  let s = "an apple a day";;
  let array_of_chars = Array.init (String.length s) (fun i -> s.[i]);;
  Array.iter (fun x -> seen <<+ (x,1)) array_of_chars;
  print_string "unique chars are:\t";
  List.iter print_char (List.sort compare (keys seen));
  print_newline ();;
  
  (* or, without the unnecessary and innefficient step of converting the string
   * into an array of chars *)
  let seen = Hashtbl.create 13;;
  let s = "an apple a day";;
  String.iter (fun x -> seen <<+ (x,1)) s;
  print_string "unique chars are:\t";
  List.iter print_char (List.sort compare (keys seen));
  print_newline ();;
  
  (* To compute the simple 31-bit checksum of a string *)
  let cksum s =
    let sum = ref 0 in
    String.iter (fun x -> sum := !sum + (Char.code x)) s;
    !sum;;
  (*
  # cksum "an apple a day";;
  - : int = 1248
  *)
  
  (* to emulate the SysV 16-bit checksum, we will first write two routines sort of
   * similar to Perl's (<>), that will return the contents of a file either as a
   * list of strings or as a single string - not that the list of strings version
   * throws away the \n at the end of each line *)
  
  let slurp_to_list filename =
    let ic = open_in filename and
    l = ref [] in
    let rec loop () =
      let line = input_line ic in
      l := line::!l;
      loop () in
    try loop () with End_of_file -> close_in ic; List.rev !l;;
  
  let slurp_to_string filename =
    let ic = open_in filename and
    buf = Buffer.create 4096 in
    let rec loop () =
      let line = input_line ic in
      Buffer.add_string buf line;
      Buffer.add_string buf "\n";
      loop () in
    try loop () with End_of_file -> close_in ic; Buffer.contents buf;;
  
  let cksum16 fn =
    let addString sum s =
      let sm = ref sum in
      String.iter (fun c -> sm := !sm + (Char.code c)) (s ^ "\n"); 
      !sm mod 65537 (* 2^16 - 1 *)in
    List.fold_left addString 0 (slurp_to_list fn);;
  
  (* or *)
  let cksum16 fn = 
    let sum = ref 0
    and s = slurp_to_string fn in
    String.iter (fun c -> sum := (!sum + (Char.code c)) mod 65537) s;
    !sum;;
  
  
  
  (* Note: slowcat as written is meant to be run from the command line, not in the
   * toplevel *)
  
  #!/usr/local/bin/ocaml
  (* slowcat - emulate a   s l o w  line printer *)
  (* usage: slowcat [-DELAY] [files ...] *)
  #load "unix.cma";;
  
  (* make sure you have the code for the slurp_to_string function in this file as
   * well... *)
  
  let _ =
    let delay,fs = try (float_of_string Sys.argv.(1)),2 with Failure _ -> 1.,1 in
    let files = Array.sub Sys.argv fs (Array.length Sys.argv - fs) in
    let print_file f =
      let s = slurp_to_string f in
      String.iter 
        (fun c -> 
          print_char c;
          ignore(Unix.select [] [] [] (0.005 *. delay))) s in
    Array.iter print_file files;;
  

