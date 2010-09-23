(* ********************************************************************** *)
(* Processing Every Word in a File *)
(* ********************************************************************** *)
let pleac_Processing_Every_Word_in_a_File () = 
  let word_stream_of_channel channel =
    (* Thanks to Mac Mason for figuring this out. *)
    let buffer = (Scanf.Scanning.from_channel channel) in
    Stream.from
      (fun count ->
         try
           match Scanf.bscanf buffer " %s " (fun x -> x) with
             | "" -> None
             | s -> Some s
         with End_of_file ->
           None)
  
  (*-----------------------------*)
  
  let () =
    Stream.iter
      (fun chunk ->
         (* do something with chunk *)
         ())
      (word_stream_of_channel stdin)
  
  (*-----------------------------*)
  
  (* Make a word frequency count *)
  let seen = Hashtbl.create 0
  let () =
    Stream.iter
      (fun word ->
         Hashtbl.replace seen word
           (try Hashtbl.find seen word + 1
            with Not_found -> 1))
      (word_stream_of_channel stdin)
  
  (* output hash in a descending numeric sort of its values *)
  let () =
    let words = ref [] in
    Hashtbl.iter (fun word _ -> words := word :: !words) seen;
    List.iter
      (fun word ->
         Printf.printf "%5d %s\n" (Hashtbl.find seen word) word)
      (List.sort
         (fun a b -> compare (Hashtbl.find seen b) (Hashtbl.find seen a))
         !words)
  
  (*-----------------------------*)
  
  (* Line frequency count *)
  
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
  let seen = Hashtbl.create 0
  let () =
    Stream.iter
      (fun line ->
         Hashtbl.replace seen line
           (try Hashtbl.find seen line + 1
            with Not_found -> 1))
      (line_stream_of_channel stdin)
  
  let () =
    let lines = ref [] in
    Hashtbl.iter (fun line _ -> lines := line :: !lines) seen;
    List.iter
      (fun line ->
         Printf.printf "%5d %s\n" (Hashtbl.find seen line) line)
      (List.sort
         (fun a b -> compare (Hashtbl.find seen b) (Hashtbl.find seen a))
         !lines)
  

