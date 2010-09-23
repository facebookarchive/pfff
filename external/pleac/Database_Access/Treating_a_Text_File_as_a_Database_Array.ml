(* ********************************************************************** *)
(* Treating a Text File as a Database Array *)
(* ********************************************************************** *)
let pleac_Treating_a_Text_File_as_a_Database_Array () = 
  let with_lines_in_file name f =
    if not (Sys.file_exists name)
    then (let out_channel = open_out name in close_out out_channel);
  
    let in_channel = open_in name in
    let in_lines = ref [] in
    begin
      try
        while true do
          in_lines := input_line in_channel :: !in_lines
        done
      with End_of_file ->
        close_in in_channel
    end;
  
    let out_lines = f (List.rev !in_lines) in
    let out_channel = open_out name in
    List.iter
      (fun line ->
         output_string out_channel line;
         output_string out_channel "\n")
      out_lines;
    flush out_channel;
    close_out out_channel
  
  let () =
    (* first create a text file to play with *)
    with_lines_in_file "/tmp/textfile"
      (fun lines ->
         ["zero"; "one"; "two"; "three"; "four"]);
  
    with_lines_in_file "/tmp/textfile"
      (fun lines ->
         (* print the records in order. *)
         print_endline "ORIGINAL\n";
         Array.iteri (Printf.printf "%d: %s\n") (Array.of_list lines);
  
         (* operate on the end of the list *)
         let lines = List.rev lines in
         let a = List.hd lines in
         let lines = List.rev ("last" :: lines) in
         Printf.printf "\nThe last record was [%s]\n" a;
  
         (* and the beginning of the list *)
         let a = List.hd lines in
         let lines = "first" :: (List.tl lines) in
         Printf.printf "\nThe first record was [%s]\n" a;
  
         (* remove the record "four" *)
         let lines =
           List.filter (function "four" -> false | _ -> true) lines in
  
         (* replace the record "two" with "Newbie" *)
         let lines =
           List.map (function "two" -> "Newbie" | x -> x) lines in
  
         (* add a new record after "first" *)
         let lines =
           List.fold_right
             (fun x a ->
                if x = "first"
                then x :: "New One" :: a
                else x :: a)
             lines [] in
  
         (* now print the records in reverse order *)
         print_endline "\nREVERSE\n";
         List.iter print_string
           (List.rev
              (Array.to_list
                 (Array.mapi
                    (fun i line -> Printf.sprintf "%d: %s\n" i line)
                    (Array.of_list lines))));
  
         (* return the new list, which will be written back to the file *)
         lines)
  
  (*-----------------------------
  ORIGINAL
  
  0: zero
  1: one
  2: two
  3: three
  4: four
  
  The last record was [four]
  
  The first record was [zero]
  
  REVERSE
  
  5: last
  4: three
  3: Newbie
  2: one
  1: New One
  0: first
  -----------------------------*)
  

