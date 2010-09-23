(* ********************************************************************** *)
(* Counting Lines (or Paragraphs or Records) in a File *)
(* ********************************************************************** *)
let pleac_Counting_Lines__or_Paragraphs_or_Records__in_a_File () = 
  #load "unix.cma";;
  
  let () =
    let proc = Unix.open_process_in ("wc -l < " ^ file) in
    let count = int_of_string (input_line proc) in
    ignore (Unix.close_process_in proc);
    (* count now holds the number of lines read *)
    ()
  
  (*-----------------------------*)
  
  let () =
    let count = ref 0 in
    let chan = open_in file in
    (try
       while true do
         ignore (input_line chan);
         incr count
       done
     with End_of_file -> close_in chan);
    (* !count now holds the number of lines read *)
    ()
  
  (*-----------------------------*)
  
  #load "str.cma";;
  
  let () =
    let delim = Str.regexp "[ \n\r\t]*$" in
    let count = ref 0 in
    let in_para = ref false in
    let chan = open_in file in
    (try
       while true do
         if Str.string_match delim (input_line chan) 0
         then in_para := false
         else begin
           if not !in_para then incr count;
           in_para := true
         end
       done
     with End_of_file -> close_in chan);
    (* !count now holds the number of paragraphs read *)
    ()
  

