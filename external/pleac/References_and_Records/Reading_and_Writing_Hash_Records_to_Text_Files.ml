(* ********************************************************************** *)
(* Reading and Writing Hash Records to Text Files *)
(* ********************************************************************** *)
let pleac_Reading_and_Writing_Hash_Records_to_Text_Files () = 
  #load "str.cma";;
  
  (* Define a list reference to contain our data. *)
  let (list_of_records : (string, string) Hashtbl.t list ref) = ref []
  
  (* Read records from standard input. *)
  let () =
    let regexp = Str.regexp "\\([^:]+\\):[ \t]*\\(.*\\)" in
    let record = ref (Hashtbl.create 0) in
    begin
      try
        while true do
          let line = read_line () in
          if Str.string_match regexp line 0
          then
            let field = Str.matched_group 1 line in
            let value = Str.matched_group 2 line in
            Hashtbl.replace !record field value
          else
            (list_of_records := !record :: !list_of_records;
             record := Hashtbl.create 0)
        done
      with End_of_file ->
        if Hashtbl.length !record > 0
        then list_of_records := !record :: !list_of_records
    end
  
  (* Write records to standard output. *)
  let () =
    List.iter
      (fun record ->
         Hashtbl.iter
           (fun field value -> Printf.printf "%s: %s\n" field value)
           record;
         print_newline ())
      !list_of_records
  

