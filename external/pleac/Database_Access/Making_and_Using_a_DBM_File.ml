(* ********************************************************************** *)
(* Making and Using a DBM File *)
(* ********************************************************************** *)
let pleac_Making_and_Using_a_DBM_File () = 
  #load "dbm.cma";;
  
  (* open database *)
  let db = Dbm.opendbm filename [Dbm.Dbm_rdwr; Dbm.Dbm_create] 0o666
  
  (* retrieve from database *)
  let v = Dbm.find db key
  
  (* put value into database *)
  let () = Dbm.replace db key value
  
  (* check whether in database *)
  let () =
    try
      ignore (Dbm.find db key);
      (* ... *)
      ()
    with Not_found ->
      (* ... *)
      ()
  
  (* delete from database *)
  let () = Dbm.remove db key
  
  (* close the database *)
  let () = Dbm.close db
  
  (*-----------------------------*)
  
  (* userstats - generates statistics on who is logged in. *)
  (* call with an argument to display totals *)
  
  #load "dbm.cma";;
  #load "str.cma";;
  #load "unix.cma";;
  
  let db_file = "/tmp/userstats.db"  (* where data is kept between runs *)
  let db = Dbm.opendbm db_file [Dbm.Dbm_rdwr; Dbm.Dbm_create] 0o666
  
  let () =
    if Array.length Sys.argv > 1
    then
      begin
        let sort a = Array.sort compare a; a in
        let keys db = Array.of_list
          (let accu = ref [] in
           Dbm.iter (fun key _ -> accu := key :: !accu) db;
           !accu) in
        let users = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
        let users = if users = [|"ALL"|] then sort (keys db) else users in
        Array.iter
          (fun user ->
             Printf.printf "%s\t%s\n"
               user (try Dbm.find db user with Not_found -> ""))
          users
      end
    else
      begin
        let who = Unix.open_process_in "who" in
        let regexp = Str.regexp "[ \t]+" in
        try
          while true do
            (* extract username (first thing on the line) and update *)
            let line = input_line who in
            let user = List.hd (Str.split_delim regexp line) in
            let count =
              try int_of_string (Dbm.find db user)
              with Not_found -> 0 in
            Dbm.replace db user (string_of_int (count + 1))
          done
        with End_of_file ->
          ignore (Unix.close_process_in who)
      end
  
  let () = Dbm.close db
  

