open Bdb

open Printf

(* 
note:
 as indicated in minsky version of ocamlbdb, if use transaction with a cursor then
 the cursor must be closed before commit
*)

(* inspired by mkfs.ml from lex stein *)
(* 
let _ = Unix.system ("rm -f /tmp/test/*") 
*)
let env = Env.create [] 
let _ = Env.env_open env "/tmp/test" [Env.DB_CREATE;Env.DB_INIT_LOG;Env.DB_INIT_LOCK; Env.DB_INIT_MPOOL;Env.DB_INIT_TXN ;Env.DB_RECOVER] (Int32.of_int 0)

let t = ref (Txn.txn_begin env None [])
let newt () = t := (Txn.txn_begin env None [])

let trans () = None
let trans () = Some !t

let main1 () = 
  let db = Db.create env [] in
  let _ = try Db.db_open db None "/tmp/test/db" "/db" Db.DB_BTREE [Db.DB_CREATE] 0 with _ -> () in
  let _ = print_endline "here" in

(*
  let _ = Db.put db None "xxx" "vvv" [] in
  let _ = Db.put db None "aaa" "zzz" [] in
  let _ = Db.put db None "v" "zzz" [] in
  let _ = (try (Printf.printf "%s\n" (Db.get db None "xxxx" [])) with _ -> print_endline "pb") in
*)

  let _ = 
    for i = 100 downto 1 do
      let _ = Db.put db None (sprintf "%d" i) (sprintf "%d" i) [] in ()
      (* let _ = Db.put db None (Marshal.to_string i []) (Marshal.to_string i []) [] in ()*)
    done
  in
    
  let dbc = Cursor.db_cursor db None [] in
  let rec aux dbc = 
    try (
      let a = Cursor.dbc_get dbc [Cursor.DB_NEXT] in
      (Printf.printf "%s --> %s\n" (fst a) (snd a); flush stdout;
      (* (Printf.printf "%d --> %d\n" (Marshal.from_string (fst a) 0) (Marshal.from_string (snd a) 0); flush stdout; *)
       aux dbc;
       ())
     ) with _ -> ()
  in
  let _ = aux dbc in

(*
  let _ = 
    for i = 0 to 100000 do
      Db.put db None (Marshal.to_string "xxx" []) (Marshal.to_string i [Marshal.Closures]) []
    done in
  *)
  

(*  let _ = Printf.printf "%s\n" (Db.get db None (Marshal.to_string "xxx" [])) in *)

  
  let _ = Db.close db [] in
  ()


(* test having complex structure as value, via marshalling *)
let main2 () = 
  let db = Db.create env [] in
  let _ = try Db.db_open db None "/tmp/test/db2" "/db2" Db.DB_BTREE [Db.DB_CREATE] 0 with _ -> () in

  let _ = (print_endline "here"; flush stdout) in
  let fmar s = Marshal.to_string s [] in
  let _ = Db.put db None "xxx" (fmar []) [] in
  let _ = Db.put db None "aaa" (fmar [1;2;3]) [] in
  let _ = Db.put db None "v"   (fmar [2]) [] in

  let dbc = Cursor.db_cursor db None [] in
  let rec aux dbc = 
    try (
      let a = Cursor.dbc_get dbc [Cursor.DB_NEXT] in
      (Printf.printf "%s --> " (fst a);
       List.iter (fun i -> print_int i) (Marshal.from_string (snd a) 0);
       print_endline "";
       flush stdout;
       aux dbc;
       ())
     ) with _ -> ()
  in
  let _ = aux dbc in

  let _ = Db.close db [] in
  let _ = Env.close env [] in

  ()
  




let test_transaction () = 
  let db = Db.create env [] in
  let print_db trans = 
    let dbc = Cursor.db_cursor db (trans ()) [] in
    let rec aux dbc = 
    try (
      let a = Cursor.dbc_get dbc [Cursor.DB_NEXT] in
      (Printf.printf "%s --> " (fst a);
       List.iter (fun i -> print_int i) (Marshal.from_string (snd a) 0);
       print_endline "";
       flush stdout;
       aux dbc;
       ())
     ) with _ -> ()
    in
    let _ = aux dbc in
    Cursor.dbc_close dbc
  in
  let fmar s = Marshal.to_string s [] in
  begin

    Db.db_open db (trans ()) "/tmp/test/db3" "/db3" Db.DB_BTREE [Db.DB_CREATE] 0;
    (print_endline "here"; flush stdout);
    Db.put db (trans ()) "___" (fmar []) [];
    print_db trans;

    Db.put db (trans ()) "xxx" (fmar []) [];
    Db.put db (trans ()) "aaa" (fmar [1;2;3]) [];
    Db.put db (trans ()) "v"   (fmar [2]) [];

    print_db trans;
    Txn.commit !t None;
    newt();


    Db.put db (trans()) "NORMALLYNOTIN"   (fmar [2]) [];

    print_db trans;




(*  
  let pid1 = Unix.getpid () in
  let _ = Sys.set_signal Sys.sigalrm (Sys.Signal_handle 
                                         (fun _ -> 
                                           let pid = Unix.getpid () in
                                           let _ = Printf.printf "preparring the kill of %d and %d\n" pid1 pid in
                                           let _ = flush stdout in
                                           
                                           Sys.command ("kill -9 " ^ (string_of_int pid));
                                           ()
                                           )) in
*)
  let _ = Sys.set_signal Sys.sigalrm (Sys.Signal_handle   (fun _ -> failwith "pb" )) in

(*  let _ = Sys.set_signal Sys.sigalrm (Sys.Signal_default) in *)
  let _ = Unix.alarm 2 in


(*  Txn.abort t; raise x *)

  while true do 
    ()
  done;
  Db.close db [];
  Env.close env [];
  end  
  
  


let _ = test_transaction ()  
       

