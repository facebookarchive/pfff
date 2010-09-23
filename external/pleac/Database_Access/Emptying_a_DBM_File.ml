(* ********************************************************************** *)
(* Emptying a DBM File *)
(* ********************************************************************** *)
let pleac_Emptying_a_DBM_File () = 
  let () =
    let db = Dbm.opendbm filename [Dbm.Dbm_rdwr; Dbm.Dbm_create] 0o666 in
      let keys = ref [] in
      Dbm.iter (fun key _ -> keys := key :: !keys) db;
      List.iter (Dbm.remove db) !keys;
      Dbm.close db
  
  (*-----------------------------*)
  
  let () =
    Sys.remove filename;
    ignore (Dbm.opendbm filename [Dbm.Dbm_rdwr; Dbm.Dbm_create] 0o666)
  

