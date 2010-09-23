(* ********************************************************************** *)
(* Taking References to Hashes *)
(* ********************************************************************** *)
let pleac_Taking_References_to_Hashes () = 
  (* Hashtbls are mutable, so creating a reference to a hash is usually
     not necessary; it creates an *additional* level of indirection. *)
  let href = ref hash
  let anon_hash = ref (Hashtbl.create 0)
  let () =
    (* Have some fun with locally-defined operators *)
    let ( => ) = Hashtbl.replace !anon_hash in
    ( "key1" => "value1"; "key2" => "value2" )
  let anon_hash_copy = ref (Hashtbl.copy !href)
  

