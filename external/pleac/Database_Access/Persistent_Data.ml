(* ********************************************************************** *)
(* Persistent Data *)
(* ********************************************************************** *)
let pleac_Persistent_Data () = 
  type data = {mutable variable1: string; mutable variable2: string}
  
  module PersistentStore =
    MakeSerializedDbm(struct
                        type value = data
                        let serialize x = Marshal.to_string x []
                        let deserialize x = Marshal.from_string x 0
                      end)
  
  let with_persistent_data f =
    let db =
      PersistentStore.opendbm "data.db" [Dbm.Dbm_rdwr; Dbm.Dbm_create] 0o666 in
    let data =
      try PersistentStore.find db "data"
      with Not_found -> {variable1=""; variable2=""} in
    f data;
    PersistentStore.replace db "data" data
    PersistentStore.close db
  
  let () =
    with_persistent_data
      (fun data ->
         begin
           Printf.printf "variable1 = %s\nvariable2 = %s\n"
             data.variable1 data.variable2;
           data.variable1 <- "foo";
           data.variable2 <- "bar";
         end)
  

