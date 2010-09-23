open Common

(* specialisation of oassocbdb that avoids some marshaling cost *)

open Bdb

open Oassoc

(* !!take care!!: this class does side effect, not a pure oassoc 
 *)
class ['b] oassoc_btree_string db namedb transact = 
let namedb = if namedb = "" then "" else "(" ^ namedb ^ ")" in
object(o)
  inherit [string,'b] oassoc
    
  val data = db
    
  method empty = 
    raise Todo

  method private addbis (k,v) = 
    let k' = k in
    let v' = 
      try Common.marshal__to_string v [] 
      with Out_of_memory -> 
        pr2 ("PBBBBBBB Out_of_memory in: " ^ namedb);
        raise Out_of_memory
          
    in (* still clos? *)
    Db.put data (transact()) k' v' []; 
    o
  method add x = 
    Common.profile_code ("Btree.add" ^ namedb) (fun () -> o#addbis x)

  (* bugfix: if not tail call (because of a try for instance), 
   * then strange behaviour in native mode 
   *)
  method private iter2 f = 
    let dbc = Cursor.db_cursor db (transact()) [] in
    (* minsky wrapper? Cursor.create ~writecursor:false ~txn:(transact()) db *)
    let rec aux dbc = 
      if
	(try 
	    let a = Cursor.dbc_get dbc [Cursor.DB_NEXT] in
            (* minsky ? Cursor.get dbc Cursor.NEXT [] *)
	    let key  = (fst a) in 
	    let valu = (Common.marshal__from_string (snd a) 0) in
	    f (key, valu);
            true
	 with Failure "ending" -> false
        ) 
      then aux dbc
      else ()
        
    in 
    aux dbc; 
    Cursor.dbc_close dbc (* minsky Cursor.close dbc *)

  method iter x = 
    Common.profile_code ("Btree.iter" ^ namedb) (fun () -> o#iter2 x)

  method view = 
    raise Todo


  
  method private length2 = 
    let dbc = Cursor.db_cursor db (transact()) [] in

    let count = ref 0 in
    let rec aux dbc = 
      if (
        try 
	  let _a = Cursor.dbc_get dbc [Cursor.DB_NEXT] in
          incr count;
          true
	with Failure "ending" -> false 
      )
      then aux dbc
      else ()

    in 
    aux dbc; 
    Cursor.dbc_close dbc; 
    !count 

  method length = 
    Common.profile_code ("Btree.length" ^ namedb) (fun () -> o#length2)


  method del (k,v) = raise Todo
  method mem e = raise Todo
  method null = raise Todo

  method private assoc2 k = 
    try 
      let k' = k in
      let vget = Db.get data (transact()) k' [] in
      (* minsky ? Db.get data ~txn:(transact() *)
      (Common.marshal__from_string vget  0)
    with Not_found -> 
      log3 ("pb assoc with k = " ^ (k)); 
      raise Not_found
  method assoc x = 
    Common.profile_code ("Btree.assoc" ^ namedb) (fun () -> o#assoc2 x)

  method private delkey2 k = 
    let k' = k in
    Db.del data (transact()) k'  []; 
    o
  method delkey x = 
    Common.profile_code ("Btree.delkey" ^ namedb) (fun () -> o#delkey2 x)


  method keys = 
    let res = ref [] in 
    let dbc = Cursor.db_cursor db (transact()) [] in
    let rec aux dbc = 
      if
	(try 
	    let a = Cursor.dbc_get dbc [Cursor.DB_NEXT] in
            (* minsky ? Cursor.get dbc Cursor.NEXT [] *)
	    let key  = (fst a) in 
	    (* 
               let valu = unv (Common.marshal__from_string (snd a) 0) in
	       f (key, valu);
            *)
            Common.push2 key res;
            true
	 with Failure "ending" -> false
        ) 
      then aux dbc
      else ()
        
    in 
    aux dbc; 
    Cursor.dbc_close dbc (* minsky Cursor.close dbc *);
    !res


  method clear = 
    let dbc = Cursor.db_cursor db (transact()) [] in
    let rec aux dbc = 
      if
	(try 
	    let a = Cursor.dbc_get dbc [Cursor.DB_NEXT] in
            Db.del data (transact()) (fst a)  [];             
            true
	 with Failure "ending" -> false
        ) 
      then aux dbc
      else ()
        
    in 
    aux dbc; 
    Cursor.dbc_close dbc (* minsky Cursor.close dbc *);
    ()

end	


let create_bdb metapath dbname env  transact size_buffer_oassoc_buffer =
  let db = Bdb.Db.create env [] in 
  Bdb.Db.db_open db (transact()) 
    (spf "%s/%s.db4" metapath dbname) 
    (spf "/%s.db4" dbname) 
    Bdb.Db.DB_BTREE [Bdb.Db.DB_CREATE] 0;
  db,
  new Oassoc_buffer.oassoc_buffer size_buffer_oassoc_buffer
    (new oassoc_btree_string db dbname transact)

