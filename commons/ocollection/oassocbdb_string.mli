(* !!take care!!: this class does side effect, not a pure oassoc.
 * 
 * Also can not put structure with ref or mutable field because when
 * you will modify those refs or fields, you will modify it in the memory,
 * not in the disk. The only way to modify on the disk is to call 
 * #add or #replace with what you modified. Oassocbdb has no way
 * to know that you modified it.
 *)
class ['b] oassoc_btree_string : 
  Bdb.db -> 
  string                     (* db name, for profiling *) -> 
  (unit -> Bdb.dbtxn option) (* transaction handler *) ->
object('o)
  inherit [string,'b] Oassoc.oassoc

  (* ocollection concrete instantiation of virtual methods *)
  method empty : 'o
  method add : string * 'b -> 'o

  method iter : (string * 'b -> unit) -> unit
  method view : (string * 'b, 'o) Ocollection.view

  method del : string * 'b -> 'o
  method mem : string * 'b -> bool
  method null : bool

  (* oassoc concrete instantiation of virtual methods *)
  method assoc : string -> 'b
  method delkey : string -> 'o

  method keys: string list

end

val create_bdb: 
  string ->
  string ->
  Bdb.dbenv ->
  (unit -> Bdb.dbtxn option) ->
  int -> 
  Bdb.db * (string, 'a) Oassoc_buffer.oassoc_buffer 
