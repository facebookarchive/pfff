(* !!take care!!: this class does side effect, not a pure oassoc.
 * 
 * Also can not put structure with ref or mutable field because when
 * you will modify those refs or fields, you will modify it in the memory,
 * not in the disk. The only way to modify on the disk is to call 
 * #add or #replace with what you modified. Oassocbdb has no way
 * to know that you modified it.
 *)
class ['a,'b] oassoc_btree : 
  Bdb.db -> 
  string                     (* db name, for profiling *) -> 
  (unit -> Bdb.dbtxn option) (* transaction handler *) ->
  ('b -> 'e) -> ('e -> 'b)   (* marshaller/unmarshaller wrappers *) ->
object('o)
  inherit ['a,'b] Oassoc.oassoc

  (* ocollection concrete instantiation of virtual methods *)
  method empty : 'o
  method add : 'a * 'b -> 'o

  method iter : ('a * 'b -> unit) -> unit
  method view : ('a * 'b, 'o) Ocollection.view

  method del : 'a * 'b -> 'o
  method mem : 'a * 'b -> bool
  method null : bool

  (* oassoc concrete instantiation of virtual methods *)
  method assoc : 'a -> 'b
  method delkey : 'a -> 'o

  method keys: 'a list

end

val create_bdb: 
  string ->
  string ->
  Bdb.dbenv ->
  (unit -> Bdb.dbtxn option) ->
  ('a -> 'b) * ('c -> 'a) ->
  int -> 
  Bdb.db * ('d, 'a) Oassoc_buffer.oassoc_buffer 
