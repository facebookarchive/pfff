open Common

open Oassoc

(* !!take care!!: this class does side effect, not a pure oassoc *)
class ['a,'b] oassoch xs = 
  let h = Common.hash_of_list xs in
  object(o)
    inherit ['a,'b] oassoc

    val data = h

    method empty = {< data = Hashtbl.create 101 >}
    method add (k,v) = (Hashtbl.replace data k v; o) (* not add cos add make iter sux *)

    (* redefine replkey to be more efficient than default. With hash, don't need
       to delkey before add, replace do both action directly.
     *)
    method replkey (k,v) = (Hashtbl.replace data k v; o)
    method iter f = Hashtbl.iter (curry f) data
    method view = raise Todo

    method del (k,v) = (Hashtbl.remove data k; o)
    method mem e = raise Todo
    method null = (try (Hashtbl.iter (fun k v -> raise ReturnExn) data; false) with ReturnExn -> true)

    method assoc k = 
      try 
        Hashtbl.find data k
      with Not_found -> (log3 ("pb assoc with k = " ^ (Dumper.dump k)); raise Not_found) 
        
    method delkey k = (Hashtbl.remove data k; o)

    method keys = 
      List.map fst (o#tolist)

end     

