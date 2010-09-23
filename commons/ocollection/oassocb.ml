open Common

open Oassoc

class ['a,'b] oassocb xs = 
  object(o)
    inherit ['a,'b] oassoc

    val data = Map_poly.empty

    method empty = {< data = Map_poly.empty >}
    method add (k,v) = {< data = Map_poly.add k v data >}
    method replkey (k,v) = {< data = Map_poly.add k v (Map_poly.remove k data) >}
    method iter f = Map_poly.iter (curry f) data
    method view = raise Todo

    method del (k,v) = {< data = Map_poly.remove k data >}
    method mem e = raise Todo
    method null = (Map_poly.is_empty data)

    method assoc k = Map_poly.find k data
    method delkey k = {< data = Map_poly.remove k data >}

    method keys = 
      List.map fst (o#tolist)
end     

