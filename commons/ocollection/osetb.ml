open Ocollection
open Oset

let empty = Set_poly.empty

class ['a] osetb xs   = 
  object(o)
    inherit ['a] oset

    val data = xs (*  Set_poly.empty *)
    method tosetb = data

    (* if put [] then no segfault, if [11] then segfault *)
    method toset = Obj.magic data 

    method empty = {< data = Set_poly.empty >}
    method add e = {< data = Set_poly.add e data >}
    method iter f = Set_poly.iter f   data
    method view = 
      if Set_poly.is_empty data 
      then Empty 
      else let el = Set_poly.choose data in Cons (el, o#del el)

    method del e = {< data = Set_poly.remove e data >}
    method mem e  = Set_poly.mem e    data
    method null   = Set_poly.is_empty data

    method tolist = Set_poly.elements data
    method length = Set_poly.cardinal data

    method union s = {< data = Set_poly.union data s#tosetb >}
    method inter s = {< data = Set_poly.inter data s#tosetb >}
    method minus s = {< data = Set_poly.diff  data s#tosetb >}
    (* todo: include, ... *)
        
  end
