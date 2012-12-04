module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module Make(Ord: OrderedType) = struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false
end

module MyMap = Make(struct 
  type t = int
  let compare x y = x
end)

let x = MyMap.empty

let y = MyMap.is_empty x
