
type t = C | D

module Nested = struct
  (* this should not shadow the enclosing type definitions,
   * values and types are in different namespace 
   *)
  let t x = x

  let f = function
    | C -> 1
    | D -> 2

end
