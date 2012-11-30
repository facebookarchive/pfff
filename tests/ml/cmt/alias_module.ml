module X = Variant
open Nested_module
module Y = Nested

let use_variant = function
  | X.Constructor1 -> 1
  | _ -> 2
