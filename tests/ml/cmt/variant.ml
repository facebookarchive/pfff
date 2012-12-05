type variant =
  | Constructor1
  | Constructor2WithArg of int
  | ConstructorUnused

let use_variant_in_pattern = function
  | Constructor1 -> 1
  | Constructor2WithArg _ -> 2
  | _ -> 3

let use_variant () = 
  Constructor1

module Nested = struct
  type variantbis =
    | ConstructorBis
        
  let use_variant_nested = function
    | ConstructorBis -> 1
        
  let use_variant_enclosing = function
    | Constructor1 -> 1
    | _ -> 2
end

