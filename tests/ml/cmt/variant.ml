type variant =
  | Constructor1
  | Constructor2WithArg of int
  | ConstructorUnused

let use_variant = function
  | Constructor1 -> 1
  | Constructor2WithArg _ -> 2
  | _ -> 3

