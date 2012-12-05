
type foo = A | B
module Nested = struct
    type t = foo
end

module NestedAlias = Nested

type bar = NestedAlias.t
