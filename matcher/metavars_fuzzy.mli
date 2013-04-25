
type mvar = string 

type 'a metavars_binding = (mvar, 'a) Common.assoc

val empty_environment: unit -> 'a metavars_binding

