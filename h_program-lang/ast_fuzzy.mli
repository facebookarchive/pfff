
type tok = Parse_info.info
type 'a wrap = 'a * tok

type tree =
  | Braces of tok * trees * tok
  | Parens of tok * (trees, tok (* comma*)) Common.either list * tok
  | Angle  of tok * trees * tok
  (* note that gcc allows $ in identifiers, so using $ for metavariables
   * means we will not be able to match such identifiers. No big deal.
   *)
  | Metavar of string wrap
  (* note that "..." are allowed in many languages, so using "..."
   * to represent a list of anything means we will not be able to
   * match specifically "...".
   *)
  | Dots of tok
  | Tok of string wrap
and trees = tree list

val is_metavar: string -> bool

val abstract_position_trees: trees -> trees
val toks_of_trees: trees -> tok list
val vof_trees: trees -> Ocaml.v

type visitor_out = trees -> unit
type visitor_in = {
  ktree: (tree -> unit) * visitor_out -> tree -> unit;
  ktrees: (trees -> unit) * visitor_out -> trees -> unit;
  ktok: (tok -> unit) * visitor_out -> tok -> unit;
}

val default_visitor: visitor_in
val mk_visitor: visitor_in -> visitor_out
