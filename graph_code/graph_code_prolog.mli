type fact =
  | At of entity * Common.filename (* readable path *) * int (* line *)
  | Kind of entity * Database_code.entity_kind
  | Type of entity * string

  | Extends of string * string 
  | Implements of string * string
  | Mixins of string * string

  | Privacy of entity * Database_code.privacy

  | Call of entity * entity
  | UseData of entity * entity

  | Misc of string

  and entity = 
   string list (* package/module/namespace/class qualifier*) * string (* name *)

val string_of_fact: fact -> string
val entity_of_str: string -> entity

(* reused in other modules which generate prolog facts *)
val string_of_entity_kind: Database_code.entity_kind -> string

val build: Graph_code.graph -> fact list
