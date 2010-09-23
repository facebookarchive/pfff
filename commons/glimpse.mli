(*s: glimpse.mli *)
open Common

(* ---------------------------------------------------------------------- *)
type glimpse_search = 
  | GlimpseCaseInsensitive 
  | GlimpseWholeWord
val default_glimpse_search : glimpse_search list

type glimpsedir = Common.dirname

(* ---------------------------------------------------------------------- *)
val glimpseindex : string -> dirname -> glimpsedir -> unit

val glimpseindex_files : filename list -> glimpsedir -> unit


(* ---------------------------------------------------------------------- *)
val glimpse : 
  string -> ?options:glimpse_search list -> glimpsedir -> filename list
val grep : 'a -> 'b


(* ---------------------------------------------------------------------- *)
val check_have_glimpse : unit -> unit

val s_of_glimpse_search : glimpse_search -> string
val s_of_glimpse_options : glimpse_search list -> string


(*e: glimpse.mli *)
