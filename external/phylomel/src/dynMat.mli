(** Dynamic (resizable) matrices *)
open ExtLib

val print_line : float DynArray.t -> unit
val print : float DynArray.t DynArray.t -> unit
val get : 'a DynArray.t DynArray.t -> int -> int -> 'a
val unsafe_get : 'a DynArray.t DynArray.t -> int -> int -> 'a
val rem_col : 'a DynArray.t DynArray.t -> int -> unit
val of_mat : 'a array array -> 'a DynArray.t DynArray.t
val get_diff : float DynArray.t DynArray.t -> int -> int -> float
