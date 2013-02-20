
val (+>) : 'a -> ('a -> 'b) -> 'b

val (=|=) : int    -> int    -> bool
val (=<=) : char   -> char   -> bool
val (=$=) : string -> string -> bool
val (=:=) : bool   -> bool   -> bool

val (=*=): 'a -> 'a -> bool

val pr : string -> unit
val pr2 : string -> unit
val pr2_once : string -> unit

val pr2_gen: 'a -> unit
val dump: 'a -> string

exception Todo
exception Impossible

exception Multi_found

val exn_to_s : exn -> string

val i_to_s : int -> string
val s_to_i : string -> int

val null_string : string -> bool

val ( =~ ) : string -> string -> bool
val matched1 : string -> string
val matched2 : string -> string * string
val matched3 : string -> string * string * string
val matched4 : string -> string * string * string * string
val matched5 : string -> string * string * string * string * string
val matched6 : string -> string * string * string * string * string * string
val matched7 : string -> string * string * string * string * string * string * string

val spf : ('a, unit, string) format -> 'a

val join : string (* sep *) -> string list -> string
val split : string (* sep regexp *) -> string -> string list

type filename = string
type dirname = string
type path = string

val cat :      filename -> string list

val write_file : file:filename -> string -> unit
val read_file : filename -> string

val with_open_outfile : 
  filename -> ((string -> unit) * out_channel -> 'a) -> 'a
val with_open_infile : 
  filename -> (in_channel -> 'a) -> 'a

val command2 : string -> unit
val cmd_to_list :  ?verbose:bool -> string -> string list (* alias *)

val ( ++ ) : 'a list -> 'a list -> 'a list
val null : 'a list -> bool
val exclude : ('a -> bool) -> 'a list -> 'a list
val sort : 'a list -> 'a list

val map_filter : ('a -> 'b option) -> 'a list -> 'b list
val find_some : ('a -> 'b option) -> 'a list -> 'b
val find_some_opt : ('a -> 'b option) -> 'a list -> 'b option

val take : int -> 'a list -> 'a list
val take_safe : int -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list
val span : ('a -> bool) -> 'a list -> 'a list * 'a list

val index_list   : 'a list -> ('a * int) list
val index_list_0 : 'a list -> ('a * int) list
val index_list_1 : 'a list -> ('a * int) list

type ('a, 'b) assoc = ('a * 'b) list

val sort_by_val_lowfirst: ('a,'b) assoc -> ('a * 'b) list
val sort_by_val_highfirst: ('a,'b) assoc -> ('a * 'b) list

val sort_by_key_lowfirst: ('a,'b) assoc -> ('a * 'b) list
val sort_by_key_highfirst: ('a,'b) assoc -> ('a * 'b) list

val group_assoc_bykey_eff : ('a * 'b) list -> ('a * 'b list) list
val group_by_mapped_key: ('a -> 'b) -> 'a list -> ('b * 'a list) list

type 'a stack = 'a list
val push2 : 'a -> 'a stack ref -> unit

val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list

type 'a hashset = ('a, bool) Hashtbl.t 
val hashset_of_list : 'a list -> 'a hashset
val hashset_to_list : 'a hashset -> 'a list

val opt: ('a -> unit) -> 'a option -> unit
val do_option : ('a -> unit) -> 'a option -> unit


type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
val partition_either :
  ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
val partition_either3 :
    ('a -> ('b, 'c, 'd) either3) -> 'a list -> 'b list * 'c list * 'd list



type arg_spec_full = Arg.key * Arg.spec * Arg.doc
type cmdline_options = arg_spec_full list

val usage : Arg.usage_msg -> cmdline_options -> unit
val parse_options : 
  cmdline_options -> Arg.usage_msg -> string array -> string list

type flag_spec   = Arg.key * Arg.spec * Arg.doc
type action_spec = Arg.key * Arg.doc * action_func 
   and action_func = (string list -> unit)

type cmdline_actions = action_spec list
exception WrongNumberOfArguments

val mk_action_0_arg : (unit -> unit)                       -> action_func
val mk_action_1_arg : (string -> unit)                     -> action_func
val mk_action_2_arg : (string -> string -> unit)           -> action_func
val mk_action_3_arg : (string -> string -> string -> unit) -> action_func
val mk_action_4_arg : (string -> string -> string -> string -> unit) -> 
  action_func

val mk_action_n_arg : (string list -> unit) -> action_func

val options_of_actions: 
  string ref (* the action ref *) -> cmdline_actions -> cmdline_options
val do_action: 
  Arg.key -> string list (* args *) -> cmdline_actions -> unit
val action_list: 
  cmdline_actions -> Arg.key list


(* emacs spirit *)
val unwind_protect : (unit -> 'a) -> (exn -> 'b) -> 'a

(* java spirit *)
val finalize :       (unit -> 'a) -> (unit -> 'b) -> 'a

val save_excursion : 'a ref -> 'a -> (unit -> 'b) -> 'b

val memoized : 
  ?use_cache:bool -> ('a, 'b) Hashtbl.t -> 'a -> (unit -> 'b) -> 'b

exception UnixExit of int 

exception Timeout
val timeout_function :
  ?verbose:bool ->
  int -> (unit -> 'a) -> 'a

val profile_code : string -> (unit -> 'a) -> 'a

val new_temp_file : string (* prefix *) -> string (* suffix *) -> filename

val realpath: filename -> filename

val cache_computation : 
  ?verbose:bool -> ?use_cache:bool -> filename  -> string (* extension *) -> 
  (unit -> 'a) -> 'a

val filename_without_leading_path : string -> filename -> filename

val files_of_dir_or_files_no_vcs :
  string (* ext *) -> string list -> filename list
val files_of_dir_or_files_no_vcs_nofilter:
 string list -> filename list

(* do some finalize, signal handling, unix exit conversion, etc *)
val main_boilerplate : (unit -> unit) -> unit
