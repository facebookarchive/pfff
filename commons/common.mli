
val (+>) : 'a -> ('a -> 'b) -> 'b

val (=|=) : int    -> int    -> bool
val (=<=) : char   -> char   -> bool
val (=$=) : string -> string -> bool
val (=:=) : bool   -> bool   -> bool

val (=*=): 'a -> 'a -> bool

val pr : string -> unit
val pr2 : string -> unit

(* forbid pr2_once to do the once "optimisation" *)
val _already_printed : (string, bool) Hashtbl.t
val disable_pr2_once : bool ref
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

val (=~) : string -> string -> bool
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

exception CmdError of Unix.process_status * string
val command2 : string -> unit
val cmd_to_list :  ?verbose:bool -> string -> string list (* alias *)
val cmd_to_list_and_status:
  ?verbose:bool -> string -> string list * Unix.process_status

val null : 'a list -> bool
val exclude : ('a -> bool) -> 'a list -> 'a list
val sort : 'a list -> 'a list

val map_filter : ('a -> 'b option) -> 'a list -> 'b list
val find_opt: ('a -> bool) -> 'a list -> 'a option
val find_some : ('a -> 'b option) -> 'a list -> 'b
val find_some_opt : ('a -> 'b option) -> 'a list -> 'b option
val filter_some: 'a option list -> 'a list

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

val group_by: ('a -> 'b) -> 'a list -> ('b * 'a list) list
val group_assoc_bykey_eff : ('a * 'b) list -> ('a * 'b list) list
val group_by_mapped_key: ('a -> 'b) -> 'a list -> ('b * 'a list) list
val group_by_multi: ('a -> 'b list) -> 'a list -> ('b * 'a list) list

type 'a stack = 'a list
val push : 'a -> 'a stack ref -> unit

val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list

type 'a hashset = ('a, bool) Hashtbl.t 
val hashset_of_list : 'a list -> 'a hashset
val hashset_to_list : 'a hashset -> 'a list

val map_opt: ('a -> 'b) -> 'a option -> 'b option
val opt: ('a -> unit) -> 'a option -> unit
val do_option : ('a -> unit) -> 'a option -> unit
val (>>=): 'a option -> ('a -> 'b option) -> 'b option
val (|||): 'a option -> 'a -> 'a


type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
val partition_either :
  ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
val partition_either3 :
    ('a -> ('b, 'c, 'd) either3) -> 'a list -> 'b list * 'c list * 'd list



type arg_spec_full = Arg.key * Arg.spec * Arg.doc
type cmdline_options = arg_spec_full list

type options_with_title = string * string * arg_spec_full list
type cmdline_sections = options_with_title list

(* A wrapper around Arg modules that have more logical argument order, 
 * and returns the remaining args.
 *)
val parse_options : 
  cmdline_options -> Arg.usage_msg -> string array -> string list
(* Another wrapper that does Arg.align automatically *)
val usage : Arg.usage_msg -> cmdline_options -> unit

(* Work with the options_with_title type way to organize a long
 * list of command line switches.
 *)
val short_usage : 
  Arg.usage_msg -> short_opt:cmdline_options -> unit
val long_usage : 
  Arg.usage_msg -> short_opt:cmdline_options -> long_opt:cmdline_sections -> 
  unit

(* With the options_with_title way, we don't want the default -help and --help
 * so need adapter of Arg module, not just wrapper.
 *)
val arg_align2 : cmdline_options -> cmdline_options
val arg_parse2 : 
  cmdline_options -> Arg.usage_msg -> (unit -> unit) (* short_usage func *) -> 
  string list

(* The action lib. Useful to debug supart of your system. cf some of
 * my main.ml for example of use. *)
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


(* if set then will not do certain finalize so faster to go back in replay *)
val debugger : bool ref

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

type prof = ProfAll | ProfNone | ProfSome of string list
val profile : prof ref
val show_trace_profile : bool ref

val _profile_table : (string, (float ref * int ref)) Hashtbl.t ref
val profile_code : string -> (unit -> 'a) -> 'a
val profile_diagnostic : unit -> string
val profile_code_exclusif : string -> (unit -> 'a) -> 'a
val profile_code_inside_exclusif_ok : string -> (unit -> 'a) -> 'a
val report_if_take_time : int -> string -> (unit -> 'a) -> 'a
(* similar to profile_code but print some information during execution too *)
val profile_code2 : string -> (unit -> 'a) -> 'a

(* creation of /tmp files, a la gcc 
 * ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c" 
 *)
val _temp_files_created : string list ref
val save_tmp_files : bool ref
val new_temp_file : string (* prefix *) -> string (* suffix *) -> filename
val erase_temp_files : unit -> unit
val erase_this_temp_file : filename -> unit

val realpath: filename -> filename

val cache_computation : 
  ?verbose:bool -> ?use_cache:bool -> filename  -> string (* extension *) -> 
  (unit -> 'a) -> 'a

val filename_without_leading_path : string -> filename -> filename
val readable: root:string -> filename -> filename

val follow_symlinks: bool ref
val files_of_dir_or_files_no_vcs_nofilter:
 string list -> filename list

(* do some finalize, signal handling, unix exit conversion, etc *)
val main_boilerplate : (unit -> unit) -> unit

(* type of maps from string to `a *)
module SMap : Map.S with type key = String.t
type 'a smap = 'a SMap.t

