exception Ocsigen_Internal_Error of string
exception Input_is_too_large
exception Ocsigen_Bad_Request
exception Ocsigen_Request_too_long
external id : 'a -> 'a = "%identity"

type url_path = string list

val string_of_url_path : encode:bool -> string list -> string


val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val uncurry2 : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
val list_remove_first_if_any : 'a -> 'a list -> 'a list
val list_remove_first_if_any_q : 'a -> 'a list -> 'a list
val list_remove_first : 'a -> 'a list -> 'a list
val list_remove_first_q : 'a -> 'a list -> 'a list
val list_remove_all : 'a -> 'a list -> 'a list
val list_remove_all_q : 'a -> 'a list -> 'a list
val list_remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val list_remove_all_assoc_q : 'a -> ('a * 'b) list -> ('a * 'b) list
val list_last : 'a list -> 'a
val list_assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list
val list_is_prefix : 'a list -> 'a list -> bool
val list_is_prefix_skip_end_slash : string list -> string list -> bool
val remove_dotdot : string list -> string list

val remove_slash_at_beginning : string list -> string list
(** Remove the slash at beginning but if it is also at the end *)

val recursively_remove_slash_at_beginning : string list -> string list
(** Recursively remove the slash at beginning but if it is also at the end *)

val remove_slash_at_end : string list -> string list
val remove_internal_slash : string list -> string list
val add_end_slash_if_missing : string list -> string list
val change_empty_list : string list -> string list
val remove_end_slash : string -> string
val string_first_diff : string -> string -> int -> int -> int
val add_to_string : string -> string -> string -> string
val concat_strings : string -> string -> string -> string
val basic_sep : char -> string -> string * string
val remove_spaces : string -> int -> int -> string

(** Cut a string to the next separator, removing spaces.
   Raises Not_found if the separator connot be found.
 *)
val sep : char -> string -> string * string

val split : ?multisep:bool -> char -> string -> string list

val string_of_exn : exn -> string
  (** [string_of_exn e] returns a (hopefully) meaningful explanation of
      the exception [e]. *)

val register_exn_printer : ((exn -> string) -> exn -> string) -> unit
  (** [register_exn_printer p] registers [p] so that a call to [p
      string_of_exn e] is tried first in [string_of_exn e]. [p] must raise
      [e] if it doesn't handle it. *)

val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val thd3 : 'a * 'b * 'c -> 'c

val apply_option : ('a -> 'b) -> 'a option -> 'b option

type ('a, 'b) leftright = Left of 'a | Right of 'b
type yesnomaybe = Yes | No | Maybe

val get_inet_addr : string -> Unix.inet_addr Lwt.t
(** returns the first inet address for one host *)

(** IP address parsing *)
type ip_address =
  | IPv4 of int32
  | IPv6 of int64 * int64
exception Invalid_ip_address of string
val parse_ip : string -> ip_address * (ip_address option)
val match_ip : ip_address * (ip_address option) -> ip_address -> bool
val network_of_ip : 
  ip_address -> 
  int32 (* ipv4 mask *) -> 
  (int64 * int64) (* ipv6 mask *) -> 
  ip_address
val inet6_addr_loopback : ip_address

val getnameinfo : Unix.inet_addr -> int -> string Lwt.t
(** calls Lwt_lib.getnameinfo and returns the result,
    but if it fails returns the IP number,
    with [ before and ] after IPv6 addresses. *)

val basename : string -> string

(** Extension of a file. Raises [Not_found] if the argument has no
  extension *)
val extension : string -> string

(** Extension of a file. Supposes that the argument does not
    contain '/' (but is faster than extension) *)
val extension_no_directory : string -> string

val fixup_url_string : string -> string
val parse_url : string ->
  bool option * string option * int option *
    string * string list * string option *
    (string * string) list Lazy.t

val make_absolute_url : 
  https:bool -> host:string -> port:int -> string -> string

module StringSet : Set.S with type elt = string

module Clist :
sig
  type 'a t
  type 'a node
  val make : 'a -> 'a node
  val create : unit -> 'a t
  val insert : 'a t -> 'a node -> unit
  val remove : 'a node -> unit
  val value : 'a node -> 'a
  val in_list : 'a node -> bool
  val is_empty : 'a t -> bool
  val iter : ('a -> unit) -> 'a t -> unit
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

module String_Table : Deriving_Json_stdlib.Map.S with type key = string
module Int_Table : Map.S with type key = int

(** The files sent in the request *)
type file_info = {tmp_filename: string; (** Where the file is stored on the server*)
                  filesize: int64; (** Size, in bytes *)
                  raw_original_filename: string;
                  (** Original file name, as given by the client. *)
                  original_basename: string (** Original file name *);
                  file_content_type: (string * string option) option (** content-type, and optionally charset, of the file, if supplied *);
                 }
(** Note that the files are cancelled once the request has been fulfilled *)

(** Only IE is known to make [raw_original_filename] and
    [original_basename] differ, as it sends the full original path
    of uploaded files.  In all cases, [original_basename] is the
    basename of the file. More precisely, it is the part of the
    filename after the last [/] or [\ ], if any, or ["none"] if one of
    these characters is the last one. You should probably never use
    [raw_original_filename]. *)





val mk_url_encoded_parameters : (string * string) list -> string

val encode : ?plus:bool -> string -> string
val decode : ?plus:bool -> string -> string

val make_cryptographic_safe_string : unit -> string
(* This function generates a new (224 bits long) string of 56 hexadecimal
 characters. *)

(*****************************************************************************)

val debug : string -> unit

val to_json : ?typ:'a Deriving_Json.t -> 'a -> string
val of_json : ?typ:'a Deriving_Json.t -> string -> 'a
