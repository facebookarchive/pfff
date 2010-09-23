(** OCaml representation of JSON data *)

(** A [json_type] is a boolean, integer, real, string, null.  It can 
  also be lists [Array] or string-keyed maps [Object] of 
  [json_type]'s.  The JSON payload can only be an [Object] or [Array].

  This type is used by the parsing and printing functions from the
  {!Json_io} module. Typically, a program would convert such data into
  a specialized type that uses records, etc. For the purpose of converting
  from and to other types, two submodules are provided: {!Json_type.Browse} 
  and  {!Json_type.Build}. 
  They are meant to be opened using either [open Json_type.Browse] 
  or [open Json_type.Build]. They provided simple functions for converting
  JSON data. *)
type json_type =
    Object of (string * json_type) list
  | Array of json_type list
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null

(** [t] is an alias for [json_type]. *)
type t = json_type

(** Errors that are produced by the json-wheel library are represented
  using the [Json_error] exception.

  Other exceptions may be raised when calling functions from the library. 
  Either they come from
  the failure of external functions or like [Not_found] they
  are not errors per se, and are specifically documented.
*)
exception Json_error of string

(** This submodule provides some simple functions for checking
  and reading the structure of JSON data.

  Use [open Json_type.Browse] when you want to convert JSON data
  into another OCaml type. 
*)
module Browse :
  sig
(** [make_table] creates a hash table from the contents of a JSON [Object].
  For example, if [x] is a JSON [Object], then the corresponding table
  can be created by [let tbl = make_table (objekt x)].

  Hash tables are more efficient than lists
  if several fields must be extracted 
  and converted into something like an OCaml record.

  The key/value pairs are added from left to right.
  Therefore if there are several bindings for the same key, the latest
  to appear in the list will be the first in the list
  returned by [Hashtbl.find_all]. *)
    val make_table : (string * t) list -> (string, t) Hashtbl.t

(** [field tbl key] looks for a unique field [key] in hash table [tbl].
  It raises a [Json_error] if [key] is not found in the table
  or if it is present multiple times. *)
    val field : (string, t) Hashtbl.t -> string -> t

(** [fieldx tbl key] works like [field tbl key], but returns [Null] if
  [key] is not found in the table. This function is convenient when
  assuming that a field which is set to [Null] is the same
  as if it were not defined. 

  For instance, [optional int (fieldx tbl "year")] looks in 
  table [tbl] for a field ["year"]. If this field is set to [Null]
  or if it is undefined, then [None] is returned, otherwise
  an [Int] is expected and returned, for example as [Some 2006].
  If the value is of another JSON type than [Int] or [Null], it causes an
  error. *)
    val fieldx : (string, t) Hashtbl.t -> string -> t

(** [optfield tbl key] queries hash table [tbl] for zero or one field [key].
  The result is returned as [None] or [Some result]. If there are several
  fields with the same [key], then a [Json_error] is produced.
  
  [Null] is returned as [Some Null], not
  as [None]. For other behaviors see {!Json_type.Browse.fieldx}
  and {!Json_type.Browse.optfieldx}. *)
    val optfield : (string, t) Hashtbl.t -> string -> t option

(** [optfieldx] is the same as [optfield] except that it
  will never return [Some Null]
  but [None] instead. *)
    val optfieldx : (string, t) Hashtbl.t -> string -> t option


(** [describe x] returns a short description of the given JSON data.
  Its purpose is to help build error messages. *)
    val describe : t -> string

(** [type_mismatch expected x] raises the [Json_error msg] exception,
  where [msg] is a message that describes the error as a type mismatch
  between the element [x] and what is [expected]. *)
    val type_mismatch : string -> t -> 'a

(** tells whether the given JSON element is null *)
    val is_null : t -> bool

(** tells whether the given JSON element is not null *)
    val is_defined : t -> bool

(** raises a [Json_error] exception if the given JSON value is not [Null]. *)
    val null : t -> unit

(** reads a JSON element as a string or raises a [Json_error] exception. *)
    val string : t -> string

(** reads a JSON element as a bool or raises a [Json_error] exception. *)
    val bool : t -> bool

(** reads a JSON element as an int or a float and returns a float 
  or raises a [Json_error] exception. *)
    val number : t -> float

(** reads a JSON element as an int or raises a [Json_error] exception. *)
    val int : t -> int

(** reads a JSON element as a float or raises a [Json_error] exception. *)
    val float : t -> float

(** reads a JSON element as a JSON [Array] and returns an OCaml list,
  or raises a [Json_error] exception. *)
    val array : t -> t list

(** reads a JSON element as a JSON [Object] and returns an OCaml list,
  or raises a [Json_error] exception.

  Note the unusual spelling. [object] being
  a keyword in OCaml, we use [objekt]. [Object] with a capital is still
  spelled [Object]. *)
    val objekt : t -> (string * t) list

(** [list f x] maps a JSON [Array x] to an OCaml list, 
  converting each element
  of list [x] using [f]. A [Json_error] exception is raised if 
  the given element is not a JSON [Array]. 

  For example, converting a JSON array that must contain only ints 
  is performed using [list int x]. Similarly, a list of lists of ints 
  can be obtained using [list (list int) x]. *)
    val list : (t -> 'a) -> t -> 'a list

(** [option x] returns [None] is [x] is [Null] and [Some x] otherwise. *)
    val option : t -> t option

(** [optional f x] maps x using the given function [f] and returns 
  [Some result], unless [x] is [Null] in which case it returns [None].

  For example, [optional int x] may return something like
  [Some 123] or [None] or raise a [Json_error] exception in case
  [x] is neither [Null] nor an [Int].

  See also {!Json_type.Browse.fieldx}. *)
    val optional : (t -> 'a) -> t -> 'a option

(**/**)
    val assert_object_or_array : t -> unit
  end


(** This submodule provides some simple functions for building
  JSON data from other OCaml types.

  Use [open Json_type.Build] when you want to convert JSON data
  into another OCaml type. 
*)
module Build :
  sig
    val null : t
      (** The [Null] value *)

    val bool : bool -> t
      (** builds a JSON [Bool] *)

    val int : int -> t
      (** builds a JSON [Int] *)

    val float : float -> t
      (** builds a JSON [Float] *)

    val string : string -> t
      (** builds a JSON [String] *)

    val objekt : (string * t) list -> t
      (** builds a JSON [Object].

	See {!Json_type.Browse.objekt} for an explanation about the unusual
	spelling. *)

    val array : t list -> t
      (** builds a JSON [Array]. *)

    val list : ('a -> t) -> 'a list -> t
      (** [list f l] maps OCaml list [l] to a JSON list using 
	function [f] to convert the elements into JSON values.

      For example, [list int [1; 2; 3]] is a shortcut for
      [Array [ Int 1; Int 2; Int 3 ]]. *)

    val option : t option -> t
      (** [option x] returns [Null] is [x] is [None], or [y] if 
	[x] is [Some y]. *)

    val optional : ('a -> t) -> 'a option -> t
      (** [optional f x] returns [Null] if [x] is [None], or [f x]
	otherwise.

      For example, [list (optional int) [Some 1; Some 2; None]] returns
      [Array [ Int 1; Int 2; Null ]]. *)
  end

(**/**)

(* pad: *)
val json_of_list: ('a -> t) -> 'a list -> t
 

val string_of_loc : (Lexing.position * Lexing.position) -> string
val json_error : string -> 'a
