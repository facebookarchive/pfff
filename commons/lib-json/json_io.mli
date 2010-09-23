(** Input and output functions for the JSON format 
  as defined by {{:http://www.json.org/}http://www.json.org/} *)


(** [json_of_string s] reads the given JSON string.

  If [allow_comments] is [true], then C++ style comments are allowed, i.e.
  [/* blabla possibly on several lines */] or 
  [// blabla until the end of the line]. Comments are not part of the JSON
  specification and are disabled by default.

  If [allow_nan] is [true], then OCaml [nan], [infinity] and [neg_infinity] 
  float values are represented using their Javascript counterparts
  [NaN], [Infinity] and [-Infinity].

  If [big_int_mode] is [true], then JSON ints that cannot be represented
  using OCaml's int type are represented by strings. 
  This would happen only for ints that are out of the range defined
  by [min_int] and [max_int], i.e. \[-1G, +1G\[ on a 32-bit platform.
  The default is [false] and a [Json_type.Json_error] exception
  is raised if an int is too big.

  If [recursive] is true, then all JSON values are accepted rather
  than just arrays and objects as specified by the standard.
  The default is [false].
*)
val json_of_string : 
  ?allow_comments:bool ->
  ?allow_nan:bool ->
  ?big_int_mode:bool ->
  ?recursive:bool -> 
  string -> Json_type.t

(** Same as [Json_io.json_of_string] but the argument is a file
  to read from. *)
val load_json : 
  ?allow_comments:bool ->
  ?allow_nan:bool ->
  ?big_int_mode:bool ->
  ?recursive:bool -> 
  string -> Json_type.t

(** Conversion of JSON data to compact text. *)
module Compact :
sig
  (** Generic printing function without superfluous space. 
    See the standard [Format] module
    for how to create and use formatters. 

    In general, {!Json_io.string_of_json} and 
    {!Json_io.save_json} are more convenient.
  *)
  val print : 
    ?allow_nan: bool ->
    ?recursive:bool ->
    Format.formatter -> Json_type.t -> unit
end

(** Conversion of JSON data to compact text, optimized for speed. *)
module Fast :
sig
  (** This function is faster than the one provided by the
    {!Json_io.Compact} submodule but it is less generic and is subject to
    the 16MB size limit of strings on 32-bit architectures. *)
  val print : 
    ?allow_nan: bool ->
    ?recursive:bool ->
    Buffer.t -> Json_type.t -> unit
end


(** Conversion of JSON data to indented text. *)
module Pretty :
sig
  (** Generic pretty-printing function. 
    See the standard [Format] module
    for how to create and use formatters.
    
    In general, {!Json_io.string_of_json} and 
    {!Json_io.save_json} are more convenient.
  *)
  val print : 
    ?allow_nan: bool ->
    ?recursive:bool ->
    Format.formatter -> Json_type.t -> unit
end

(** [string_of_json] converts JSON data to a string.

  By default, the output is indented. If the [compact] flag is set to true,
  the output will not contain superfluous whitespace and will
  be produced faster.

  If [allow_nan] is [true], then OCaml [nan], [infinity] and [neg_infinity] 
  float values are represented using their Javascript counterparts
  [NaN], [Infinity] and [-Infinity].
*)
val string_of_json :
  ?allow_nan: bool ->
  ?compact:bool ->
  ?recursive:bool ->
  Json_type.t -> string

(** [save_json] works like {!Json_io.string_of_json} but 
  saves the results directly into the file specified by the
  argument of type string. *)
val save_json : 
  ?allow_nan:bool -> 
  ?compact:bool -> 
  ?recursive:bool ->
  string -> Json_type.t -> unit
