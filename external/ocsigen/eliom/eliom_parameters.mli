(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomparameters.mli
 * Copyright (C) 2007 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(**
   This module defines the values used to declare the type of service
   parameters.
 *)

(** Here are some examples of how to specify the types and names of
   service parameters:
   - [unit] for a service without parameter.
   - [(int "myvalue")] for a service that takes one parameter,
   of type [int], called ["myvalue"].
   (The service handler function takes a parameter of type [int]).
   - [(int "myvalue" ** string "mystring")] for a service that takes
   two parameters, one of type [int] called ["myvalue"],
   and one of type [string] called ["mystring"].
   (The service handler function takes a parameter of type [(int * string)]).
   - [(opt (int "myvalue"))] for a service that takes an optional parameter
   of type [int] called ["myvalue"].
   (The handler function takes a parameter of type [int option]).
   - [(set int "myvalue")] for a service that takes any number of integer
   parameters, all called ["myvalue"].
   (The handler function takes a parameter of type [int list]).
   - [list "l" (int "myvalue" ** string "mystring")] for a service
   taking a list of pairs.
   (The handler takes a parameter of type [(int * string) list]).

Note: We could make even more static checking in parameter's types (for example
to forbid [any] in suffixes), but it would make the types of parameters and
services more complicated. We believe that these errors should be easy
to find during implementation.

 *)

open Ocsigen_extensions

(** {2 Types used by the module} *)

type ('typ, +'suff, +'name) params_type
(** Type for parameters of a web page.
   - [ 'typ] is the type of the parameter (taken by the service handler)
   - [ 'suff] is a polymorphic variant type telling the type of parameter
   (suffix or not ...)
   - [ 'name] is the type of the parameter name (usually using {!Eliom_parameters.param_name})
 *)

type no_param_name
(** empty type used when it is not possible to use the parameter in a form *)

type 'a param_name
(** Type for names of page parameters (given to the functions
   to construct forms, for example in {!Eliom_output.XHTMLFORMSSIG.get_form}).
   The names of parameters are not just strings to enforce using
   forms widgets with the right parameter type.
   The parameter of that type is often a subtype of the polymorphic variant
   type [[ `Set of 'a | `One of 'a | `Radio of 'a ]], where
   - [`Set of 'a] means: any number of ['a]
   - [`One of 'a] means: exactly one ['a]
   - [`Radio of 'a] means: zero or one ['a]
 *)

type 'a setoneradio = [ `Set of 'a | `One of 'a | `Radio of 'a ]
(** This type is used by some form widgets like
   {!Eliom_output.XHTMLFORMSSIG.int_input} that may be used against services
   expecting one parameter of that name,
   or services expecting an optional parameter of that name,
   or services expecting any number of parameters of that name.
 *)

type 'a oneradio = [ `One of 'a | `Radio of 'a ]
(** This type is used by some form widgets like
   {!Eliom_output.XHTMLFORMSSIG.int_image_input} that may be used against services
   expecting one parameter of that name
   or services expecting an optional parameter of that name.
 *)

type 'a setone = [ `Set of 'a | `One of 'a ]
(** This type is used by some form widgets like
   {!Eliom_output.XHTMLFORMSSIG.int_button} that may be used against services
   expecting one parameter of that name,
   or services expecting any number of parameters of that name.
 *)


type ('a, 'b) binsum = Inj1 of 'a | Inj2 of 'b
(** Type used for parameters of type bynary sum *)

type 'an listnames =
    {it:'el 'a. ('an -> 'el -> 'a -> 'a) -> 'el list -> 'a -> 'a}
(** Type of the iterator used to construct forms from lists *)

(** {2 Basic types of pages parameters} *)

val int : string ->
  (int, [ `WithoutSuffix ], [ `One of int ] param_name) params_type
(** [int s] tells that the service takes an integer as parameter, labeled [s]. *)

val int32 : string ->
  (int32, [ `WithoutSuffix ], [ `One of int32 ] param_name) params_type
(** [int32 s] tells that the service takes a 32 bits integer as parameter, labeled [s]. *)

val int64 : string ->
  (int64, [ `WithoutSuffix ], [ `One of int64 ] param_name) params_type
(** [int64 s] tells that the service takes a 64 bits integer as parameter, labeled [s]. *)

val float : string ->
  (float, [ `WithoutSuffix ], [ `One of float ] param_name) params_type
(** [float s] tells that the service takes a floating point number as parameter, labeled [s]. *)

val string :
    string ->
      (string, [ `WithoutSuffix ], [ `One of string ] param_name) params_type
(** [string s] tells that the service takes a string as parameter, labeled [s]. *)

val bool :
    string ->
      (bool, [ `WithoutSuffix ], [ `One of bool ] param_name) params_type
(** [bool s] tells that the service takes a boolean as parameter, labeled [s].
   (to use for example with boolean checkboxes) *)

val file :
    string -> (Ocsigen_lib.file_info, [ `WithoutSuffix ],
               [ `One of Ocsigen_lib.file_info ] param_name) params_type
(** [file s] tells that the service takes a file as parameter, labeled [s]. *)

val unit : (unit, [ `WithoutSuffix ], unit) params_type
(** used for services that don't have any parameters *)

val user_type :
  of_string:(string -> 'a) ->
  to_string:('a -> string) ->
  string ->
  ('a, [ `WithoutSuffix ], [ `One of 'a ] param_name) params_type
(** Allows to use whatever type you want for a parameter of the service.
   [user_type t_of_string string_of_t s] tells that the service take a parameter, labeled [s], and that the server will have to use [t_of_string] and [string_of_t] to make the conversion from and to string.
 *)

(** [coordinates] is for the data sent by an [<input type="image" ...>]. *)
type coordinates =
    {abscissa: int;
     ordinate: int}

val coordinates :
    string ->
      (coordinates, [ `WithoutSuffix ],
       [ `One of coordinates ] param_name) params_type
(** [string s] tells that the service takes as parameters the coordinates
   of the point where the user were clicked on an image. *)

val string_coordinates :
    string ->
      (string * coordinates, [ `WithoutSuffix ],
       [ `One of (string * coordinates) ] param_name) params_type
(** It is possible to send a value together with the coordinates
   ([<input type="image" value="..." ...>]) (Here a string) *)

val int_coordinates :
    string ->
      (int * coordinates, [`WithoutSuffix],
       [ `One of (int * coordinates) ] param_name) params_type
(** Same for an integer value *)

val int32_coordinates :
    string ->
      (int32 * coordinates, [`WithoutSuffix],
       [ `One of (int32 * coordinates) ] param_name) params_type
(** Same for a 32 bits integer value *)

val int64_coordinates :
    string ->
      (int64 * coordinates, [`WithoutSuffix],
       [ `One of (int64 * coordinates) ] param_name) params_type
(** Same for a 64 integer value *)

val float_coordinates :
    string ->
      (float * coordinates, [`WithoutSuffix],
       [ `One of (float * coordinates) ] param_name) params_type
(** Same for a float value *)

val user_type_coordinates :
    of_string:(string -> 'a) -> to_string:('a -> string) -> string ->
      ('a * coordinates, [`WithoutSuffix],
       [ `One of ('a * coordinates) ] param_name) params_type
(** Same for a value of your own type *)

(** {2 Composing types of pages parameters} *)

val ( ** ) :
    ('a, [ `WithoutSuffix ], 'b) params_type ->
      ('c, [< `WithoutSuffix | `Endsuffix ] as 'e, 'd) params_type ->
        ('a * 'c, 'e, 'b * 'd) params_type
(** This is a combinator to allow the service to take several parameters
   (see examples above)
   {e Warning: it is a binary operator.
   Pages cannot take tuples but only pairs.}
 *)

val prod :
    ('a, [ `WithoutSuffix ], 'b) params_type ->
      ('c, [< `WithoutSuffix | `Endsuffix ] as 'e, 'd) params_type ->
        ('a * 'c, 'e, 'b * 'd) params_type
(** Same as [**] above *)

val sum :
    ('a, [ `WithoutSuffix ], 'b) params_type ->
      ('c, [ `WithoutSuffix ], 'd) params_type ->
        (('a, 'c) binsum, [ `WithoutSuffix ], 'b * 'd) params_type
(** This is a combinator to allow the service to take either a parameter
   or another one
   {e Warning: it is a binary operator.}
 *)

val opt :
    ('a, [ `WithoutSuffix ], 'b) params_type ->
      ('a option, [ `WithoutSuffix ], 'b) params_type
(** Use this if you want one or some parameters to be optional *)

val radio :
    (string ->
      ('a, [ `WithoutSuffix ], [ `One of 'b ] param_name) params_type) ->
        string ->
          ('a option, [ `WithoutSuffix ], [ `Radio of 'b ] param_name) params_type
(** Use this if you want to use this parameter with a radio button.
    It is equivalent to [opt] but works only for one single parameter.
*)

val any :
      ((string * string) list, [ `WithoutSuffix ], unit) params_type
(** Use this if you want to take any parameters.
   The service will answer to all the request,
   and get all parameters as an association list of strings.
 *)

val set :
    (string ->
      ('a, [ `WithoutSuffix ], [ `One of 'b ] param_name) params_type) ->
        string ->
          ('a list, [ `WithoutSuffix ], [ `Set of 'b ] param_name) params_type
(** Use this if you want your service to take several parameters
   with the same name. The service handler will receive a list of values.
   To create the form, just use the same name several times.
   For example [set int "i"] will match the parameter string
   [i=4&i=22&i=111] and send to the service handler a list containing
   the three integers 4, 22 and 111. The order is unspecified.
 *)

val list :
    string ->
      ('a, [ `WithoutSuffix ], 'b) params_type ->
        ('a list, [ `WithoutSuffix ], 'b listnames) params_type
(** The service takes a list of parameters.
   The first parameter of this function is the name of the list.
   The service handler will receive a list of values.
   To create the form, an iterator of type {!Eliom_parameters.listnames} is given to
   generate the name for each value.
 *)

val regexp :
    Netstring_pcre.regexp -> string -> to_string:(string -> string) -> string ->
      (string, [ `WithoutSuffix ],
       [` One of string ] param_name) params_type
(** [regexp r d s] tells that the service takes a string
   that matches the regular expression [r] as parameter,
   labeled [s], and that will be rewritten in d.
   The syntax of regexp is PCRE's one (uses [Netstring_pcre], from OCamlnet).
   For example: [regexp (Netstring_pcre.regexp "\\[(.* )\\]") "($1)" "myparam"]
   will match the parameter [myparam=[hello]] and send the string ["(hello)"] to
   the service handler.
 *)

val suffix :
  ?redirect_if_not_suffix:bool ->
  ('s, [< `WithoutSuffix | `Endsuffix ], 'sn) params_type ->
  ('s, [ `WithSuffix ], 'sn) params_type
(** Tells that the parameter of the service handler is
    the suffix of the URL of the current service.
    e.g. [suffix (int "i" ** string "s")] will match an URL ending by [380/yo].
    and send [(380, "yo")] to the service handler.

    For each service with suffix, there is also a service with regular
    parameters (without suffix) that will be used if you create a form
    towards a service with suffix.
    If [redirect_if_not_suffix] is [true] (default),
    this service without suffix will be redirected to the suffix version.
 *)

val all_suffix :
  string ->
  (string list, [`Endsuffix], [` One of string list ] param_name) params_type
(** Takes all the suffix, as long as possible, as a (slash separated)
   string list *)

val all_suffix_string :
  string -> (string, [`Endsuffix], [` One of string ] param_name) params_type
(** Takes all the suffix, as long as possible, as a string *)

val all_suffix_user :
  of_string:(string -> 'a) ->
  to_string:('a -> string) -> string ->
  ('a, [ `Endsuffix ], [` One of 'a ] param_name) params_type
    (** Takes all the suffix, as long as possible,
        with a type specified by the user. *)
    
val all_suffix_regexp :
  Netstring_pcre.regexp -> string -> to_string:(string -> string) -> string ->
      (string, [ `Endsuffix ], [` One of string ] param_name) params_type
(** [all_suffix_regexp r d s] takes all the suffix, as long as possible,
   matching the regular expression [r], name [s], and rewrite it in [d].
 *)

val suffix_prod :
  ?redirect_if_not_suffix:bool ->
  ('s,[<`WithoutSuffix|`Endsuffix],'sn) params_type ->
  ('a,[`WithoutSuffix], 'an) params_type ->
  (('s * 'a), [`WithSuffix], 'sn * 'an) params_type
(** Tells that the function that will generate the service takes
   a pair whose first element is the suffix of the URL of the current service,
   and the second element corresponds to other (regular) parameters.
   e.g.: [suffix_prod (int "suff" ** all_suffix "endsuff") (int "i")]
   will match an URL ending by [777/go/go/go?i=320] and send the value
   [((777, ["go";"go";"go"]), 320)] to the service handler.
 *)

val suffix_const :
    string ->
      (unit, [ `WithoutSuffix ], [ `One of unit ] param_name) params_type
(** [suffix_const v] is used only inside suffixes (do nothing for
    regular parameters).
    It tells that the service takes a constant parameter
    inside the suffix, whose value must be [v].
    It is used for putting constant directory names inside
    suffix parameters (and thus make suffix parameters not be only suffixes
    but anywhere you want in the path, e.g. [/param1/const/param2]).
*)

type 'a caml
(** marshaled OCaml values of type 'a *)

val caml :
  string ->
  'a Deriving_Json.t ->
  ('a, [ `WithoutSuffix ], [ `One of 'a caml ] param_name) params_type
(** [caml s] tells that the service is expecting some caml (client side)
    program to send some value of type 'a, marshaled.
    As usual [s] is the name of the parameter. *)

val raw_post_data :
  (((string * string) * (string * string) list) option *
      string Ocsigen_stream.t option,
   [ `WithoutSuffix ], no_param_name) params_type
(** When the content type is neither URLencoded form data or multipart data,
    it is possible to get it as a stream of strings.
    The first element of the pair is the content-type.
    This kind of parameter cannot be combined with other ones,
    and it is not possible to create a form towards a service taking
    this kind of parameter.
*)

val guard : (string -> ('a, 'b, [ `One of string] param_name) params_type) -> string
  -> ('a -> bool) -> ('a, 'b, [ `One of string] param_name) params_type
(** [guard construct name pred] returns the same parameter
    as [construct name] but with ensuring that each value must satisfy [pred].
    For instance: [int "age" ((>=) 0)] *)

(** {2 Non localized parameters} *)
type ('a, +'tipo, +'names) non_localized_params

val make_non_localized_parameters :
  prefix : string ->
  name : string ->
  ?persistent:bool ->
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a, [ `WithoutSuffix ], 'b) non_localized_params
(** create a new specification for non localized parameters.
    You must give a name to this set of parameters.
    Warning: the names must be unique for the whole application.
    That's why the name is composed by a prefix (the name of your project) and
    another string (the name of your non localized parameters).

    Will fail with exception [Failure _] if the name contains a dot.
    If [?persistent] is [true], the non localized parameter
    may remain if you call another service, if this service
    allows this (default [false]).
*)

val get_non_localized_get_parameters :
  ('a, [ `WithoutSuffix ], 'b) non_localized_params ->
  'a option
(** [get_non_localized_get_parameters ~sp p] decodes and 
    returns non localized GET parameters specified by [p] if present. *)

val get_non_localized_post_parameters :
  ('a, [ `WithoutSuffix ], 'b) non_localized_params ->
  'a option
(** [get_non_localized_post_parameters ~sp p] decodes and 
    returns non localized POST parameters specified by [p] if present. *)

(** Use this type to give non localized parameters to a link or a form *)
type nl_params_set

val empty_nl_params_set : nl_params_set

val add_nl_parameter : 
  nl_params_set ->
  ('a, [< `WithSuffix | `WithoutSuffix ], _) non_localized_params -> 
  'a -> nl_params_set

val get_nl_params_names : 
  (_, [< `WithSuffix | `WithoutSuffix ], 'a) non_localized_params -> 'a

(** Given a parameter type, get the two functions
    that converts from and to strings. You should
    only use this function on
    - options ;
    - basic types : int, int32, int64, float, string
    - marshal
    - unit
    - string
    - bool
 *)
val get_to_and_from : ('a, 'b, 'c) params_type -> (string -> 'a) * ('a -> string)


(**/**)

val walk_parameter_tree : [ `One of string ] param_name -> ('a, 'b, 'c) params_type
  -> ((string -> 'd) * ('d -> string)) option
val contains_suffix : ('a, 'b, 'c) params_type -> bool option
  (* None = no suffix. The bool means : redirect_if_not_suffix *)

val add_pref_params :
    string ->
      ('a, 'b, 'c) params_type ->
        ('a, 'b, 'c) params_type

val construct_params :
  (string * string) list Ocsigen_lib.String_Table.t ->
  ('a, [< `WithSuffix | `WithoutSuffix ], 'b) params_type ->
  'a -> string list option * string

val construct_params_string : (string * string) list -> string

val construct_params_list_raw :
  (string * string) list Ocsigen_lib.String_Table.t ->
  ('a, [< `WithSuffix | `WithoutSuffix ], 'b) params_type ->
  'a -> string list option * 
  (string * string) list Ocsigen_lib.String_Table.t *
     (string * string) list

val construct_params_list :
  (string * string) list Ocsigen_lib.String_Table.t ->
  ('a, [< `WithSuffix | `WithoutSuffix ], 'b) params_type ->
  'a -> 
  string list option * (string * string) list

val reconstruct_params :
  sp:Eliom_common.server_params ->
  ('a, [< `WithSuffix | `WithoutSuffix ], 'b) params_type ->
  (string * string) list Lwt.t option ->
  (string * Ocsigen_lib.file_info) list Lwt.t option -> 
  bool ->
  Ocsigen_lib.url_path option -> 'a Lwt.t

type anon_params_type = int

val anonymise_params_type : ('a, 'b, 'c) params_type -> anon_params_type

val make_params_names :
    ('a, 'b, 'c) params_type -> bool * 'c (* bool = contains a suffix *)

val string_of_param_name : 'a param_name -> string

val nl_prod : 
  ('a, 'su, 'an) params_type ->
  ('s, [ `WithoutSuffix ], 'sn) non_localized_params ->
  ('a * 's, 'su, 'an * 'sn) params_type 

val remove_from_nlp :
  (string * string) list Ocsigen_lib.String_Table.t ->
  ('a, [< `WithSuffix | `WithoutSuffix ], 'b) params_type ->
  (string * string) list Ocsigen_lib.String_Table.t

val table_of_nl_params_set : nl_params_set -> 
  (string * string) list Ocsigen_lib.String_Table.t

val list_of_nl_params_set : nl_params_set -> (string * string) list

val string_of_nl_params_set : nl_params_set -> string

val wrap_param_type : ('a, 'b, 'c) params_type -> ('a, 'b, 'c) params_type
