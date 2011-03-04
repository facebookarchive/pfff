(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_predefmod
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

open Eliom_services
open Eliom_parameters
open Eliom_mkforms


(** {3 Creating links and forms with XHTML5.M} *)

module type XHTML5FORMSSIG = sig

  open XHTML5.M
  open Xhtml5types

(** {2 Links and forms} *)


    val make_string_uri :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, unit,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      'get -> 
      string
(** Creates the string corresponding to the relative URL of a service applied to
    its GET parameters.

    If [absolute] is set to [true], or if there is a protocol change,
    the URL will be absolute.
    
    If [absolute_path] is set to [true], and [absolute] is [false],
    the URL will be absolute, but without [protocol://server:port].
    
    Default hostname is determined from the [Host] http header of the request
    (or the attribute of <host> tag in
    configuration file if the option [<usedefaulthostname/>] is set).
    Default port is the current port (or another port of the server if
    you are switching from or to https).
    But you can choose the hostname or port you want by setting 
    the optional [?hostname] and [?port] parameters here.

 *)

    val make_uri :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, unit,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string -> 
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      'get -> 
      uri
(** Creates the URL for a service.
    Like the [a] function, it may take extra parameters. *)

    val make_uri_components :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, unit,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string -> 
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      'get -> 
      string * (string * string) list * string option
(** Creates the URL for a service.
    Returns the path (as a string, encoded),
    the association list of get parameters (not encoded),
    and the fragment (not encoded, if any).
    Like the [a] function, it may take extra parameters. *)

    val make_post_uri_components :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      service:('get, 'post, [< post_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string -> 
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?keep_get_na_params:bool ->
      'get -> 
      'post ->
      string * (string * string) list * string option * (string * string) list
(** Like [make_uri_components], but also creates a table of post parameters. *)

(**/**)
    val make_proto_prefix :
      ?hostname:string ->
      ?port:int ->
      sp:Eliom_common.server_params option ->
      bool ->
      string
(** Creates the string corresponding to the beginning of the URL,
    containing the scheme (protocol), server and port number (if necessary).
 *)
(**/**)

    val a :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:a_attrib attrib list ->
      service:('get, unit, [< get_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?no_appl:bool ->
      'a elt list -> 
      'get -> 
    [> 'a a] XHTML5.M.elt
(** [a service cont ()] creates a link to [service].
   The text of
   the link is [cont]. For example [cont] may be something like
   [\[pcdata "click here"\]].

   The last  parameter is for GET parameters.
   For example [a service cont (42,"hello")]

   The [~a] optional parameter is used for extra attributes.

   The [~fragment] optional parameter is used for the "fragment" part
   of the URL, that is, the part after character "#".

    When possible, all links generated by Eliom are relative, for example
    to make easier the use with reverse proxies.
    But in case of protocol change (if you want to switch to https using
    [~https:true] for example, or if the service imposes https),
    absolute links will be generated. 
    In that case,
    default hostname is determined from the [Host] http header of the request
    (or the attribute of <host> tag in
    configuration file if the option [<usedefaulthostname/>] is set).
    Default port is the current port (or another port of the server if
    you are switching from or to https).
    But you can choose the hostname or port you want by setting 
    the optional [?hostname] and [?port] parameters here.
    These options have no effect for relative links.

    You can add non-localized parameters using the optional parameter
    [nl_params]. See {!Eliom_parameters.nl_params_set}.

    If [~keep_nl_params] is [`Persistent] (resp. [`All]),
    persistent (resp all) non localized GET parameters
    will be kept in the URL (default is the default for the service).

    If a client side application is running, and unless
    [~no_appl:true] is specified, it will use [<a onclick=...>]
    instead of [<a href=...>] in case of link inside a same Eliom application.
    Thus, the client side application will not be stopped when the link
    is clicked.

*)

    val css_link : ?a:link_attrib attrib list -> uri:uri -> unit -> [>link] elt
(** Creates a [<link>] tag for a Cascading StyleSheet (CSS). *)

    val js_script :
        ?a:script_attrib attrib list -> uri:uri -> unit -> [>script] elt
(** Creates a [<script>] tag to add a javascript file *)


    val get_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ],
               [<suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?no_appl:bool ->
      ('gn -> form_content elt list) -> 
      [>form] elt
(** [get_form service formgen] creates a GET form to [service].
   The content of
   the form is generated by the function [formgen], that takes the names
   of the service parameters as parameters. *)

    val lwt_get_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, unit, [< get_service_kind ],
               [<suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?no_appl:bool ->
      ('gn -> form_content elt list Lwt.t) -> 
      [>form] elt Lwt.t
(** The same but taking a cooperative function. *)


    val post_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, 'post, [< post_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?keep_get_na_params:bool ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?no_appl:bool ->
      ('pn -> form_content elt list) -> 
      'get -> 
      [>form] elt
(** [post_form service formgen] creates a POST form to [service].
    The last parameter is for GET parameters (as in the function [a]).

    If [~keep_nl_params] is [`Persistent] (resp. [`All]),
    persistent (resp all) non localized GET parameters
    will be kept in the URL (default is the default for the service).

 *)

    val lwt_post_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib attrib list ->
      service:('get, 'post, [< post_service_kind ],
               [< suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?keep_get_na_params:bool ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?no_appl:bool ->
      ('pn -> form_content elt list Lwt.t) -> 
      'get -> 
      [>form] elt Lwt.t
(** The same but taking a cooperative function. *)







(** {2 Form widgets} *)

  type basic_input_type =
      [
    | `Hidden
    | `Password
    | `Submit
    | `Text ]

  val int_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< int setoneradio ] param_name ->
          ?value:int -> unit -> [> input ] elt
(** Creates an [<input>] tag for an integer *)

  val int32_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< int32 setoneradio ] param_name ->
          ?value:int32 -> unit -> [> input ] elt
(** Creates an [<input>] tag for a 32 bits integer *)

  val int64_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< int64 setoneradio ] param_name ->
          ?value:int64 -> unit -> [> input ] elt
(** Creates an [<input>] tag for a 64 bits integer *)

  val float_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< float setoneradio ] param_name ->
          ?value:float -> unit -> [> input ] elt
(** Creates an [<input>] tag for a float *)

  val string_input :
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< string setoneradio ] param_name ->
          ?value:string -> unit -> [> input ] elt
(** Creates an [<input>] tag for a string *)

  val user_type_input :
    ('a -> string) -> 
      ?a:input_attrib attrib list -> input_type:[< basic_input_type ] ->
        ?name:[< 'a setoneradio ] param_name ->
          ?value:'a -> unit -> [> input ] elt
(** Creates an [<input>] tag for a user type *)

  val raw_input :
      ?a:input_attrib attrib list ->
        input_type:[< basic_input_type | `Reset | `Button ] ->
        ?name:string -> ?value:string -> unit -> [> input ] elt
(** Creates an untyped [<input>] tag. You may use the name you want
   (for example to use with {!Eliom_parameters.any}).
 *)

  val file_input :
      ?a:input_attrib attrib list ->
        name:[< Ocsigen_lib.file_info setoneradio ] param_name ->
          unit -> [> input ] elt
(** Creates an [<input>] tag for sending a file *)

  val image_input :
      ?a:input_attrib attrib list ->
        name:[< coordinates oneradio ] param_name ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="...">] tag that sends the coordinates
   the user clicked on *)

  val int_image_input :
      ?a:input_attrib attrib list ->
        name:[< (int * coordinates) oneradio ] param_name -> value:int ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int *)

  val int32_image_input :
      ?a:input_attrib attrib list ->
        name:[< (int32 * coordinates) oneradio ] param_name -> value:int32 ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int32 *)

  val int64_image_input :
      ?a:input_attrib attrib list ->
        name:[< (int64 * coordinates) oneradio ] param_name -> value:int64 ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int64 *)

  val float_image_input :
      ?a:input_attrib attrib list ->
        name:[< (float * coordinates) oneradio ] param_name -> value:float ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type float *)

  val string_image_input :
      ?a:input_attrib attrib list ->
        name:[< (string * coordinates) oneradio ] param_name -> value:string ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type string *)

  val user_type_image_input :
    ('a -> string) -> 
      ?a:input_attrib attrib list ->
        name:[< ('a * coordinates) oneradio ] param_name -> value:'a ->
          ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of user defined type *)

  val raw_image_input :
      ?a:input_attrib attrib list ->
        name:string -> value:string -> ?src:uri -> unit -> [> input ] elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and an untyped value *)


  val bool_checkbox :
      ?a:input_attrib attrib list -> ?checked:bool ->
        name:[ `One of bool ] param_name -> unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a boolean value.
   The service must declare a [bool] parameter.
 *)

    val int_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of int ] param_name -> value:int ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have an int value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val int32_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of int32 ] param_name -> value:int32 ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have an int32 value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val int64_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of int64 ] param_name -> value:int64 ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have an int64 value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val float_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of float ] param_name -> value:float ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a float value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)


    val string_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of string ] param_name -> value:string ->
            unit -> [> input ] elt
(** Creates a checkbox [<input>] tag that will have a string value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)


    val user_type_checkbox :
      ('a -> string) -> 
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:[ `Set of 'a ] param_name -> value:'a -> unit -> 
            [> input ] elt
(** Creates a checkbox [<input>] tag that will have a "user type" value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)


    val raw_checkbox :
        ?a:input_attrib attrib list -> ?checked:bool ->
          name:string -> value:string -> unit -> [> input ] elt
(** Creates a checkbox [<input>] tag with untyped content.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [any].
 *)




  val string_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
    name:[ `Radio of string ] param_name -> value:string -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with string content *)

  val int_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of int ] param_name -> value:int -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with int content *)

  val int32_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of int32 ] param_name -> value:int32 -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with int32 content *)

  val int64_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of int64 ] param_name -> value:int64 -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with int64 content *)

  val float_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
     name:[ `Radio of float ] param_name -> value:float -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with float content *)

  val user_type_radio : 
    ('a -> string) -> ?a:(input_attrib attrib list ) -> ?checked:bool ->
    name:[ `Radio of 'a ] param_name -> value:'a -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with user_type content *)

  val raw_radio : ?a:(input_attrib attrib list ) -> ?checked:bool ->
    name:string -> value:string -> unit -> [> input ] elt
(** Creates a radio [<input>] tag with untyped string content (low level) *)


  type button_type =
      [ `Button | `Reset | `Submit ]

  val string_button : ?a:button_attrib attrib list ->
    name:[< string setone ] param_name -> value:string ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with string content *)

  val int_button : ?a:button_attrib attrib list ->
    name:[< int setone ] param_name -> value:int ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with int content *)

  val int32_button : ?a:button_attrib attrib list ->
    name:[< int32 setone ] param_name -> value:int32 ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with int32 content *)

  val int64_button : ?a:button_attrib attrib list ->
    name:[< int64 setone ] param_name -> value:int64 ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with int64 content *)

  val float_button : ?a:button_attrib attrib list ->
    name:[< float setone ] param_name -> value:float ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with float content *)

  val user_type_button : ('a -> string) -> ?a:button_attrib attrib list ->
    name:[< 'a setone ] param_name -> value:'a ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with user_type content *)

  val raw_button : ?a:button_attrib attrib list ->
    button_type:[< button_type ] ->
      name:string -> value:string ->
        button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with untyped string content *)

  val button : ?a:button_attrib attrib list ->
    button_type:[< button_type ] ->
      button_content elt list -> [> button ] elt
(** Creates a [<button>] tag with no value. No value is sent. *)



  val textarea :
      ?a:textarea_attrib attrib list ->
        name:[< string setoneradio ] param_name ->
          ?value:string ->
            rows:int -> cols:int ->
              unit -> [> textarea ] elt
(** Creates a [<textarea>] tag *)

  val raw_textarea :
      ?a:textarea_attrib attrib list ->
        name:string ->
          ?value:string ->
            rows:int -> cols:int ->
              unit -> [> textarea ] elt
(** Creates a [<textarea>] tag for untyped form *)

  type 'a soption =
      Xhtml5types.option_attrib XHTML5.M.attrib list
        * 'a (* Value to send *)
        * pcdata elt option (* Text to display (if different from the latter) *)
        * bool (* selected *)

  type 'a select_opt =
    | Optgroup of
        [ common | `Disabled ] XHTML5.M.attrib list
          * string (* label *)
          * 'a soption
          * 'a soption list
    | Option of 'a soption

  (** The type for [<select>] options and groups of options.
     - The field of type 'a in [soption] is the value that will be sent
     by the form.
     - If the [pcdata elt option] is not present it is also the
     value displayed.
     - The string in [select_opt] is the label
   *)

  val int_select :
      ?a:select_attrib attrib list ->
        name:[< `One of int ] param_name ->
          int select_opt ->
            int select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int values. *)

  val int32_select :
      ?a:select_attrib attrib list ->
        name:[< `One of int32 ] param_name ->
          int32 select_opt ->
            int32 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int32 values. *)

  val int64_select :
      ?a:select_attrib attrib list ->
        name:[< `One of int64 ] param_name ->
          int64 select_opt ->
            int64 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int64 values. *)

  val float_select :
      ?a:select_attrib attrib list ->
        name:[< `One of float ] param_name ->
          float select_opt ->
            float select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for float values. *)

  val string_select :
      ?a:select_attrib attrib list ->
        name:[< `One of string ] param_name ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for string values. *)

  val user_type_select :
    ('a -> string) -> 
      ?a:select_attrib attrib list ->
        name:[< `One of 'a ] param_name ->
          'a select_opt ->
            'a select_opt list ->
                [> select ] elt
(** Creates a [<select>] tag for user type values. *)

  val raw_select :
      ?a:select_attrib attrib list ->
        name:string ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for any (untyped) value. *)


  val int_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of int ] param_name ->
          int select_opt ->
            int select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int values. *)

  val int32_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of int32 ] param_name ->
          int32 select_opt ->
            int32 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int32 values. *)

  val int64_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of int64 ] param_name ->
          int64 select_opt ->
            int64 select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for int64 values. *)

  val float_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of float ] param_name ->
          float select_opt ->
            float select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for float values. *)

  val string_multiple_select :
      ?a:select_attrib attrib list ->
        name:[< `Set of string ] param_name ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for string values. *)

  val user_type_multiple_select :
    ('a -> string) -> 
      ?a:select_attrib attrib list ->
        name:[< `Set of 'a ] param_name ->
          'a select_opt ->
            'a select_opt list ->
                [> select ] elt
(** Creates a [<select>] tag for user type values. *)

  val raw_multiple_select :
      ?a:select_attrib attrib list ->
        name:string ->
          string select_opt ->
            string select_opt list ->
              [> select ] elt
(** Creates a [<select>] tag for any (untyped) value. *)


end


module Xhtml5forms : XHTML5FORMSSIG

(**/**)

type full_input_type =
  [ `Button
  | `Checkbox
  | `File
  | `Hidden
  | `Image
  | `Password
  | `Radio
  | `Reset
  | `Submit
  | `Text ]

type button_type =
  [ `Button
  | `Reset
  | `Submit
  ]

module Xhtml5forms_ : FORMCREATE
  with type form_content_elt = Xhtml5types.form_content XHTML5.M.elt
  and type form_content_elt_list = Xhtml5types.form_content XHTML5.M.elt list
  and type uri = Xhtml5types.uri
  and type 'a a_content_elt = 'a XHTML5.M.elt
  and type 'a a_content_elt_list = 'a XHTML5.M.elt list
        
  and type div_content_elt = Xhtml5types.div_content XHTML5.M.elt
  and type div_content_elt_list = Xhtml5types.div_content XHTML5.M.elt list
        
  and type 'a a_elt = 'a Xhtml5types.a XHTML5.M.elt
  and type 'a a_elt_list = 'a Xhtml5types.a XHTML5.M.elt list
  and type form_elt = Xhtml5types.form XHTML5.M.elt
        
  and type textarea_elt = Xhtml5types.textarea XHTML5.M.elt
  and type input_elt = Xhtml5types.input XHTML5.M.elt

  and type link_elt = Xhtml5types.link XHTML5.M.elt
  and type script_elt = Xhtml5types.script XHTML5.M.elt

  and type pcdata_elt = Xhtml5types.pcdata XHTML5.M.elt

  and type select_elt = Xhtml5types.select XHTML5.M.elt
  and type select_content_elt = Xhtml5types.select_content XHTML5.M.elt
  and type select_content_elt_list = Xhtml5types.select_content XHTML5.M.elt list
  and type option_elt = Xhtml5types.selectoption XHTML5.M.elt
  and type option_elt_list = Xhtml5types.selectoption XHTML5.M.elt list

  and type button_elt = Xhtml5types.button XHTML5.M.elt
  and type button_content_elt = Xhtml5types.button_content XHTML5.M.elt
  and type button_content_elt_list = Xhtml5types.button_content XHTML5.M.elt list

  and type a_attrib_t = Xhtml5types.a_attrib XHTML5.M.attrib list
  and type form_attrib_t = Xhtml5types.form_attrib XHTML5.M.attrib list
  and type input_attrib_t = Xhtml5types.input_attrib XHTML5.M.attrib list
  and type textarea_attrib_t = Xhtml5types.textarea_attrib XHTML5.M.attrib list
  and type select_attrib_t = Xhtml5types.select_attrib XHTML5.M.attrib list
  and type link_attrib_t = Xhtml5types.link_attrib XHTML5.M.attrib list
  and type script_attrib_t = Xhtml5types.script_attrib XHTML5.M.attrib list
  and type optgroup_attrib_t = [ Xhtml5types.common | `Disabled ] XHTML5.M.attrib list
  and type option_attrib_t = Xhtml5types.option_attrib XHTML5.M.attrib list
  and type button_attrib_t = Xhtml5types.button_attrib XHTML5.M.attrib list
  and type input_type_t = full_input_type
  and type button_type_t = button_type

module MakeXhtml5forms : functor (Xhtml5forms' : ELIOMFORMSIG
           with type form_content_elt = Xhtml5types.form_content XHTML5.M.elt
           and type form_content_elt_list = Xhtml5types.form_content XHTML5.M.elt list
           and type uri = Xhtml5types.uri
           and type 'a a_content_elt = 'a XHTML5.M.elt
           and type 'a a_content_elt_list = 'a XHTML5.M.elt list

           and type div_content_elt = Xhtml5types.div_content XHTML5.M.elt
           and type div_content_elt_list = Xhtml5types.div_content XHTML5.M.elt list

           and type 'a a_elt = 'a Xhtml5types.a XHTML5.M.elt
           and type 'a a_elt_list = 'a Xhtml5types.a XHTML5.M.elt list
           and type form_elt = Xhtml5types.form XHTML5.M.elt

           and type textarea_elt = Xhtml5types.textarea XHTML5.M.elt
           and type input_elt = Xhtml5types.input XHTML5.M.elt

           and type link_elt = Xhtml5types.link XHTML5.M.elt
           and type script_elt = Xhtml5types.script XHTML5.M.elt

           and type pcdata_elt = Xhtml5types.pcdata XHTML5.M.elt

           and type select_elt = Xhtml5types.select XHTML5.M.elt
           and type select_content_elt = Xhtml5types.select_content XHTML5.M.elt
           and type select_content_elt_list = Xhtml5types.select_content XHTML5.M.elt list
           and type option_elt = Xhtml5types.selectoption XHTML5.M.elt
           and type option_elt_list = Xhtml5types.selectoption XHTML5.M.elt list

           and type button_elt = Xhtml5types.button XHTML5.M.elt
           and type button_content_elt = Xhtml5types.button_content XHTML5.M.elt
           and type button_content_elt_list = Xhtml5types.button_content XHTML5.M.elt list

           and type a_attrib_t = Xhtml5types.a_attrib XHTML5.M.attrib list
           and type form_attrib_t = Xhtml5types.form_attrib XHTML5.M.attrib list
           and type input_attrib_t = Xhtml5types.input_attrib XHTML5.M.attrib list
           and type textarea_attrib_t = Xhtml5types.textarea_attrib XHTML5.M.attrib list
           and type select_attrib_t = Xhtml5types.select_attrib XHTML5.M.attrib list
           and type link_attrib_t = Xhtml5types.link_attrib XHTML5.M.attrib list
           and type script_attrib_t = Xhtml5types.script_attrib XHTML5.M.attrib list
           and type optgroup_attrib_t = [ Xhtml5types.common | `Disabled ] XHTML5.M.attrib list
           and type option_attrib_t = Xhtml5types.option_attrib XHTML5.M.attrib list
           and type button_attrib_t = Xhtml5types.button_attrib XHTML5.M.attrib list
           and type input_type_t = full_input_type
           and type button_type_t = button_type
) -> XHTML5FORMSSIG
