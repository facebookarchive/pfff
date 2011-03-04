(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_output
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

(** This modules contains predefined modules for generating forms and
   registering handlers, for several types of pages:
   XHTML pages typed with polymorphic variants,
   untyped (text) pages, actions, redirections, files ...
 *)

open Eliom_services
open Eliom_parameters
open Eliom_mkforms
open Eliom_mkreg


(** The signature of such modules. *)
module type ELIOMSIG = sig
  include Eliom_mkreg.ELIOMREGSIG
  include Eliom_mkforms.ELIOMFORMSIG
end


(** {2 Module for registering Xhtml pages typed with polymorphic variants using {!XHTML.M}} *)

(** {3 Creating links and forms with XHTML.M} *)

module type XHTMLFORMSSIG = sig

  open XHTML.M
  open Xhtmltypes

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
      a_content elt list -> 
      'get -> 
    [> a] XHTML.M.elt
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
      Xhtmltypes.option_attrib XHTML.M.attrib list
        * 'a (* Value to send *)
        * pcdata elt option (* Text to display (if different from the latter) *)
        * bool (* selected *)

  type 'a select_opt =
    | Optgroup of
        [ common | `Disabled ] XHTML.M.attrib list
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




(*
(* The sig include A end are a workaround not to have to install
   the clients .cmi *)
module type XHTMLFORMSSIG = sig include Eliom_output_cli.XHTMLFORMSSIG end
*)



(** {3 Forms and registration functions} *)

      (** Eliom forms and service registration functions for XHTML *)
module Xhtml : sig

  include Eliom_mkreg.ELIOMREGSIG with type page = Xhtmltypes.xhtml XHTML.M.elt 
                                  and type options = XHTML.M.doctypes
                                  and type return = Eliom_services.http

  include XHTMLFORMSSIG

end


module Xhtmlforms : XHTMLFORMSSIG
module Xhtmlreg : Eliom_mkreg.ELIOMREGSIG with type page = Xhtmltypes.xhtml XHTML.M.elt 
                                  and type options = XHTML.M.doctypes
                                  and type return = Eliom_services.http

module Xhtmlreg_ : 
  functor(Xhtml_content : 
            Ocsigen_http_frame.HTTP_CONTENT with type t = [ `Html ] XHTML.M.elt
                                            and type options = XHTML.M.doctypes
         ) -> Eliom_mkreg.REGCREATE with type page =  Xhtml_content.t
                                    and type options = XHTML.M.doctypes
                                    and type return = Eliom_services.http



      (** Eliom forms and service registration functions for XHTML, with
          compact markup (i.e., without pretty-printing). *)
module Xhtmlcompact : sig

  include Eliom_mkreg.ELIOMREGSIG with type page = Xhtmltypes.xhtml XHTML.M.elt
                                  and type options = XHTML.M.doctypes
                                  and type return = Eliom_services.http

  include XHTMLFORMSSIG

end

(** {2 Same for html 5} *)

(* include module type of Eliom_output_cli 
   does not work with camlp4 ... I replace by:
*)
module type XHTML5FORMSSIG = Eliom_output_cli.XHTML5FORMSSIG




module Xhtml5 : sig

  include Eliom_mkreg.ELIOMREGSIG 
  with type page = Xhtml5types.xhtml XHTML5.M.elt 
  and type options = XHTML5.M.doctypes
  and type return = Eliom_services.http

  include XHTML5FORMSSIG

end

module Xhtml5compact : sig

  include Eliom_mkreg.ELIOMREGSIG 
  with type page = Xhtml5types.xhtml XHTML5.M.elt
  and type options = XHTML5.M.doctypes
  and type return = Eliom_services.http

  include XHTML5FORMSSIG

end



(** {3 Eliom client/server applications} *)

(** Parameters for an Eliom application *)
type appl_service_params =
    {
      ap_doctype: XHTML5.M.doctypes;
      ap_title: string;
      ap_container : 'a.
        ((([< Xhtml5types.common ] as 'a) XHTML5.M.attrib list) option *
           (Xhtml5types.body_content XHTML5.M.elt ->
            Xhtml5types.body_content XHTML5.M.elt list))
        option;
      ap_body_attributes : 
        'a. (([< Xhtml5types.common ] as 'a) XHTML5.M.attrib list) option;
      ap_headers_before : Xhtml5types.head_content_fun XHTML5.M.elt list;
      (** Headers to be added before loading the main program *)
      ap_headers_after : Xhtml5types.head_content_fun XHTML5.M.elt list
      (** Headers to be added after loading the main program *)
    }

val default_appl_params : appl_service_params



module type APPL_PARAMS = sig

  (** Name of the application. 
      The name of the client side program must be the this name plus
      ".uue" suffix.
      Two distincts applications must have distincts names.
  *)
  val application_name : string

  val params : appl_service_params
end


(** Parameters for an Eliom application service *)
type appl_service_options =
    {
      do_not_launch : bool; (** Do not launch the client side program
                                if it is not already launched.
                                Default: [false]. *)
    }
(** 
    If you set do_not_launch to [true] for a service, it will send the page
    without launching the client side program if it is not already launched.
    Use this if some of your pages are not using the client side program,
    and you want to make them load faster (for example the main page).
*)

val default_appl_service_options : appl_service_options

module Eliom_appl (Appl_params : APPL_PARAMS) : sig
  include Eliom_mkreg.ELIOMREGSIG 
    with type page = Xhtml5types.body_content XHTML5.M.elt list
    and type options = appl_service_options
    and type return = Eliom_services.appl_service

  include XHTML5FORMSSIG

  (** Unique identifier for this application.
      It is the application name.
      Warning: do not mix up with the "application instance id",
      that is unique for each instance of the application.
  *)
  val application_name : string
end


module Xhtmlcompactreg : Eliom_mkreg.ELIOMREGSIG 
  with type page = Xhtmltypes.xhtml XHTML.M.elt
  and type options = XHTML.M.doctypes
  and type return = Eliom_services.http

(** {3 Module to register subpages of type [block]} *)
(** For XHTML *)
module Blocks : sig

  include Eliom_mkreg.ELIOMREGSIG with type page = Xhtmltypes.body_content XHTML.M.elt list
                                  and type options = unit
                                  and type return = Eliom_services.http
  include XHTMLFORMSSIG

end

(** For XHTML5 *)
module Blocks5 : sig

  include Eliom_mkreg.ELIOMREGSIG with type page = Xhtml5types.body_content XHTML5.M.elt list
                                  and type options = unit
                                  and type return = Eliom_services.http
  include XHTMLFORMSSIG

end
  (** Use this module for example for XMLHttpRequests for block tags (e.g. <div>) *)


(** {3 Functor to create modules to register subpages for other subtypes of XHTML} *)

module SubXhtml(Format : sig 
      type doctypes
      type content
      type 'a elt
      val xhtml_list_stream : ?version:doctypes -> ?width: int -> ?encode:(string->string) 
        -> ?html_compat : bool -> content elt list -> string Ocsigen_stream.t end) :
  sig

    include Eliom_mkreg.ELIOMREGSIG with type page = Format.content Format.elt list
    include XHTMLFORMSSIG

  end




(** {2 Untyped pages} *)

(** {3 Module to create forms and register untyped HTML pages} *)
module HtmlText : ELIOMSIG with
type page = string
and type form_content_elt = string
and type form_content_elt_list = string
and type form_elt = string
and type 'a a_content_elt = string
and type 'a a_content_elt_list = string
and type 'a a_elt = string
and type 'a a_elt_list = string
and type div_content_elt = string
and type div_content_elt_list = string
and type uri = string
and type link_elt = string
and type script_elt = string
and type textarea_elt = string
and type select_elt = string
and type input_elt = string
and type pcdata_elt = string
and type a_attrib_t = string
and type form_attrib_t = string
and type input_attrib_t = string
and type textarea_attrib_t = string
and type select_attrib_t = string
and type link_attrib_t = string
and type script_attrib_t = string
and type input_type_t = string

(** {3 Module to register untyped CSS pages} *)
module CssText : Eliom_mkreg.ELIOMREGSIG with
  type page = string
  and type return = Eliom_services.http

(** {3 Module to register untyped text pages} *)
module Text : Eliom_mkreg.ELIOMREGSIG with
  type page = string * string
  and type return = Eliom_services.http
(** The first string is the content, the second is the content type,
 for example "text/html" *)

(** {2 Other kinds of services} *)

(** Actions do not generate any page. They do something,
    then the page corresponding to the URL (without POST parameters
    or non-attached parameters or coservice parameters) is sent to the browser.

    If you want to give information to the handler that will be called
    to reload the page, put it in an Eliom reference with scope [`Request].

    If you give the optional parameter
    [~options:`NoReload] to the registration function, no page will be sent.
 *)
module Action : Eliom_mkreg.ELIOMREGSIG 
  with
    type page = unit
  and type options = [ `Reload | `NoReload ]
  and type return = Eliom_services.http


(** Like actions, but the page is not reloaded. Just do something and do
   not generate any page. To be used carefully. Probably not usefull at all.
   (Same as {!Eliom_output.Action} with [`NoReload] option).
 *)
module Unit : Eliom_mkreg.ELIOMREGSIG with
  type page = unit
  and type return = Eliom_services.http
(** Allows to create redirections towards another service.
   A 301 or 307 code is sent to the browser to ask it to redo the request to
   another URL.

   To choose if you want permanent or temporary redirection, use
   the [options] parameter of registration functions.
   For example: [register ~options:`Temporary ...].
*)
module Redirection : Eliom_mkreg.ELIOMREGSIG with
  type page =
  (unit, unit, Eliom_services.get_service_kind,
   [ `WithoutSuffix ],
   unit, unit, Eliom_services.registrable, Eliom_services.http)
    Eliom_services.service
  and type options = [ `Temporary | `Permanent ]
  and type return = Eliom_services.http

(** Allows to create redirections towards other URLs.
   A 301 or 307 code is sent to the browser to ask it to redo the request to
   another URL.

   Warning: The URL given must be an absolute URI.

   To choose if you want permanent or temporary redirection, use
   the [options] parameter of registration functions.
   For example: [register ~options:`Temporary ...].
 *)
module String_redirection : Eliom_mkreg.ELIOMREGSIG with
  type page = Xhtmltypes.uri
  and type options = [ `Temporary | `Permanent ]
  and type return = Eliom_services.http
(*VVV Would be better to define the type uri elsewhere *)

(** Allows to send files. The content is the name of the file to send. *)
module Files : Eliom_mkreg.ELIOMREGSIG with
  type page = string
  and type return = Eliom_services.http

(** Allows to create services that choose dynamically what they want
   to send. The content is created using one {!Eliom_mkreg.ELIOMREGSIG.send}
   function, for ex [Xhtml.send] or [Files.send].
   .
 *)
module Any : Eliom_mkreg.ELIOMREGSIG with
  type page = Ocsigen_http_frame.result
  and type return = Eliom_services.http

(** Allows to send raw data using Ocsigen's streams.
    The content is a pair containing:

    - a list of functions returning a stream and the
    function to close it,
    - the  content type string to send.

    Streams are opened by calling the functions in the list, and closed
    automatically by a call to the closing function.
    If something goes wrong, the current stream is closed,
    and the following are not opened.
 *)
module Streamlist : Eliom_mkreg.ELIOMREGSIG with
  type page = (((unit -> string Ocsigen_stream.t Lwt.t) list) *
                 string)
  and type return = Eliom_services.http


(** Allows to register services that send caml values.
    Note that this kind of services are most of the time
    POST coservices, and GET (co)services are probably useless here.
*)
module Caml : sig

  type options = unit

  val register :
    ?scope:Eliom_common.scope ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?state_name:string ->
    ?secure_session:bool ->
    service:('get, 'post,
             [< internal_service_kind ],
             [< suff ], 'gn, 'pn, [ `Registrable ], 
             'return Eliom_parameters.caml) service ->
    ?error_handler:((string * exn) list -> 'return Lwt.t) ->
    ('get -> 'post -> 'return Lwt.t) ->
    unit


  val register_service :
    ?scope:Eliom_common.scope ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?state_name:string ->
    ?secure_session:bool ->
    ?https:bool ->
    path:Ocsigen_lib.url_path ->
    get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
    ?error_handler:((string * exn) list -> 'return Lwt.t) ->
    ('get -> unit -> 'return Lwt.t) ->
    ('get, unit,
     [> `Attached of
        ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
     'tipo, 'gn, unit,
     [> `Registrable ], 'return Eliom_parameters.caml) service

  val register_coservice :
    ?scope:Eliom_common.scope ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?state_name:string ->
    ?secure_session:bool ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_state_name: string ->
    ?csrf_scope:Eliom_common.user_scope ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    fallback:(unit, unit,
              [ `Attached of ([ `Internal of [ `Service ] ], [`Get]) a_s ],
              [ `WithoutSuffix ] as 'tipo,
              unit, unit, [< registrable ], 'return Eliom_parameters.caml)
      service ->
    get_params:
      ('get, [`WithoutSuffix], 'gn) params_type ->
    ?error_handler:((string * exn) list -> 'return Lwt.t) ->
    ('get -> unit -> 'return Lwt.t) ->
    ('get, unit,
     [> `Attached of
        ([> `Internal of [> `Coservice ] ], [> `Get]) a_s ],
     'tipo, 'gn, unit,
     [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_coservice' :
    ?scope:Eliom_common.scope ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?state_name:string ->
    ?secure_session:bool ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_state_name: string ->
    ?csrf_scope:Eliom_common.user_scope ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    get_params:
      ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
    ?error_handler:((string * exn) list -> 'return Lwt.t) ->
    ('get -> unit -> 'return Lwt.t) ->
    ('get, unit,
     [> `Nonattached of [> `Get] na_s ],
     'tipo, 'gn, unit, [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_post_service :
    ?scope:Eliom_common.scope ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?state_name:string ->
    ?secure_session:bool ->
    ?https:bool ->
    fallback:('get, unit,
              [ `Attached of
                  ([ `Internal of
                       ([ `Service | `Coservice ] as 'kind) ], [`Get]) a_s ],
              [< suff ] as 'tipo, 'gn,
              unit, [< `Registrable ], 'return Eliom_parameters.caml)
      service ->
    post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
    ?error_handler:((string * exn) list ->
                      'return Lwt.t) ->
    ('get -> 'post -> 'return Lwt.t) ->
    ('get, 'post, [> `Attached of
                     ([> `Internal of 'kind ], [> `Post]) a_s ],
     'tipo, 'gn, 'pn, [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_post_coservice :
    ?scope:Eliom_common.scope ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?state_name:string ->
    ?secure_session:bool ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_state_name: string ->
    ?csrf_scope:Eliom_common.user_scope ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    fallback:('get, unit ,
              [ `Attached of
                  ([ `Internal of [< `Service | `Coservice ] ], [`Get]) a_s ],
              [< suff ] as 'tipo,
              'gn, unit, [< `Registrable ], 'return Eliom_parameters.caml)
      service ->
    post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
    ?error_handler:((string * exn) list -> 'return Lwt.t) ->
    ('get -> 'post -> 'return Lwt.t) ->
    ('get, 'post,
     [> `Attached of
        ([> `Internal of [> `Coservice ] ], [> `Post]) a_s ],
     'tipo, 'gn, 'pn, [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_post_coservice' :
    ?scope:Eliom_common.scope ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?state_name:string ->
    ?secure_session:bool ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_state_name: string ->
    ?csrf_scope:Eliom_common.user_scope ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?keep_get_na_params:bool ->
    ?https:bool ->
    post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
    ?error_handler:((string * exn) list -> 'return Lwt.t) ->
    (unit -> 'post -> 'return Lwt.t) ->
    (unit, 'post, [> `Nonattached of [> `Post] na_s ],
     [ `WithoutSuffix ], unit, 'pn,
     [> `Registrable ], 'return Eliom_parameters.caml)
      service


end





module Customize : functor (R : Eliom_mkreg.ELIOMREGSIG) -> 
  functor (T : sig type page val translate : page -> R.page Lwt.t end) ->
    Eliom_mkreg.ELIOMREGSIG with type page = T.page
                            and type options = R.options
                            and type return = R.return
