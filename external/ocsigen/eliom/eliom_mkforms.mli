(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkforms
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


(** This module defines the functor to use to creates modules
   generating form widgets for your own types of pages.
   It is used for example in {!Eliom_output}.
 *)


open Lwt
open Eliom_parameters
open Eliom_services


module type FORMCREATE =
  sig
    type form_content_elt
    type form_content_elt_list
    type form_elt
    type 'a a_content_elt
    type 'a a_content_elt_list
    type 'a a_elt
    type 'a a_elt_list
    type div_content_elt
    type div_content_elt_list
    type uri
    type link_elt
    type script_elt
    type textarea_elt
    type input_elt
    type pcdata_elt
    type select_elt
    type select_content_elt
    type select_content_elt_list
    type option_elt
    type option_elt_list
    type button_elt
    type button_content_elt
    type button_content_elt_list

    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type select_attrib_t
    type link_attrib_t
    type script_attrib_t
    type optgroup_attrib_t
    type option_attrib_t
    type button_attrib_t


    type input_type_t
    type button_type_t




    val hidden : input_type_t
    val checkbox : input_type_t
    val radio : input_type_t
    val submit : input_type_t
    val file : input_type_t
    val image : input_type_t

    val buttonsubmit : button_type_t

    val empty_seq : form_content_elt_list
    val cons_form :
        form_content_elt -> form_content_elt_list -> form_content_elt_list
    val map_option :
        ('a -> option_elt) -> 'a list ->
          option_elt_list
    val map_optgroup :
        ('a -> select_content_elt) -> 'a -> 'a list ->
          (select_content_elt * select_content_elt_list)
    val select_content_of_option : option_elt -> select_content_elt

    val make_pcdata : string -> pcdata_elt
    val make_a : ?a:a_attrib_t -> ?href:string -> ?onclick:XML.event ->
      'a a_content_elt_list -> 'a a_elt
    val make_get_form : ?a:form_attrib_t ->
      action:string ->
      ?onsubmit:XML.event ->
      form_content_elt -> form_content_elt_list -> form_elt
    val make_post_form : ?a:form_attrib_t ->
      action:string ->
      ?onsubmit:XML.event ->
      ?id:string -> ?inline:bool ->
      form_content_elt -> form_content_elt_list -> form_elt
    val make_hidden_field : input_elt option -> form_content_elt
    val remove_first :
        form_content_elt_list -> form_content_elt * form_content_elt_list
    val make_input : ?a:input_attrib_t -> ?checked:bool ->
      typ:input_type_t -> ?name:string -> ?src:uri ->
        ?value:string -> unit -> input_elt
    val make_button : ?a:button_attrib_t -> button_type:button_type_t ->
      ?name:string -> ?value:string ->
        button_content_elt_list -> button_elt
    val make_textarea :
        ?a:textarea_attrib_t ->
          name:string -> ?value:string -> rows:int -> cols:int ->
            unit -> textarea_elt
    val make_select :
        ?a:select_attrib_t ->
          multiple:bool ->
            name:string ->
              select_content_elt ->
                select_content_elt_list ->
                  select_elt
    val make_option :
        ?a:option_attrib_t ->
          selected:bool ->
            ?value:string ->
              pcdata_elt ->
                option_elt
    val make_optgroup :
        ?a:optgroup_attrib_t ->
          label:string ->
            option_elt ->
              option_elt_list ->
                select_content_elt
    val uri_of_string : string -> uri


    val make_css_link : ?a:link_attrib_t -> uri:uri -> unit -> link_elt

    val make_js_script : ?a:script_attrib_t -> uri:uri -> unit -> script_elt

    val register_event_a : 'elt a_elt -> string -> ('a -> unit Lwt.t) -> 'a -> unit
    val register_event_form : form_elt -> string -> ('a -> unit Lwt.t) -> 'a -> unit

    val add_tab_cookies_to_get_form : form_elt -> unit -> unit Lwt.t
    val add_tab_cookies_to_post_form : form_elt -> unit -> unit Lwt.t
    val add_tab_cookies_to_get_form_id_string : string
    val add_tab_cookies_to_post_form_id_string : string

    val appl_name : string option (* The application name, if any
                                     (for Eliom_appl only, None otherwise) *)

  end

module type ELIOMFORMSIG =
  sig


    type form_content_elt
    type form_content_elt_list
    type form_elt
    type 'a a_content_elt
    type 'a a_content_elt_list
    type 'a a_elt
    type 'a a_elt_list
    type div_content_elt
    type div_content_elt_list
    type uri
    type link_elt
    type script_elt
    type textarea_elt
    type input_elt
    type pcdata_elt
    type select_elt
    type select_content_elt
    type select_content_elt_list
    type option_elt
    type option_elt_list
    type button_elt
    type button_content_elt
    type button_content_elt_list

    type a_attrib_t
    type form_attrib_t
    type input_attrib_t
    type textarea_attrib_t
    type select_attrib_t
    type link_attrib_t
    type script_attrib_t
    type optgroup_attrib_t
    type option_attrib_t
    type button_attrib_t

    type input_type_t
    type button_type_t

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
      ?a:a_attrib_t ->
      service:('get, unit, [< Eliom_services.get_service_kind ],
               [< Eliom_services.suff ], 'd, 'e,
               [< Eliom_services.registrable ], 'f)
        Eliom_services.service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?no_appl:bool ->
      'a a_content_elt_list -> 
      'get -> 
      'a a_elt
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

    val css_link : ?a:link_attrib_t -> uri:uri -> unit -> link_elt
(** Creates a [<link>] tag for a Cascading StyleSheet (CSS). *)

    val js_script :
        ?a:script_attrib_t -> uri:uri -> unit -> script_elt
(** Creates a [<script>] tag to add a javascript file *)


    val get_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib_t ->
      service:('get, unit, [< get_service_kind ],
               [<suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?no_appl:bool ->
      ('gn -> form_content_elt_list) -> 
      form_elt
(** [get_form service formgen] creates a GET form to [service].
   The content of
   the form is generated by the function [formgen], that takes the names
   of the service parameters as parameters. *)

    val lwt_get_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib_t ->
      service:('get, unit, [< get_service_kind ],
               [<suff ], 'gn, 'pn,
               [< registrable ], 'return) service ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `Persistent | `None ] ->
      ?nl_params: Eliom_parameters.nl_params_set ->
      ?no_appl:bool ->
      ('gn -> form_content_elt_list Lwt.t) -> 
      form_elt Lwt.t
(** The same but taking a cooperative function. *)


    val post_form :
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:form_attrib_t ->
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
      ('pn -> form_content_elt_list) -> 
      'get -> 
      form_elt
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
      ?a:form_attrib_t ->
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
      ('pn -> form_content_elt_list Lwt.t) -> 
      'get -> 
      form_elt Lwt.t
(** The same but taking a cooperative function. *)





(** {2 Form widgets} *)


    val int_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< int setoneradio ] param_name ->
            ?value:int -> unit -> input_elt
(** Creates an [<input>] tag for an integer *)

    val int32_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< int32 setoneradio ] param_name ->
            ?value:int32 -> unit -> input_elt
(** Creates an [<input>] tag for an integer *)

    val int64_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< int64 setoneradio ] param_name ->
            ?value:int64 -> unit -> input_elt
(** Creates an [<input>] tag for an integer *)

    val float_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< float setoneradio ] param_name ->
            ?value:float -> unit -> input_elt
(** Creates an [<input>] tag for a float *)

    val string_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
           ?name:[< string setoneradio ] param_name ->
             ?value:string -> unit -> input_elt
(** Creates an [<input>] tag for a string *)

    val user_type_input : ('a -> string) ->
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:[< 'a setoneradio ] param_name ->
            ?value:'a -> unit -> input_elt
(** Creates an [<input>] tag for a user type *)

    val raw_input :
        ?a:input_attrib_t -> input_type:input_type_t ->
          ?name:string -> ?value:string -> unit -> input_elt
(** Creates an untyped [<input>] tag. You may use the name you want
   (for example to use with {!Eliom_parameters.any}).
 *)

    val file_input :
        ?a:input_attrib_t ->
          name:[< Ocsigen_lib.file_info setoneradio ] param_name ->
            unit -> input_elt
(** Creates an [<input>] tag for sending a file *)

    val image_input :
        ?a:input_attrib_t ->
          name:[< coordinates oneradio ] param_name ->
          ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="...">] tag that sends the coordinates
   the user clicked on *)

    val int_image_input :
        ?a:input_attrib_t ->
          name:[< (int * coordinates) oneradio ] param_name -> value:int ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int *)

    val int32_image_input :
        ?a:input_attrib_t ->
          name:[< (int32 * coordinates) oneradio ] param_name -> value:int32 ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int32 *)

    val int64_image_input :
        ?a:input_attrib_t ->
          name:[< (int64 * coordinates) oneradio ] param_name -> value:int64 ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type int64 *)

    val float_image_input :
        ?a:input_attrib_t ->
          name:[< (float * coordinates) oneradio ] param_name -> value:float ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
    the coordinates the user clicked on and a value of type float *)

    val string_image_input :
        ?a:input_attrib_t ->
          name:[< (string * coordinates) oneradio ] param_name -> value:string ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of type string *)

    val user_type_image_input : ('a -> string) ->
        ?a:input_attrib_t ->
          name:[< ('a * coordinates) oneradio ] param_name -> value:'a ->
            ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and a value of user defined type *)

    val raw_image_input :
        ?a:input_attrib_t ->
          name:string -> value:string -> ?src:uri -> unit -> input_elt
(** Creates an [<input type="image" name="..." value="...">] tag that sends
   the coordinates the user clicked on and an untyped value *)


    val bool_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `One of bool ] param_name -> unit -> input_elt
(** Creates a checkbox [<input>] tag that will have a boolean value.
   The service must declare a [bool] parameter.
 *)


    val int_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of int ] param_name -> value:int -> unit -> input_elt
(** Creates a checkbox [<input>] tag that will have an int value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val int32_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of int32 ] param_name -> value:int32 -> unit -> input_elt
(** Creates a checkbox [<input>] tag that will have an int32 value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val int64_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of int64 ] param_name -> value:int64 -> unit -> input_elt
(** Creates a checkbox [<input>] tag that will have an int64 value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val float_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of float ] param_name -> value:float -> unit -> input_elt
(** Creates a checkbox [<input>] tag that will have a float value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val string_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of string ] param_name -> value:string ->
            unit -> input_elt
(** Creates a checkbox [<input>] tag that will have a string value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val user_type_checkbox : ('a -> string) ->
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Set of 'a ] param_name -> value:'a ->
            unit -> input_elt
(** Creates a checkbox [<input>] tag that will have a "user type" value.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [set].
 *)

    val raw_checkbox :
        ?a:input_attrib_t -> ?checked:bool ->
          name:string -> value:string -> unit -> input_elt
(** Creates a checkbox [<input>] tag with untyped content.
   Thus you can do several checkboxes with the same name
   (and different values).
   The service must declare a parameter of type [any].
 *)


    val string_radio :
        ?a:input_attrib_t -> ?checked:bool ->
          name:[ `Radio of string ] param_name ->
            value:string -> unit -> input_elt
(** Creates a radio [<input>] tag with string content *)

    val int_radio :
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Radio of int ] param_name ->
             value:int -> unit -> input_elt
(** Creates a radio [<input>] tag with int content *)

    val int32_radio :
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Radio of int32 ] param_name ->
             value:int32 -> unit -> input_elt
(** Creates a radio [<input>] tag with int32 content *)

    val int64_radio :
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Radio of int64 ] param_name ->
             value:int64 -> unit -> input_elt
(** Creates a radio [<input>] tag with int64 content *)

    val float_radio :
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Radio of float ] param_name ->
             value:float -> unit -> input_elt
(** Creates a radio [<input>] tag with float content *)

    val user_type_radio : ('a -> string) ->
        ?a:input_attrib_t -> ?checked:bool ->
           name:[ `Radio of 'a ] param_name ->
             value:'a -> unit -> input_elt
(** Creates a radio [<input>] tag with user_type content *)

    val raw_radio :
        ?a:input_attrib_t -> ?checked:bool ->
          name:string -> value:string -> unit -> input_elt
(** Creates a radio [<input>] tag with untyped string content (low level) *)


    val string_button :
        ?a:button_attrib_t ->
          name:[< string setone ] param_name -> value:string ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with string content *)

    val int_button :
        ?a:button_attrib_t ->
          name:[< int setone ] param_name -> value:int ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with int content *)

    val int32_button :
        ?a:button_attrib_t ->
          name:[< int32 setone ] param_name -> value:int32 ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with int32 content *)

    val int64_button :
        ?a:button_attrib_t ->
          name:[< int64 setone ] param_name -> value:int64 ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with int64 content *)

    val float_button :
        ?a:button_attrib_t ->
          name:[< float setone ] param_name -> value:float ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with float content *)

    val user_type_button : ('a -> string) ->
        ?a:button_attrib_t ->
          name:[< 'a setone ] param_name -> value:'a ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with user_type content *)

    val raw_button :
        ?a:button_attrib_t ->
          button_type:button_type_t ->
            name:string -> value:string ->
              button_content_elt_list -> button_elt
(** Creates a [<button>] tag with untyped string content *)

    val button :
        ?a:button_attrib_t ->
          button_type:button_type_t ->
            button_content_elt_list -> button_elt
(** Creates a [<button>] tag with no value. No value is sent. *)




    val textarea :
        ?a:textarea_attrib_t ->
          name:[< string setoneradio ] param_name -> ?value:string ->
            rows:int -> cols:int -> unit -> textarea_elt
(** Creates a [<textarea>] tag *)

    val raw_textarea :
        ?a:textarea_attrib_t ->
          name:string -> ?value:string ->
            rows:int -> cols:int -> unit -> textarea_elt
(** Creates a [<textarea>] tag for untyped form *)

    type 'a soption =
        option_attrib_t
          * 'a (* Content (or value if the following is present) *)
          * pcdata_elt option (* if content different from value *)
          * bool (* selected *)

    type 'a select_opt =
      | Optgroup of
          optgroup_attrib_t
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

    val raw_select :
        ?a:select_attrib_t ->
          name:string ->
            string select_opt ->
              string select_opt list ->
                select_elt
(** Creates a [<select>] tag for any (untyped) value. *)

    val int_select :
        ?a:select_attrib_t ->
          name:[< `One of int ] param_name ->
            int select_opt ->
              int select_opt list ->
                select_elt
(** Creates a [<select>] tag for int values. *)

    val int32_select :
        ?a:select_attrib_t ->
          name:[< `One of int32 ] param_name ->
            int32 select_opt ->
              int32 select_opt list ->
                select_elt
(** Creates a [<select>] tag for int32 values. *)

    val int64_select :
        ?a:select_attrib_t ->
          name:[< `One of int64 ] param_name ->
            int64 select_opt ->
              int64 select_opt list ->
                select_elt
(** Creates a [<select>] tag for int64 values. *)

    val float_select :
        ?a:select_attrib_t ->
          name:[< `One of float ] param_name ->
            float select_opt ->
              float select_opt list ->
                select_elt
(** Creates a [<select>] tag for float values. *)

    val string_select :
        ?a:select_attrib_t ->
          name:[< `One of string ] param_name ->
            string select_opt ->
              string select_opt list ->
                select_elt
(** Creates a [<select>] tag for string values. *)

    val user_type_select : ('a -> string) ->
        ?a:select_attrib_t ->
          name:[< `One of 'a ] param_name ->
            'a select_opt ->
              'a select_opt list ->
                  select_elt
(** Creates a [<select>] tag for user type values. *)

    val raw_multiple_select :
        ?a:select_attrib_t ->
          name:string ->
            string select_opt ->
              string select_opt list ->
                select_elt
(** Creates a [<select>] tag for any (untyped) value. *)

    val int_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of int ] param_name ->
            int select_opt ->
              int select_opt list ->
                select_elt
(** Creates a [<select>] tag for int values. *)

    val int32_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of int32 ] param_name ->
            int32 select_opt ->
              int32 select_opt list ->
                select_elt
(** Creates a [<select>] tag for int32 values. *)

    val int64_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of int64 ] param_name ->
            int64 select_opt ->
              int64 select_opt list ->
                select_elt
(** Creates a [<select>] tag for int64 values. *)

    val float_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of float ] param_name ->
            float select_opt ->
              float select_opt list ->
                select_elt
(** Creates a [<select>] tag for float values. *)

    val string_multiple_select :
        ?a:select_attrib_t ->
          name:[< `Set of string ] param_name ->
            string select_opt ->
              string select_opt list ->
                select_elt
(** Creates a [<select>] tag for string values. *)

    val user_type_multiple_select :
      ('a -> string) ->
      ?a:select_attrib_t ->
      name:[< `Set of 'a ] param_name ->
      'a select_opt ->
      'a select_opt list ->
      select_elt
(** Creates a [<select>] tag for user type values. *)



end



module MakeForms : functor (Pages: FORMCREATE) -> ELIOMFORMSIG with
type form_content_elt = Pages.form_content_elt
and type form_content_elt_list = Pages.form_content_elt_list
and type form_elt = Pages.form_elt
and type 'a a_content_elt = 'a Pages.a_content_elt
and type 'a a_content_elt_list = 'a Pages.a_content_elt_list
and type 'a a_elt = 'a Pages.a_elt
and type 'a a_elt_list = 'a Pages.a_elt_list
and type div_content_elt = Pages.div_content_elt
and type div_content_elt_list = Pages.div_content_elt_list
and type uri = Pages.uri
and type link_elt = Pages.link_elt
and type script_elt = Pages.script_elt
and type textarea_elt = Pages.textarea_elt
and type input_elt = Pages.input_elt
and type pcdata_elt = Pages.pcdata_elt
and type select_elt = Pages.select_elt
and type select_content_elt = Pages.select_content_elt
and type select_content_elt_list = Pages.select_content_elt_list
and type button_elt = Pages.button_elt
and type button_content_elt = Pages.button_content_elt
and type button_content_elt_list = Pages.button_content_elt_list
and type option_elt = Pages.option_elt
and type option_elt_list = Pages.option_elt_list

and type a_attrib_t = Pages.a_attrib_t
and type form_attrib_t = Pages.form_attrib_t
and type input_attrib_t = Pages.input_attrib_t
and type textarea_attrib_t = Pages.textarea_attrib_t
and type select_attrib_t = Pages.select_attrib_t
and type link_attrib_t = Pages.link_attrib_t
and type script_attrib_t = Pages.script_attrib_t
and type optgroup_attrib_t = Pages.optgroup_attrib_t
and type option_attrib_t = Pages.option_attrib_t
and type button_attrib_t = Pages.button_attrib_t

and type input_type_t = Pages.input_type_t
and type button_type_t = Pages.button_type_t


(**/**)
val nl_internal_appl_form :
  (bool, [ `WithoutSuffix ],
   [ `One of bool ] Eliom_parameters.param_name)
           Eliom_parameters.non_localized_params
