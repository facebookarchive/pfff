open CalendarLib


type error =
  | NoError
  | ErrorNoMsg
  | ErrorMsg of string

type 'a convert =
  | ConvError of string
  | Converted of 'a


module type Xform = sig

type 'a monad

type (+'html, +'o) t


val string_input :
     ?a:Xhtmltypes.input_attrib XHTML.M.attrib list
  -> string
  -> (Xhtmltypes.inlinemix XHTML.M.elt, string) t

(** Maps the empty list to None, and all the others lists to Some *)
val string_opt_input :
     ?a:Xhtmltypes.input_attrib XHTML.M.attrib list
  -> string option
  -> (Xhtmltypes.inlinemix XHTML.M.elt, string option) t

val int_input :
     ?a:Xhtmltypes.input_attrib XHTML.M.attrib list
  -> ?format:(int -> string)
  -> int
  -> (Xhtmltypes.inlinemix XHTML.M.elt, int) t
val bounded_int_input :
     ?format:(int -> string)
  -> int -> int -> int
  -> (Xhtmltypes.inlinemix XHTML.M.elt, int) t

val bool_checkbox :
     ?a:Xhtmltypes.input_attrib XHTML.M.attrib list
  -> bool
  -> (Xhtmltypes.inlinemix XHTML.M.elt, bool) t

val text_area :
     ?a:Xhtmltypes.textarea_attrib XHTML.M.attrib list
  -> rows:int -> cols:int -> string
  -> (Xhtmltypes.inlinemix XHTML.M.elt, string) t

val submit_button : string -> (Xhtmltypes.inlinemix XHTML.M.elt, bool) t

val select_single :
     (string * string) list -> string
  -> (Xhtmltypes.inlinemix XHTML.M.elt, string) t

val list :
     'i list
  -> ('i -> (Xhtmltypes.form_content XHTML.M.elt, 'o) t)
  -> (Xhtmltypes.form_content XHTML.M.elt, 'o list) t

val list' :
     int
  -> (Xhtmltypes.form_content XHTML.M.elt, 'o) t
  -> (Xhtmltypes.form_content XHTML.M.elt, 'o list) t

val extensible_list :
  string -> 'i -> 'i list ->
  ('i -> (Xhtmltypes.form_content XHTML.M.elt, 'o) t) ->
  (Xhtmltypes.form_content XHTML.M.elt, 'o list) t

(* Displays the input control for 'a, and a checkbox to encode Some/None *)
val opt_input:
  input:('a -> (Xhtmltypes.inlinemix XHTML.M.elt, 'b) t) ->
  default:'a ->
  'a option ->
  (Xhtmltypes.inlinemix XHTML.M.elt, 'b option) t


module Ops : sig

val (@@) : ('elt, 'o1) t -> ('elt, 'o2) t -> ('elt, 'o1 * 'o2) t
val (+@) : ('a, 'b) t -> 'a list -> ('a, 'b) t
val (@+) : 'a list -> ('a, 'b) t -> ('a, 'b) t
val ( |> ) : ('html, 'o1) t -> ('o1 -> 'o2) -> ('html, 'o2) t
val ( ||> ) : ('html, 'o1) t -> ('o1 -> 'o2 monad) -> ('html, 'o2) t

end

val wrap : ('html1 list -> 'html2 list) -> ('html1, 'o) t -> ('html2, 'o) t

val check :
     (Xhtmltypes.inlinemix XHTML.M.elt, 'a) t
  -> ('a -> string option)
  -> (Xhtmltypes.inlinemix XHTML.M.elt, 'a) t

val convert :
     (Xhtmltypes.inlinemix XHTML.M.elt, 'a) t
  -> ('a -> 'b convert monad)
  -> (Xhtmltypes.inlinemix XHTML.M.elt, 'b) t

val hour_input : int -> int -> (Xhtmltypes.inlinemix XHTML.M.elt, int * int) t
val day_input :
  int -> int -> int -> (Xhtmltypes.inlinemix XHTML.M.elt, int * int * int) t
val date_input : Calendar.t -> (Xhtmltypes.inlinemix XHTML.M.elt, Calendar.t) t

val text : string -> Xhtmltypes.inlinemix XHTML.M.elt list
val strong :
  Xhtmltypes.inlinemix XHTML.M.elt list -> Xhtmltypes.inlinemix XHTML.M.elt
val p :
     (Xhtmltypes.inlinemix XHTML.M.elt, 'b) t
  -> (Xhtmltypes.form_content XHTML.M.elt, 'b) t

val form:
     fallback:(
       'a,
       unit,
       [ `Attached of
          ([`Internal of [< `Coservice | `Service ]], [ `Get ])
              Eliom_services.a_s ],
       [< Eliom_services.suff ],
       'b,
       unit,
       [< `Registrable ],
       Eliom_services.http)
          Eliom_services.service
  -> get_args:'a
  -> page:(   Eliom_sessions.server_params
           -> 'a
           -> error
           -> [>Xhtmltypes.form] XHTML.M.elt
           -> XHTML.M.html Lwt.t)
  -> sp:Eliom_sessions.server_params
  -> ?err_handler:(exn -> string option)
  -> (Xhtmltypes.form_content XHTML.M.elt,
      Eliom_sessions.server_params -> Eliom_predefmod.Xhtml.page Lwt.t) t
  -> [>Xhtmltypes.form] XHTML.M.elt monad

end

module Xform: Xform with type 'a monad = 'a
module XformLwt : Xform with type 'a monad = 'a Lwt.t
