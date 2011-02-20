(*
   OClosure Project - 2010
   Class goog.ui.CharPicker
   
   @author : Oran Charles
   @version 0.1
*)
#ifndef UI
open Component
open Js
#endif
class type charPicker = object
  inherit component

  (** Gets the last selected character. **)
  method getSelectedChar : js_string t opt meth

 (** Gets the list of characters user selected recently. **) 
  method getRecentChars : js_string t js_array t meth

 (** @inheritDoc **)
  method createDom : unit meth

 (** @inheritDoc **)
  method disposeInternal : unit meth

 (** @inheritDoc **)
  method decorateInternal : #Dom_html.element t -> unit meth

 (** @inheritDoc **)
  method enterDocument : unit meth
end

val charPicker : (I18n.charPickerData t -> js_string t js_array t -> int opt -> int opt -> int opt -> int opt -> Gdom.domHelper t opt -> charPicker t) constr
