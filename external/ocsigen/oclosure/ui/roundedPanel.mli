(*
   OClosure Project - 2010
   Class goog.ui.BaseRoundedPanel

   @author Gabriel Cardoso 
   @version 0.2
*)

#ifndef UI
open Js
open Component
#endif

class type baseRoundedPanel = object
  inherit component 

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
   Returns the DOM element containing the actual content.
   @return The element containing the actual content (null if none).
*)
  method getContentElement : Dom_html.element t meth 
end

(**
   Factory method that returns an instance of a BaseRoundedPanel.
   @param radius The radius of the rounded corner(s), in pixels.
   @param borderWidth The thickness of the border, in pixels.
   @param borderColor The border color of the panel.
   @param opt_backgroundColor The background color of the panel.
   @param opt_corners The corners of the panel to be rounded. Any
   corners not specified will be rendered as square corners. Will default
   to all square corners if not specified.
   @param opt_domHelper The DOM helper object for the
   document we want to render in.
   @return An instance of a goog.ui.BaseRoundedPanel subclass.
*)

module RoundedPanel : sig
  val create : int -> int -> js_string t -> js_string t opt -> int opt 
    -> Gdom.domHelper t opt -> baseRoundedPanel t
end
