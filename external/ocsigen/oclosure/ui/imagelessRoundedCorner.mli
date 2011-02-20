(*
   OClosure Project - 2010
   Class goog.ui.AbstractImagelessRoundedCorner
   Class goog.ui.CanvasRoundedCorner
   Class goog.ui.VmlRoundedCorner
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
#endif

class type abstractImagelessRoundedCorner = object
  method draw : unit meth

  method getBackgroundColor : js_string t meth

  method getBorderColor : js_string t meth

  method getHeight : int meth
	
  method getLineWidth : int meth
      
  method getRadius : int meth

  method getWidth : int meth

  method setBakcgroundColor : js_string t -> unit meth

  method setBorderColor : js_string t -> unit meth

  method setHeight : int -> unit meth

  method setLineWidth : int -> unit meth

  method setRadius : int -> unit meth

  method setWidth : int -> unit meth
end

module ImagelessRoundedCorner : sig
 val create : #Dom_html.element t -> int -> int -> int -> int -> int 
   -> js_string t -> js_string t opt -> Gdom.domHelper t opt 
   -> abstractImagelessRoundedCorner t optdef
end
