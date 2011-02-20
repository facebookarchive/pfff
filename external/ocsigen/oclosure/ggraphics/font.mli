(*
   OClosure Project - 2010

   Class goog.graphics.Font
   
   @author : Oran Charles
   @version 0.2
 *)
#ifndef GRAPHICS
open Js
#endif

class type font = object 

  (** Indication if text should be bolded *)
  method bold : js_string t meth

  (** Indication if text should be in italics *)
  method italic : js_string t meth
end

(** This class represents a font to be used with a renderer.
    @param size  The font size.
    @param family  The font family.
*)
val font : (int -> js_string t -> font t) constr
