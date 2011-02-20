(*
   OClosure Project - 2010
   Class goog.ui.RichTextSpellChecker
   
   @author : Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Js
open AbstractSpellChecker
#endif
open Tools

class type richTextSpellChecker = object
  inherit abstractSpellChecker

(**
   Checks spelling for all text and displays correction UI.
 *)
  method check : unit meth

(**
   Creates the initial DOM representation for the component.
*)
  method createDom : unit meth

(**
   Decorates the element for the UI component.
   
   @param element Element to decorate.
 *)
  method decorateInternal : #Dom_html.element t -> unit meth

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
   Called when the component's element is known to be in the document.
 *)
  method enterDocument : unit meth

(**
   Hides correction UI.
 *)
  method resume : unit meth

(** @inheritDoc *)
  method setExcludeMarker : (regExp t, js_string t) Union.t opt -> unit meth
end

val richTextSpellChecker : (richTextSpellChecker t Spell.spellCheck t -> Gdom.domHelper t opt -> richTextSpellChecker t) constr 
