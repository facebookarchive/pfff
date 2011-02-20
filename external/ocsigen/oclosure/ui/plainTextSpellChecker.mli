(*
   OClosure Project - 2010
   Class goog.ui.PlainTextSpellChecker
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open AbstractSpellChecker
#endif

class type plainTextSpellChecker = object
  inherit abstractSpellChecker

(**
   Checks spelling for all text and displays correction UI.
 *)
  method check : unit meth

(**
   Creates the initial DOM representation for the component.
*)
  method createDom : unit meth

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
   Called when the component's element is known to be in the document.
 *)
  method enterDocument : unit meth

(** @inheritDoc *)
  method exitDocument : unit meth

(**
   Handles key down for overlay.
   @param  e The browser event.
   @return The handled value.
*)
  method handleOverlayKeyEvent : 
      Events.browserEvent t -> bool t meth

(**
   Hides correction UI.
 *)
  method resume : unit meth
end

val plainTextSpellChecker : (plainTextSpellChecker t Spell.spellCheck t -> Gdom.domHelper t opt -> plainTextSpellChecker t) constr
