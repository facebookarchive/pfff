(*
   OClosure Project - 2010
   Class goog.ui.AbstractSpellChecker

   Abstract base class for spell checker editor implementations. Provides basic
   functionality such as word lookup and caching.
   
   @author : Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Component
open Js
open Tools
#endif

class type abstractSpellChecker = object ('self)
  inherit component

(**
   Checks spelling for all text.
   Should be overridden by implementation.
 *)
  method check : unit meth

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
   @return The handler used for caching and lookups.
 *)
  method getHandler : 'self t Spell.spellCheck t meth

(**
   Ignores spelling of word.
 
   @param word Word to add.
 *)
  method ignoreWord : js_string t -> unit meth

(**
   @return Whether the correction ui is visible.
 *)
  method isVisible : bool t meth

(**
   Replaces word.
 
   @param el An element wrapping the word that should be replaced.
   @param old Word that was replaced.
   @param word Word to replace with.
 *)
  method replaceWord : #Dom_html.element t -> js_string t -> js_string t -> unit meth

(**
   Hides correction UI.
   Should be overridden by implementation.
 *)
  method resume : unit meth

(**
   Sets the marker for the excluded text.
 
   @param marker RegExp for plain text or class name for
          the rich text spell checker for the elements to exclude from
          checking.
 *)
  method setExcludeMarker : (Js.regExp t, js_string t) Union.t opt -> unit meth

(**
   Sets the handler used for caching and lookups.
 
   @param handler The handler used for caching and
   lookups.
 *)
  method setHandler : 'self t Spell.spellCheck t -> unit meth

(**
   Displays suggestions menu.
 
   @param el Element to display menu for.
   @param opt_pos Position to
   display menu at relative to the viewport (in client coordinates), or a
   mouse event.
 *)
  method showSuggestionsMenu : #Dom_html.element t -> (Events.browserEvent t, Math.coordinate t) Union.t opt -> unit meth

  method markCorrected : bool t prop
end
