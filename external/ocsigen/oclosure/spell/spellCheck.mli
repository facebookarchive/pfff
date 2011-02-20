(*
   OClosure Project - 2010
   Class goog.spell.Spellcheck
   
   @author : Cardoso Gabriel
   @version 0.2
*)

open Js

type wordStatus = UNKNOWN | VALID | INVALID | IGNORED | CORRECTED 

type wordEntry

val wordEntry : js_string t -> wordStatus -> js_string t js_array t -> wordEntry

type 'a callback 

val invokeCallback : 'a callback -> 'a -> wordEntry js_array t -> unit

class type ['a] spellCheck = object
  inherit Events.eventTarget

(**
   Checks spelling for a block of text.
       
   @param text Block of text to spell check.
*)
  method checkBlock : js_string t -> unit meth

(**
   Checks spelling for a single word. Returns the status of the supplied word,
   or UNKNOWN if it's not cached. If it's not cached the word is added to a
   queue and checked with the verification implementation with a short delay.
   
   @param word Word to check spelling of.
   @return The status of the supplied word,
   or UNKNOWN if it's not cached.
*)
  method checkWord : js_string t -> int meth

(**
   Returns language.
 
   @return Content language.
*)
  method getLanguage : js_string t meth

(**
   Returns suggestions for the given word.
 
   @param word Word to get suggestions for.
   @return An array of suggestions for the given word.
*)
  method getSuggestions : js_string t -> js_string t js_array t meth

(**
   Processes pending words unless a lookup operation has already been queued or
   is in progress.
 *)
  method processPending : unit meth

(**
   Sets language.
 
   @param opt_language Content language.
*)
  method setLanguage : js_string opt -> unit meth

(**
   Sets the lookup function.
 
   @param f Function to use for word lookup. Must accept an array of
       words, an object reference and a callback function as parameters.
       It must also call the callback function (as a method on the object),
       once ready, with an array containing the original words, their
       spelling status and optionally an array of suggestions.
 *)
  method setLookupFunction : 
      (js_string t js_array t -> 'a -> 'a callback -> unit) Js.callback -> unit meth

(**
   Sets a words spelling status.
 
   @param word Word to set status for.
   @param status Status of word.
   @param opt_suggestions Suggestions.
 
   Example:
   obj.setWordStatus('word', VALID);
   obj.setWordStatus('wrod', INVALID, ['word', 'wood', 'rod']);.
 *)
  method setWordStatus :
      js_string t -> int -> js_string t js_array t opt -> unit meth
end

val spellCheck : ((js_string t js_array t -> 'a -> 'a callback -> unit) Js.callback 
		  -> 'a spellCheck t) constr
