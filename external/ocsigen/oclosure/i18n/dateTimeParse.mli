(*
   OClosure Project - 2010
   Class goog.i18n.DateTimeParse
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef I18N
open Js
open DateTimeFormat
#endif

class type dateTimeParse = object

(**
   Parse the given string and fill info into date object. This version does
   not validate the input.
   @param text The string being parsed.
   @param date The Date object to hold the parsed date.
   @param opt_start The position from where parse should begin.
   @return How many characters parser advanced.
*)
  method parse : js_string t -> Js.date t -> int opt -> int meth

(**
   Parse the given string and fill info into date object. This version will
   validate the input and make sure it is a validate date/time.
   @param text The string being parsed.
   @param date The Date object to hold the parsed date.
   @param opt_start The position from where parse should begin.
   @return How many characters parser advanced.
*)
  method strictParse : js_string t -> Js.date t -> int opt -> int meth
end

(**
   Construct a DateTimeParse based on current locale.
   @param pattern pattern specification or pattern type.
*)
val dateTimeParse : ((js_string t, DateTimeFormat.format) Tools.Union.t 
		      -> dateTimeParse t) constr
