(*
   OClosure Project - 2010
   Class goog.i18n.DateTimeFormat
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef I18N
open Js
open TimeZone
#endif

module DateTimeFormat : sig
(**
   Enum to identify predefined Date/Time format pattern.
*)
  type format = 
    | FULL_DATE
    | LONG_DATE
    | MEDIUM_DATE
    | SHORT_DATE
    | FULL_TIME
    | LONG_TIME
    | MEDIUM_TIME
    | SHORT_TIME
    | FULL_DATETIME
    | LONG_DATETIME
    | MEDIUM_DATETIME
    | SHORT_DATETIME
end

class type dateTimeFormat = object

(**
   Format the given date object according to preset pattern and current lcoale.
   @param date The Date object that is being formatted.
   @param opt_timeZone optional, if specified, time
   related fields will be formatted based on its setting. When this field
   is not specified, "undefined" will be pass around and those function
   that really need time zone service will create a default one.
   @return Formatted string for the given date.
*)
  method format : Js.date t -> timeZone t opt -> js_string t meth
end

(**
   Construct a DateTimeFormat object based on current locale.
   @param pattern pattern specification or pattern type.
*)
val dateTimeFormat : ((js_string t, DateTimeFormat.format) Tools.Union.t 
		      -> dateTimeFormat t) constr
