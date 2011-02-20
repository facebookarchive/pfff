(*
   OClosure Project - 2010
   Class goog.date.Interval

   @author : Cardoso Gabriel
   @version 0.2
*)

class type interval = object
(**
   Adds the Interval in the argument to this Interval field by field.
 
   @param interval The Interval to add 
*)
  method add : interval Js.t -> unit Js.meth

(**
   @return A clone of the interval object. 
*)
  method clone : interval Js.t Js.meth

(**
   Tests whether the given interval is equal to this interval.
   Note, this is a simple field-by-field comparison, it doesn't
   account for comparisons like "12 months == 1 year".

   @param other The interval to test
   @return Whether the intervals are equal. 
*)
  method equals : interval Js.t -> bool Js.t Js.meth

(**
   @return Negative of this interval.
*)
  method getInverse : interval Js.t Js.meth

(**
   Serializes goog.date.Interval into XML Schema duration (ISO 8601 extended).

   @param opt_verbose Include zero fields in the duration string
   @return An XML schema duration in ISO 8601 extended format,
       or null if the interval contains both positive and negative fields. 
*)
  method toIsoString : bool Js.t Js.opt -> Js.js_string Js.t Js.opt Js.meth
end

(**
   Class representing a date/time interval. Used for date calculations.
   <pre>
   new goog.date.Interval(0, 1) // One month
   new goog.date.Interval(0, 0, 3, 1) // Three days and one hour
   new goog.date.Interval(goog.date.Interval.DAYS, 1) // One day
   </pre>
   
   @param opt_years Years or string representing date part
   @param opt_months Months or number of whatever date part specified by first
   parameter
   @param opt_days Days
   @param opt_hours Hours
   @param opt_minutes Minutes
   @param opt_seconds Seconds
*)
val interval : ((int, Js.js_string Js.t) Tools.Union.t Js.opt -> int Js.opt -> int Js.opt -> int Js.opt -> int Js.opt -> int Js.opt -> interval Js.t) Js.constr
