(*
   OClosure Project - 2010
   Class goog.i18n.TimeZone
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef I18N
open Js
#endif

class type timeZone = object

(**
   Return the DST adjustment to the time zone offset for a given time.
   While Daylight Saving Time is in effect, this number is positive.
   Otherwise, it is zero.
   @param date The time to check.
   @return The DST adjustment in minutes EAST of UTC.
 *)
  method getDaylightAdjustment : Js.date t -> int meth

(**
   Return the GMT representation of this time zone object.
   @param date The date for which time to retrieve GMT string.
   @return GMT representation string.
 *)
  method getGMTString : Js.date t -> js_string t meth

(**
   Get the long time zone name for a given date/time.
   @param date The time for which to retrieve the long time zone name.
   @return The long time zone name.
 *)
  method getLongName : Js.date t -> js_string t meth

(**
   Get the time zone offset in minutes WEST of UTC for a given date/time.
   @param date The time for which to retrieve the time zone offset.
   @return The time zone offset in minutes WEST of UTC.
 *)
  method getOffset : Js.date t -> int meth

(**
   Get the RFC representation of the time zone for a given date/time.
   @param date The time for which to retrieve the RFC time zone string.
   @return The RFC time zone string.
 *)
  method getRFCTimeZoneString : Js.date t -> js_string t meth

(**
   Get the short time zone name for given date/time.
   @param date The time for which to retrieve the short time zone name.
   @return The short time zone name.
 *)
  method getShortName : Js.date t -> int meth

(**
   Return the time zone ID for this time zone.
   @return The time zone ID.
 *)
  method getTimeZoneId : js_string t meth

(**
   Check if Daylight Saving Time is in effect at a given time in this time zone.
   @param date The time to check.
   @return True if Daylight Saving Time is in effect.
 *)
  method isDaylightTime : Js.date t -> bool t meth
end

(**
   TimeZone class implemented a time zone resolution and name information
   source for client applications. The time zone object is initiated from
   a time zone information object. Application can initiate a time zone
   statically, or it may choose to initiate from a data obtained from server.
   Each time zone information array is small, but the whole set of data
   is too much for client application to download. If end user is allowed to
   change time zone setting, dynamic retrieval should be the method to use.
   In case only time zone offset is known, there is a decent fallback
   that only use the time zone offset to create a TimeZone object.
   A whole set of time zone information array was available under
   http://go/js_locale_data. It is generated based on CLDR and
   Olson time zone data base (through pytz), and will be updated timely.
 *)
val timeZone : timeZone t constr

module TimeZone : sig
  (**
   This factory method creates a time zone instance.  It takes either an object
   containing complete time zone information, or a single number representing a
   constant time zone offset.  If the latter form is used, DST functionality is
   not available.
   @param timeZoneData If this parameter is a number, it should
       indicate minutes WEST of UTC to be used as a constant time zone offset.
       Otherwise, it should be an object with these four fields:
       - id: A string ID for the time zone.
       - std_offset: The standard time zone offset in minutes EAST of UTC.
       - names: An array of four names (standard short name, standard long
             name, daylight short name, daylight long, name)
       - transitions: An array of numbers which are interpreted in pairs:
             [time1, adjustment1, time2, adjustment2, ...] where each time is
             a DST transition point given as a number of hours since 00:00 UTC,
             January 1, 1970, and each adjustment is the adjustment to apply
             for times after the DST transition, given as minutes EAST of UTC.
   @return A goog.i18n.TimeZone object for the given
       time zone data.
 *)
  val createTimeZone : int -> timeZone t
end
