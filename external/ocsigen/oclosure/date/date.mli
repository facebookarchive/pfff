(*
   OClosure Project - 2010
   Class goog.date.Date

   Date module.

   Contains classes Date, DateTime, Interval

   @author : Cardoso Gabriel
   @version 0.2
*)

#include "interval.mli"

class type date = object ('self)
(**
   Performs date calculation by adding the supplied interval to the date.

   @param interval Date interval to add
*)
  method add : interval Js.t -> unit Js.meth

(**
   @return A clone of the date object.
*)
  method clone : 'self Js.t Js.meth

(**
   Tests whether given date is equal to this Date.
   Note: This ignores units more precise than days (hours and below)
   and also ignores timezone considerations.
 
   @param other The date to compare
   @return Whether the given date is equal to this one.
*) 
  method equals : 'self Js.t -> bool Js.t Js.meth

(**
   @return The date of month.
*)
  method getDate : int Js.meth

(**
   @return The day of week, US style. 0 = Sun, 6 = Sat.
*)
  method getDay : int Js.meth

(**
   @return The day of year.
*)
  method getDayOfYear : int Js.meth

(**
   @return The first day of the week. 0 = Mon, 6 = Sun.
*)
  method getFirstDayOfWeek : int Js.meth

(**
   @return The cut off weekday used for week number calculations.
   0 = Mon, 6 = Sun.
*)
  method getFirstWeekCutOffDay : int Js.meth

(**
   @return The four digit year of date.
*)
  method getFullYear : int Js.meth

(**
   @return The day of week, ISO style. 0 = Mon, 6 = Sun.
*)
  method getIsoWeekday : int Js.meth

(**
   @return The month of date, 0 = Jan, 11 = Dec.
*)
  method getMonth : int Js.meth

(**
   @return The number of days for the selected month.
*)
  method getNumberOfDaysInMonth : int Js.meth

(**
   Returns the number of milliseconds since 1 January 1970 00:00:00.
   
   @return The number of milliseconds since 1 January 1970 00:00:00.
*)
  method getTime : float Js.t Js.meth

(**
   Returns timezone offset. The timezone offset is the delta in minutes between
   UTC and your local time. E.g., UTC+10 returns -600. Daylight savings time
   prevents this value from being constant.
   
   @return The timezone offset.
*)
  method getTimezoneOffset : int Js.meth

(**
   Returns timezone offset as a string. Returns offset in [+-]HH:mm format or Z
   for UTC.
   
   @return The timezone offset as a string.
*)
  method getTimezoneOffsetString : Js.js_string Js.t Js.meth

(**
   @return The date of month according to universal time.
*)
  method getUTCDate : int Js.meth

(**
   @return The day of week according to universal time, US style.
       0 = Sun, 1 = Mon, 6 = Sat.
*)
  method getUTCDay : int Js.meth

(**
   @return The four digit year of date according to universal time.
*)
  method getUTCFullYear : int Js.meth

(**
   @return The hours value according to universal time.
*)
  method getUTCHours : int Js.meth

(**
   @return The day of week according to universal time, ISO style.
       0 = Mon, 6 = Sun.
*)
  method getUTCIsoWeekday : int Js.meth

(**
   @return The hours value according to universal time.
*)
  method getUTCMinutes : int Js.meth

(**
   @return The month of date according to universal time,
       0 = Jan, 11 = Dec.
*)
  method getUTCMonth : int Js.meth

(**
   @return The day of week according to universal time and
       firstDayOfWeek setting.
*)
  method getUTCWeekday : int Js.meth

(**
   @return The week number.
*)
  method getWeekNumber : int Js.meth

(**
   @return The day of week according to firstDayOfWeek setting.
*)
  method getWeekday : int Js.meth

(**
   Alias for getFullYear.
 
   @return The four digit year of date.
*)
  method getYear : int Js.meth

(**
   Sets the date.
   
   @param date Date object to set date from
*)
  method set : date Js.t -> unit Js.meth

(**
   Sets the day part of the date.

   @param date The day part
*)
  method setDate : int -> float Js.t Js.meth

(**
   Sets the first day of week.

   @param day 0 = Mon, 6 = Sun
*)
  method setFirstDayOfWeek : int -> unit Js.meth

(**
   Sets cut off weekday used for week number calculations. 0 = Mon, 6 = Sun.

   @param day The cut off weekday
*)
  method setFirstWeekCutOffDay : int -> unit Js.meth

(**
   Sets the year part of the date.

   @param year Four digit year
*)
  method setFullYear : int -> float Js.t Js.meth

(**
   Sets the month part of the date.

   @param month The month, where 0 = Jan, 11 = Dec
*)
  method setMonth : int -> float Js.t Js.meth

(**
   Sets the value of the date object as expressed in the number of milliseconds
   since 1 January 1970 00:00:00.

   @param ms Number of milliseconds since 1 Jan 1970
*)
  method setTime : float -> float Js.t Js.meth

(**
   Sets the day part of the date according to universal time.

   @param date The UTC date
*)
  method setUTCDate : int -> float Js.t Js.meth

(**
   Sets the year part of the date according to universal time.

   @param year Four digit year
*)
  method setUTCFullYear : int -> float Js.t Js.meth

(**
   Sets the month part of the date according to universal time.

   @param month The month, where 0 = Jan, 11 = Dec
*)
  method setUTCMonth : int -> float Js.t Js.meth

(**
   Alias for setFullYear.

   @param year Four digit year
*)
  method setYear : int -> unit Js.meth

(**
   Returns ISO 8601 string representation of date.

   @param opt_verbose Whether the verbose format should be used
   instead of the default compact one
   @param opt_tz Whether the timezone offset should be included
   in the string
   @return ISO 8601 string representation of date.
*)
  method toIsoString : bool Js.t Js.opt -> bool Js.t Js.opt -> Js.js_string Js.t Js.meth

(**
   Overloaded toString method for object.
   @return ISO 8601 string representation of date.
*)
  method toString : Js.js_string Js.t Js.meth

(**
   Returns ISO 8601 string representation of date according to universal time.

   @param opt_verbose Whether the verbose format should be used
   instead of the default compact one
   @param opt_tz Whether the timezone offset should be included in
   the string
   @return ISO 8601 string representation of date according to
   universal time.
*)
  method toUTCIsoString : bool Js.t Js.opt -> bool Js.t Js.opt -> Js.js_string Js.t Js.meth

(**
   @return Value of wrapped date.
*)
  method valueOf : float Js.t Js.meth
end

(**   
   date.Date Extends Js.date
   Class representing a date. Defaults to current date if none is specified. 
   Implements most methods of the native js Date object (except the time related
   ones, goog.date.DateTime) and can be used interchangeably with it just as if
   goog.date.Date was a subclass of Date. To allow goog.date.Date objects to be
   passed as arguments to methods expecting Date objects this class is marked as
   extending the built in Date object even though that's not strictly true.

   @param opt_year Four digit year. If not set, the created object will 
   contain the date determined by goog.now().
   @param opt_month Month, 0 = Jan, 11 = Dec.
   @param opt_date Date of month, 1 - 31.
*)
val date : (int Js.opt -> int Js.opt -> int Js.opt -> date Js.t) Js.constr 

class type dateTime = object ('self)
  inherit date
(**
   Returns the hours part of the datetime.

   @return An integer between 0 and 23, representing the hour.
*)
  method getHours : int Js.meth

(**
   Returns the minutes part of the datetime.

   @return An integer between 0 and 59, representing the minutes.
*)
  method getMinutes : int Js.meth

(**
   Returns the seconds part of the datetime.

   @return An integer between 0 and 59, representing the seconds.
*)
  method getSeconds : int Js.meth

(**
   Returns the milliseconds part of the datetime.
 
   @return An integer between 0 and 999, representing the milliseconds.
*)
  method getMilliseconds : int Js.meth

(**
   Returns the day of week according to universal time, US style.

   @return Day of week, 0 = Sun, 1 = Mon, 6 = Sat.
*)
  method getUTCDay : int Js.meth 

(**
   Returns the hours part of the datetime according to universal time.

   @return An integer between 0 and 23, representing the hour.
*)
  method getUTCHours : int Js.meth 

(**
   Returns the minutes part of the datetime according to universal time.

   @return An integer between 0 and 59, representing the minutes.
*)
  method getUTCMinutes : int Js.meth 

(**
   Returns the seconds part of the datetime according to universal time.

   @return An integer between 0 and 59, representing the seconds.
*)
  method getUTCSeconds : int Js.meth 

(**
   Returns the milliseconds part of the datetime according to universal time.

   @return An integer between 0 and 999, representing the milliseconds.
*)
  method getUTCMilliseconds : int Js.meth 

(**
   Sets the hours part of the datetime.

   @param hours An integer between 0 and 23, representing the hour.
*)
  method setHours : int -> float Js.t Js.meth 

(**
   Sets the minutes part of the datetime.

   @param minutes Integer between 0 and 59, representing the minutes.
*)
  method setMinutes : int -> float Js.t Js.meth

(**
   Sets the seconds part of the datetime.

   @param seconds Integer between 0 and 59, representing the seconds.
*)
  method setSeconds : int -> float Js.t Js.meth 

(**
   Sets the seconds part of the datetime.

   @param ms Integer between 0 and 999, representing the milliseconds.
*)
  method setMilliseconds : int -> float Js.t Js.meth 

(**
   Sets the hours part of the datetime according to universal time.

   @param hours An integer between 0 and 23, representing the hour.
*)
  method setUTCHours : int -> float Js.t Js.meth 

(**
   Sets the minutes part of the datetime according to universal time.

   @param minutes Integer between 0 and 59, representing the minutes.
*)
  method setUTCMinutes : int -> float Js.t Js.meth 

(**
   Sets the seconds part of the datetime according to universal time.

   @param seconds Integer between 0 and 59, representing the seconds.
*)
  method setUTCSeconds : int -> float Js.t Js.meth 

(**
   Sets the seconds part of the datetime according to universal time.

   @param ms Integer between 0 and 999, representing the milliseconds.
*)
  method setUTCMilliseconds : int -> float Js.t Js.meth 

(**
   Performs date calculation by adding the supplied interval to the date.

   @param interval Date interval to add.
*)
  method add : interval Js.t -> unit Js.meth 

(**
   Returns ISO 8601 string representation of date/time.

   @param opt_verbose Whether the verbose format should be used
       instead of the default compact one.
   @param opt_tz Whether the timezone offset should be included
       in the string.
   @return ISO 8601 string representation of date/time.
*)
  method toIsoString : bool Js.t Js.opt -> bool Js.t Js.opt -> Js.js_string Js.t Js.meth 

(**
   Returns XML Schema 2 string representation of date/time.
   The return value is also ISO 8601 compliant.

   @param opt_timezone Should the timezone offset be included in the
       string?.
   @return XML Schema 2 string representation of date/time.
*)
  method toXmlDateTime : bool Js.t Js.opt -> Js.js_string Js.t Js.meth 

(**
   Returns ISO 8601 string representation of date/time according to universal
   time.

   @param opt_verbose Whether the opt_verbose format should be
       returned instead of the default compact one.
   @param opt_tz Whether the the timezone offset should be included
       in the string.
   @return ISO 8601 string representation of date/time according to
       universal time.
*)
  method toUTCIsoString : bool Js.t Js.opt -> bool Js.t Js.opt -> Js.js_string Js.t Js.meth 

(**
   Overloaded toString method for object.
   @return ISO 8601 string representation of date/time.
*)
  method toString : Js.js_string Js.t Js.meth 

(**
   Generates time label for the datetime, e.g., '5:30am'.
   By default this does not pad hours (e.g., to '05:30') and it does add
   an am/pm suffix.
   TODO: i18n -- hardcoding time format like this is bad.  E.g., in CJK
                 locales, need Chinese characters for hour and minute units.
   @param opt_padHours Whether to pad hours, e.g., '05:30' vs '5:30'.
   @param opt_showAmPm Whether to show the 'am' and 'pm' suffix.
   @param opt_omitZeroMinutes E.g., '5:00pm' becomes '5pm', but '5:01pm' 
   remains '5:01pm'.
   @return The time label.
*)
  method toUsTimeString : bool Js.t Js.opt -> bool Js.t Js.opt -> bool Js.t Js.opt -> Js.js_string Js.t Js.meth 

(**
   Generates time label for the datetime in standard ISO 24-hour time format.
   E.g., '06:00:00' or '23:30:15'.
   @param opt_showSeconds Whether to shows seconds. Defaults to TRUE.
   @return The time label.
*)
  method toIsoTimeString : bool Js.t Js.opt -> Js.js_string Js.t Js.meth

end

val dateTime : (int Js.opt -> int Js.opt -> int Js.opt -> int Js.opt -> int Js.opt 
  -> int Js.opt -> int Js.opt -> dateTime Js.t) Js.constr 

class type dateRange = object
   method getEndDate : date Js.t Js.meth
   method getStartDate : date Js.t Js.meth
end

(**
   Constructs a date range.
   @param startDate The start date of the range.
   @param endDate The end date of the range.
*)
val dateRange : (Js.date Js.t -> Js.date Js.t -> dateRange Js.t) Js.constr 


class type standardDateRangeKeys = object
end

val standardDateRangeKeys : (standardDateRangeKeys Js.t) Js.constr 
