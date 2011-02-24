(* $Id: netdate.mli 1393 2010-01-17 21:24:44Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Support for common date/time parsing and formatting.
 * Many routines refer to the epoch, which for Unix is 
 * 00:00:00 UTC, January 1, 1970.
 *)

type t = {
  year : int;		(** complete year *)
  month : int;		(** 1..12 *)
  day : int;		(** 1..31 *)
  hour : int;
  minute : int;
  second : int;
  zone : int;		(** in minutes; 60 = UTC+0100 *)
  week_day : int	(** 0 = sunday; -1 if not given *)
}

val localzone : int
  (** The offset in minutes for the local time zone from the UTC *)

val create : ?zone:int -> float -> t
  (** Convert the time (seconds since the epoch) to a date/time record *)

val parse : ?zone:int -> string -> t
  (** Parse a string and return a date/time record.

      The following particles are recognized (by example):
      - Date: [1971/06/22]
      - Date: [06/22/1971]
      - Date: [1971-06-22]
      - Date: [22-June-1971]
      - Date: [22.06.1971]
      - Date: [June 22, 1971]
      - Date: [22 June 1971]
      - Date (2 digit year): [06/22/71]
      - Date (2 digit year): [22.06.71]
      - Date (2 digit year): [71-06-22]
      - Date (2 digit year): [22-June-71]
      - Month names ([June], [Jun])
      - Weekday names ([Monday], [Mon])
      - Time: [14:55]
      - Time: [14:55:28]
      - Time: [14:55:28.6754]   (the fractional part is not returned)
      - Time may be preceded by [T]
      - Time zone: identifiers like [UTC], [CET], or [Z]
      - Time zone: [+01:00], [-01:00], only following time
      - Time zone: [+0100], [-0100], only following time

      Years must have 2 or 4 digits. 2-digit years >= 70 are interpreted
      as [1900+x]. 2-digit years < 70 are interpreted as [2000+x].
      Support for 2-digit years will be removed in a future version
      of Ocamlnet. (Support for 3-digit years is already removed in
      Ocamlnet 3.0.)

      Only English names of months and weekdays are recognized.

      A date must be given. Time, time zones, and weekdays are optional.
      A missing time is reported as "00:00:00". A missing weekday is
      reported by setting [week_day=(-1)]. A missing time zone is 
      reported by setting [zone] to the passed default.

      It is not checked whether the parsed numbers make sense
      (e.g. whether months are between 1 and 12).

      Date/time strings as defined in RFC 3339 are supported since
      Ocamlnet 3.0.
   *)

val since_epoch : t -> float
  (** Convert a date/time record into the time (seconds since the epoch) *)

val parse_epoch : ?zone:int -> string -> float
  (** Parse a string and return the time (seconds since the epoch *)

val format_to : Netchannels.out_obj_channel -> fmt:string -> t -> unit
  (** Format a date/time record according to the format string and outputs
   * the resulting string to the channel.
   *
   * The format string consists of zero or more conversion specifications
   * and ordinary characters.  All ordinary characters are output directly
   * to the channel.  A conversion specification consists of the '%'
   * character and one other character.
   *
   * The conversion specifications are:
   *
   *  - [%A]: full weekday name.
   *  - [%a]: abbreviated weekday name.
   *  - [%B]: full month name.
   *  - [%b]: abbreviated month name.
   *  - [%C]: (year / 100) as an integer; single digits are preceded by a zero.
   *  - [%c]: equivalent to ["%a %b %e %T %Y"].
   *  - [%D]: equivalent to ["%m/%d/%y"].
   *  - [%d]: day of the month as an integer (01-31); single digits are
   *          preceded by a zero.
   *  - [%e]: day of the month as an integer (1-31).
   *  - [%H]: hour (24-hour clock) as an integer (00-23).
   *  - [%h]: the same as %b.
   *  - [%I]: hour (12-hour clock) as an integer (01-12).
   *  - [%j]: day of the year as an integer (001-366).
   *  - [%k]: hour (24-hour clock) as an integer (0-23);
   *          single digits are preceded by a blank.
   *  - [%l]: hour (12-hour clock) as an integer (1-12);
   *          single digits are preceded by a blank.
   *  - [%M]: minute as an integer (00-59).
   *  - [%m]: month as an integer (01-12).
   *  - [%n]: a newline.
   *  - [%p]: either "AM" or "PM" as appropriate.
   *  - [%P]: either "am" or "pm" as appropriate.
   *  - [%R]: equivalent to ["%H:%M"].
   *  - [%r]: equivalent to ["%I:%M:%S %p"].
   *  - [%S]: second as an integer (00-60).
   *  - [%T]: equivalent to ["%H:%M:%S"].
   *  - [%t]: a tab.
   *  - [%U]: week number of the year (Sunday as the first day
   *          of the week) as an integer (00-53).
   *  - [%u]  weekday (Monday as the first day of the week) as
   *          an integer (1-7).
   *  - [%w]: weekday (Sunday as the first day of the week) as
   *          an integer (0-6).
   *  - [%X]: representation of the time.
   *  - [%x]: representation of the date.
   *  - [%Y]: year with century as an integer.
   *  - [%y]: year without century as an integer (00-99).
   *  - [%z]: time zone offset from UTC; a leading plus sign
   *        stands for east of UTC, a minus sign for west of UTC, hours and
   *        minutes follow with two digits each and no delimiter between them
   *        (common form for RFC 822 date headers).
   *  - [%:z]: time zone with colon, e.g. +05:00 (new since Ocamlnet 3)
   *  - [%%]: a `%' character.
   *
   *)

val format : fmt:string -> t -> string
  (** Format a date/time record as a string *)

val mk_mail_date : ?zone:int -> float -> string
  (** Convert the time (seconds since the epoch) to a date string that
   * conforms to RFC 1123 (which updates RFC 822).
   *
   * Example: ["Sun, 06 Nov 1994 08:49:37 -0500"].
   *)

val mk_usenet_date : ?zone:int -> float -> string
  (** Convert the time (seconds since the epoch) to a date string that
   * conforms to RFC 1036 (which obsoletes RFC 850).
   *
   * Example: ["Sunday, 06-Nov-94 08:49:37 -0500"].
   *
   * Note that this format has only two digits for the year.
   *)

val mk_internet_date : ?zone:int -> float -> string
  (** Convert the time (seconds since the epoch) to a date string that
   * conforms to RFC 3339. This is the most modern format, and should
   * be used if permitted by the network protocol.
   *
   * Example: ["1996-12-19T16:39:57.89-08:00"].
   *)
