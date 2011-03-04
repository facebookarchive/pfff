
(* All dates are UTC *)

type status = Tentative | Confirmed | Cancelled

type transp = Opaque | Transparent

type event_end = Dtend of CalendarLib.Calendar.t | Duration of int

type event =
  { dtstart : CalendarLib.Calendar.t;
    event_end : event_end option;
    dtstamp : CalendarLib.Calendar.t;
    uid : string;  (* event_1234@hostname *)
    summary : string;
    description : string option;
    comment : string list;
    location : string option;
(* XXX organizer ?*)
    sequence : int option;
    status : status option;
    transp : transp;
    created : CalendarLib.Calendar.t option;
    last_modified : CalendarLib.Calendar.t option;
    url : string option }

type t =
  { prodid : string; (*XXX see ISO 9070 *)
    events : event list;
    calname : string option;
    caldesc : string option }
(*XXX description (X-WR-CALDESC) ...
*)

val calendar : t -> string

(*XXX Default event?*)