type identity =
  { id : string;
    link : string;
    updated : CalendarLib.Calendar.t; (* UTC date *)
    title : string }

type entry =
  { e_id : identity;
    author : string;
    content : Xhtmltypes_duce.flows }

val simple_feed : string -> identity -> entry list -> string
