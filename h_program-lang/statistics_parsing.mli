type parsing_stat = {
    filename: Common.filename;
    mutable have_timeout: bool;
    mutable correct: int;
    mutable bad: int;
    mutable commentized: int;
  } 

val default_stat: Common.filename -> parsing_stat

val print_parsing_stat_list: ?verbose:bool -> parsing_stat list -> unit
