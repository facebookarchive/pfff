type parsing_stat = {
    filename: Common.filename;
    mutable have_timeout: bool;
    mutable correct: int;
    mutable bad: int;
    mutable commentized: int;
  } 

val print_parsing_stat_list: ?verbose:bool -> parsing_stat list -> unit
