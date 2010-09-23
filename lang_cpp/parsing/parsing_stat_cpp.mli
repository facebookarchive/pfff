
type parsing_stat = {
    filename: Common.filename;
    mutable have_timeout: bool;

    mutable correct: int;  
    mutable bad: int;

    mutable commentized: int; (* by our pp commentizer *)
}

val default_stat: Common.filename -> parsing_stat


val print_parsing_stat_list: 
  ?verbose:bool -> parsing_stat list -> unit

val print_stat_numbers: 
  unit -> unit
