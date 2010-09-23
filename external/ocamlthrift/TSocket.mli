
class t :
  string (* host *) ->
  int (* port *) ->
  object

    method close : unit
    method flush : unit
    method isOpen : bool
    method opn : unit

    method read : string -> int -> int -> int
    method readAll : string -> int -> int -> int
    method write : string -> int -> int -> unit
  end
