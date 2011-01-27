
class t :
  int (* port *) -> (Unix.socket_bool_option * bool) list ->
  object
    val mutable sock : Unix.file_descr option

    method accept : Thrift.Transport.t
    method acceptImpl : Thrift.Transport.t
    method close : unit
    method listen : unit
  end
