
class virtual t :
  Thrift.Processor.t ->
  Thrift.Transport.server_t ->
  Thrift.Transport.factory ->
  Thrift.Protocol.factory ->
  Thrift.Protocol.factory -> 
object 
  method virtual serve : unit 
end

val run_basic_server :
  < process : TBinaryProtocol.t -> TBinaryProtocol.t -> bool; .. > ->
  int -> unit
