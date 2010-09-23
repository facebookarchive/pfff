
class t :
  Thrift.Processor.t ->
  Thrift.Transport.server_t ->

  Thrift.Transport.factory ->
  Thrift.Protocol.factory ->
  Thrift.Protocol.factory -> 
object 
  method serve : unit 
end
