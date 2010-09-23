
class t :
  Thrift.Transport.t ->
  object

    method getTransport : Thrift.Transport.t

    method readBinary : string
    method readBool : bool
    method readByte : int
    method readDouble : float
    method readFieldBegin : string * Thrift.Protocol.t_type * int
    method readFieldEnd : unit
    method readI16 : int
    method readI32 : int
    method readI64 : Int64.t
    method readListBegin : Thrift.Protocol.t_type * int
    method readListEnd : unit
    method readMapBegin : Thrift.Protocol.t_type * Thrift.Protocol.t_type * int
    method readMapEnd : unit
    method readMessageBegin : string * Thrift.Protocol.message_type * int
    method readMessageEnd : unit
    method readSetBegin : Thrift.Protocol.t_type * int
    method readSetEnd : unit
    method readString : string
    method readStructBegin : string
    method readStructEnd : unit

    method skip : Thrift.Protocol.t_type -> unit

    method writeBinary : string -> unit
    method writeBool : bool -> unit
    method writeByte : int -> unit
    method writeDouble : float -> unit
    method writeFieldBegin : string * Thrift.Protocol.t_type * int -> unit
    method writeFieldEnd : unit
    method writeFieldStop : unit
    method writeI16 : int -> unit
    method writeI32 : int -> unit
    method writeI64 : Int64.t -> unit
    method writeListBegin : Thrift.Protocol.t_type * int -> unit
    method writeListEnd : unit
    method writeMapBegin : Thrift.Protocol.t_type * Thrift.Protocol.t_type * int -> unit
    method writeMapEnd : unit
    method writeMessageBegin : string * Thrift.Protocol.message_type * int -> unit
    method writeMessageEnd : unit
    method writeSetBegin : Thrift.Protocol.t_type * int -> unit
    method writeSetEnd : unit
    method writeString : string -> unit
    method writeStructBegin : string -> unit
    method writeStructEnd : unit
  end

class factory : 
 object method getProtocol : Thrift.Transport.t -> Thrift.Protocol.t 
end
