
val debug_thrift : bool ref
val debug : string -> unit

exception Break
exception Thrift_error
exception Field_empty of string

module Transport :
  sig
    type exn_type =
        UNKNOWN
      | NOT_OPEN
      | ALREADY_OPEN
      | TIMED_OUT
      | END_OF_FILE
    exception E of exn_type * string

    class virtual t :
      object
        method virtual close : unit
        method virtual flush : unit
        method virtual isOpen : bool
        method virtual opn : unit

        method virtual read : string -> int -> int -> int
        method readAll : string -> int -> int -> int
        method virtual write : string -> int -> int -> unit
      end

    class factory : object method getTransport : t -> t end

    class virtual server_t :
      object
        method accept : t
        method virtual acceptImpl : t
        method virtual close : unit
        method virtual listen : unit
      end
  end

module Protocol :
  sig
    type t_type =
        T_STOP
      | T_VOID
      | T_BOOL
      | T_BYTE
      | T_I08
      | T_I16
      | T_I32
      | T_U64
      | T_I64
      | T_DOUBLE
      | T_STRING
      | T_UTF7
      | T_STRUCT
      | T_MAP
      | T_SET
      | T_LIST
      | T_UTF8
      | T_UTF16
    val t_type_to_i : t_type -> int

    val t_type_of_i : int -> t_type
    type message_type = CALL | REPLY | EXCEPTION | ONEWAY
    val message_type_to_i : message_type -> int
    val message_type_of_i : int -> message_type

    class virtual t :
      Transport.t ->
      object
        val mutable trans_ : Transport.t
        method getTransport : Transport.t
        method virtual readBinary : string
        method virtual readBool : bool
        method virtual readByte : int
        method virtual readDouble : float
        method virtual readFieldBegin : string * t_type * int
        method virtual readFieldEnd : unit
        method virtual readI16 : int
        method virtual readI32 : int
        method virtual readI64 : Int64.t
        method virtual readListBegin : t_type * int
        method virtual readListEnd : unit
        method virtual readMapBegin : t_type * t_type * int
        method virtual readMapEnd : unit
        method virtual readMessageBegin : string * message_type * int
        method virtual readMessageEnd : unit
        method virtual readSetBegin : t_type * int
        method virtual readSetEnd : unit
        method virtual readString : string
        method virtual readStructBegin : string
        method virtual readStructEnd : unit
        method skip : t_type -> unit
        method virtual writeBinary : string -> unit
        method virtual writeBool : bool -> unit
        method virtual writeByte : int -> unit
        method virtual writeDouble : float -> unit
        method virtual writeFieldBegin : string * t_type * int -> unit
        method virtual writeFieldEnd : unit
        method virtual writeFieldStop : unit
        method virtual writeI16 : int -> unit
        method virtual writeI32 : int -> unit
        method virtual writeI64 : Int64.t -> unit
        method virtual writeListBegin : t_type * int -> unit
        method virtual writeListEnd : unit
        method virtual writeMapBegin : t_type * t_type * int -> unit
        method virtual writeMapEnd : unit
        method virtual writeMessageBegin :
          string * message_type * int -> unit
        method virtual writeMessageEnd : unit
        method virtual writeSetBegin : t_type * int -> unit
        method virtual writeSetEnd : unit
        method virtual writeString : string -> unit
        method virtual writeStructBegin : string -> unit
        method virtual writeStructEnd : unit
      end
    class virtual factory :
      object method virtual getProtocol : Transport.t -> t end
    type exn_type =
        UNKNOWN
      | INVALID_DATA
      | NEGATIVE_SIZE
      | SIZE_LIMIT
      | BAD_VERSION
    exception E of exn_type * string
  end

module Processor :
  sig
    class virtual t :
      object method virtual process : Protocol.t -> Protocol.t -> bool end
    class factory :
      t ->
      object val processor_ : t method getProcessor : Transport.t -> t end
  end

module Application_Exn :
  sig
    type typ =
        UNKNOWN
      | UNKNOWN_METHOD
      | INVALID_MESSAGE_TYPE
      | WRONG_METHOD_NAME
      | BAD_SEQUENCE_ID
      | MISSING_RESULT
    val typ_of_i : int -> typ
    val typ_to_i : typ -> int
    class t :
      object
        val mutable message : string
        val mutable typ : typ
        method get_message : string
        method get_type : typ
        method set_message : string -> unit
        method set_type : typ -> unit
        method write : Protocol.t -> unit
      end
    val create : typ -> string -> t
    val read : Protocol.t -> t
    exception E of t
  end
