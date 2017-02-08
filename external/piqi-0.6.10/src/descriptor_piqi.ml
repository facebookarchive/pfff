module Piqirun = Piqi_piqirun
  
module rec Descriptor_piqi :
             sig
               type uint64 = int64
               
               type float64 = float
               
               type protobuf_int32 = int32
               
               type protobuf_int64 = int64
               
               type binary = string
               
               type field_descriptor_proto_type =
                 [
                   | `type_double
                   | `type_float
                   | `type_int64
                   | `type_uint64
                   | `type_int32
                   | `type_fixed64
                   | `type_fixed32
                   | `type_bool
                   | `type_string
                   | `type_group
                   | `type_message
                   | `type_bytes
                   | `type_uint32
                   | `type_enum
                   | `type_sfixed32
                   | `type_sfixed64
                   | `type_sint32
                   | `type_sint64
                 ]
               
               type field_descriptor_proto_label =
                 [ | `label_optional | `label_required | `label_repeated
                 ]
               
               type file_options_optimize_mode =
                 [ | `speed | `code_size | `lite_runtime
                 ]
               
               type field_options_ctype =
                 [ | `string | `cord | `string_piece
                 ]
               
               type file_descriptor_set = File_descriptor_set.t
               
               type file_descriptor_proto = File_descriptor_proto.t
               
               type descriptor_proto = Descriptor_proto.t
               
               type descriptor_proto_extension_range =
                 Descriptor_proto_extension_range.
                 t
               
               type field_descriptor_proto = Field_descriptor_proto.t
               
               type enum_descriptor_proto = Enum_descriptor_proto.t
               
               type enum_value_descriptor_proto =
                 Enum_value_descriptor_proto.
                 t
               
               type service_descriptor_proto = Service_descriptor_proto.t
               
               type method_descriptor_proto = Method_descriptor_proto.t
               
               type file_options = File_options.t
               
               type message_options = Message_options.t
               
               type field_options = Field_options.t
               
               type enum_options = Enum_options.t
               
               type enum_value_options = Enum_value_options.t
               
               type service_options = Service_options.t
               
               type method_options = Method_options.t
               
               type uninterpreted_option = Uninterpreted_option.t
               
               type uninterpreted_option_name_part =
                 Uninterpreted_option_name_part.
                 t
               
             end = Descriptor_piqi
and
  File_descriptor_set :
    sig
      type t = { mutable file : Descriptor_piqi.file_descriptor_proto list }
      
    end = File_descriptor_set
and
  File_descriptor_proto :
    sig
      type t =
        { mutable name : string option; mutable package : string option;
          mutable dependency : string list;
          mutable message_type : Descriptor_piqi.descriptor_proto list;
          mutable enum_type : Descriptor_piqi.enum_descriptor_proto list;
          mutable service : Descriptor_piqi.service_descriptor_proto list;
          mutable extension : Descriptor_piqi.field_descriptor_proto list;
          mutable options : Descriptor_piqi.file_options option
        }
      
    end = File_descriptor_proto
and
  Descriptor_proto :
    sig
      type t =
        { mutable name : string option;
          mutable field : Descriptor_piqi.field_descriptor_proto list;
          mutable extension : Descriptor_piqi.field_descriptor_proto list;
          mutable nested_type : Descriptor_piqi.descriptor_proto list;
          mutable enum_type : Descriptor_piqi.enum_descriptor_proto list;
          mutable extension_range :
          Descriptor_piqi.descriptor_proto_extension_range list;
          mutable options : Descriptor_piqi.message_options option
        }
      
    end = Descriptor_proto
and
  Descriptor_proto_extension_range :
    sig
      type t =
        { mutable start : Descriptor_piqi.protobuf_int32 option;
          mutable p_end : Descriptor_piqi.protobuf_int32 option
        }
      
    end = Descriptor_proto_extension_range
and
  Field_descriptor_proto :
    sig
      type t =
        { mutable name : string option;
          mutable number : Descriptor_piqi.protobuf_int32 option;
          mutable label : Descriptor_piqi.field_descriptor_proto_label option;
          mutable p_type : Descriptor_piqi.field_descriptor_proto_type option;
          mutable type_name : string option;
          mutable extendee : string option;
          mutable default_value : string option;
          mutable options : Descriptor_piqi.field_options option
        }
      
    end = Field_descriptor_proto
and
  Enum_descriptor_proto :
    sig
      type t =
        { mutable name : string option;
          mutable value : Descriptor_piqi.enum_value_descriptor_proto list;
          mutable options : Descriptor_piqi.enum_options option
        }
      
    end = Enum_descriptor_proto
and
  Enum_value_descriptor_proto :
    sig
      type t =
        { mutable name : string option;
          mutable number : Descriptor_piqi.protobuf_int32 option;
          mutable options : Descriptor_piqi.enum_value_options option
        }
      
    end = Enum_value_descriptor_proto
and
  Service_descriptor_proto :
    sig
      type t =
        { mutable name : string option;
          mutable p_method : Descriptor_piqi.method_descriptor_proto list;
          mutable options : Descriptor_piqi.service_options option
        }
      
    end = Service_descriptor_proto
and
  Method_descriptor_proto :
    sig
      type t =
        { mutable name : string option; mutable input_type : string option;
          mutable output_type : string option;
          mutable options : Descriptor_piqi.method_options option
        }
      
    end = Method_descriptor_proto
and
  File_options :
    sig
      type t =
        { mutable java_package : string option;
          mutable java_outer_classname : string option;
          mutable java_multiple_files : bool;
          mutable optimize_for : Descriptor_piqi.file_options_optimize_mode;
          mutable cc_generic_services : bool;
          mutable java_generic_services : bool;
          mutable py_generic_services : bool;
          mutable uninterpreted_option :
          Descriptor_piqi.uninterpreted_option list
        }
      
    end = File_options
and
  Message_options :
    sig
      type t =
        { mutable message_set_wire_format : bool;
          mutable no_standard_descriptor_accessor : bool;
          mutable uninterpreted_option :
          Descriptor_piqi.uninterpreted_option list
        }
      
    end = Message_options
and
  Field_options :
    sig
      type t =
        { mutable ctype : Descriptor_piqi.field_options_ctype;
          mutable packed : bool option; mutable deprecated : bool;
          mutable experimental_map_key : string option;
          mutable uninterpreted_option :
          Descriptor_piqi.uninterpreted_option list
        }
      
    end = Field_options
and
  Enum_options :
    sig
      type t =
        {
          mutable uninterpreted_option :
          Descriptor_piqi.uninterpreted_option list
        }
      
    end = Enum_options
and
  Enum_value_options :
    sig
      type t =
        {
          mutable uninterpreted_option :
          Descriptor_piqi.uninterpreted_option list
        }
      
    end = Enum_value_options
and
  Service_options :
    sig
      type t =
        {
          mutable uninterpreted_option :
          Descriptor_piqi.uninterpreted_option list
        }
      
    end = Service_options
and
  Method_options :
    sig
      type t =
        {
          mutable uninterpreted_option :
          Descriptor_piqi.uninterpreted_option list
        }
      
    end = Method_options
and
  Uninterpreted_option :
    sig
      type t =
        { mutable name : Descriptor_piqi.uninterpreted_option_name_part list;
          mutable identifier_value : string option;
          mutable positive_int_value : Descriptor_piqi.uint64 option;
          mutable negative_int_value : Descriptor_piqi.protobuf_int64 option;
          mutable double_value : Descriptor_piqi.float64 option;
          mutable string_value : Descriptor_piqi.binary option
        }
      
    end = Uninterpreted_option
and
  Uninterpreted_option_name_part :
    sig type t = { mutable name_part : string; mutable is_extension : bool }
        
    end = Uninterpreted_option_name_part
  
let rec parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x
and parse_int64 x = Piqirun.int64_of_zigzag_varint x
and packed_parse_int64 x = Piqirun.int64_of_packed_zigzag_varint x
and parse_string x = Piqirun.string_of_block x
and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x
and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x
and parse_uint64 x = Piqirun.int64_of_varint x
and packed_parse_uint64 x = Piqirun.int64_of_packed_varint x
and parse_protobuf_int64 x = Piqirun.int64_of_signed_varint x
and packed_parse_protobuf_int64 x = Piqirun.int64_of_packed_signed_varint x
and parse_float64 x = Piqirun.float_of_fixed64 x
and packed_parse_float64 x = Piqirun.float_of_packed_fixed64 x
and parse_binary x = Piqirun.string_of_block x
and parse_file_descriptor_set x =
  let x = Piqirun.parse_record x in
  let (_file, x) =
    Piqirun.parse_repeated_field 1 parse_file_descriptor_proto x
  in (Piqirun.check_unparsed_fields x; { File_descriptor_set.file = _file; })
and parse_file_descriptor_proto x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_optional_field 1 parse_string x in
  let (_package, x) = Piqirun.parse_optional_field 2 parse_string x in
  let (_dependency, x) = Piqirun.parse_repeated_field 3 parse_string x in
  let (_message_type, x) =
    Piqirun.parse_repeated_field 4 parse_descriptor_proto x in
  let (_enum_type, x) =
    Piqirun.parse_repeated_field 5 parse_enum_descriptor_proto x in
  let (_service, x) =
    Piqirun.parse_repeated_field 6 parse_service_descriptor_proto x in
  let (_extension, x) =
    Piqirun.parse_repeated_field 7 parse_field_descriptor_proto x in
  let (_options, x) = Piqirun.parse_optional_field 8 parse_file_options x
  in
    (Piqirun.check_unparsed_fields x;
     {
       File_descriptor_proto.name = _name;
       File_descriptor_proto.package = _package;
       File_descriptor_proto.dependency = _dependency;
       File_descriptor_proto.message_type = _message_type;
       File_descriptor_proto.enum_type = _enum_type;
       File_descriptor_proto.service = _service;
       File_descriptor_proto.extension = _extension;
       File_descriptor_proto.options = _options;
     })
and parse_descriptor_proto x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_optional_field 1 parse_string x in
  let (_field, x) =
    Piqirun.parse_repeated_field 2 parse_field_descriptor_proto x in
  let (_nested_type, x) =
    Piqirun.parse_repeated_field 3 parse_descriptor_proto x in
  let (_enum_type, x) =
    Piqirun.parse_repeated_field 4 parse_enum_descriptor_proto x in
  let (_extension_range, x) =
    Piqirun.parse_repeated_field 5 parse_descriptor_proto_extension_range x in
  let (_extension, x) =
    Piqirun.parse_repeated_field 6 parse_field_descriptor_proto x in
  let (_options, x) = Piqirun.parse_optional_field 7 parse_message_options x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Descriptor_proto.name = _name;
       Descriptor_proto.field = _field;
       Descriptor_proto.nested_type = _nested_type;
       Descriptor_proto.enum_type = _enum_type;
       Descriptor_proto.extension_range = _extension_range;
       Descriptor_proto.extension = _extension;
       Descriptor_proto.options = _options;
     })
and parse_descriptor_proto_extension_range x =
  let x = Piqirun.parse_record x in
  let (_start, x) = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let (_p_end, x) = Piqirun.parse_optional_field 2 parse_protobuf_int32 x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Descriptor_proto_extension_range.start = _start;
       Descriptor_proto_extension_range.p_end = _p_end;
     })
and parse_field_descriptor_proto x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_optional_field 1 parse_string x in
  let (_extendee, x) = Piqirun.parse_optional_field 2 parse_string x in
  let (_number, x) = Piqirun.parse_optional_field 3 parse_protobuf_int32 x in
  let (_label, x) =
    Piqirun.parse_optional_field 4 parse_field_descriptor_proto_label x in
  let (_p_type, x) =
    Piqirun.parse_optional_field 5 parse_field_descriptor_proto_type x in
  let (_type_name, x) = Piqirun.parse_optional_field 6 parse_string x in
  let (_default_value, x) = Piqirun.parse_optional_field 7 parse_string x in
  let (_options, x) = Piqirun.parse_optional_field 8 parse_field_options x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Field_descriptor_proto.name = _name;
       Field_descriptor_proto.extendee = _extendee;
       Field_descriptor_proto.number = _number;
       Field_descriptor_proto.label = _label;
       Field_descriptor_proto.p_type = _p_type;
       Field_descriptor_proto.type_name = _type_name;
       Field_descriptor_proto.default_value = _default_value;
       Field_descriptor_proto.options = _options;
     })
and parse_field_descriptor_proto_type x =
  match Piqirun.int32_of_signed_varint x with
  | 1l -> `type_double
  | 2l -> `type_float
  | 3l -> `type_int64
  | 4l -> `type_uint64
  | 5l -> `type_int32
  | 6l -> `type_fixed64
  | 7l -> `type_fixed32
  | 8l -> `type_bool
  | 9l -> `type_string
  | 10l -> `type_group
  | 11l -> `type_message
  | 12l -> `type_bytes
  | 13l -> `type_uint32
  | 14l -> `type_enum
  | 15l -> `type_sfixed32
  | 16l -> `type_sfixed64
  | 17l -> `type_sint32
  | 18l -> `type_sint64
  | x -> Piqirun.error_enum_const x
and packed_parse_field_descriptor_proto_type x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 1l -> `type_double
  | 2l -> `type_float
  | 3l -> `type_int64
  | 4l -> `type_uint64
  | 5l -> `type_int32
  | 6l -> `type_fixed64
  | 7l -> `type_fixed32
  | 8l -> `type_bool
  | 9l -> `type_string
  | 10l -> `type_group
  | 11l -> `type_message
  | 12l -> `type_bytes
  | 13l -> `type_uint32
  | 14l -> `type_enum
  | 15l -> `type_sfixed32
  | 16l -> `type_sfixed64
  | 17l -> `type_sint32
  | 18l -> `type_sint64
  | x -> Piqirun.error_enum_const x
and parse_field_descriptor_proto_label x =
  match Piqirun.int32_of_signed_varint x with
  | 1l -> `label_optional
  | 2l -> `label_required
  | 3l -> `label_repeated
  | x -> Piqirun.error_enum_const x
and packed_parse_field_descriptor_proto_label x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 1l -> `label_optional
  | 2l -> `label_required
  | 3l -> `label_repeated
  | x -> Piqirun.error_enum_const x
and parse_enum_descriptor_proto x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_optional_field 1 parse_string x in
  let (_value, x) =
    Piqirun.parse_repeated_field 2 parse_enum_value_descriptor_proto x in
  let (_options, x) = Piqirun.parse_optional_field 3 parse_enum_options x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Enum_descriptor_proto.name = _name;
       Enum_descriptor_proto.value = _value;
       Enum_descriptor_proto.options = _options;
     })
and parse_enum_value_descriptor_proto x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_optional_field 1 parse_string x in
  let (_number, x) = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let (_options, x) =
    Piqirun.parse_optional_field 3 parse_enum_value_options x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Enum_value_descriptor_proto.name = _name;
       Enum_value_descriptor_proto.number = _number;
       Enum_value_descriptor_proto.options = _options;
     })
and parse_service_descriptor_proto x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_optional_field 1 parse_string x in
  let (_p_method, x) =
    Piqirun.parse_repeated_field 2 parse_method_descriptor_proto x in
  let (_options, x) = Piqirun.parse_optional_field 3 parse_service_options x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Service_descriptor_proto.name = _name;
       Service_descriptor_proto.p_method = _p_method;
       Service_descriptor_proto.options = _options;
     })
and parse_method_descriptor_proto x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_optional_field 1 parse_string x in
  let (_input_type, x) = Piqirun.parse_optional_field 2 parse_string x in
  let (_output_type, x) = Piqirun.parse_optional_field 3 parse_string x in
  let (_options, x) = Piqirun.parse_optional_field 4 parse_method_options x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Method_descriptor_proto.name = _name;
       Method_descriptor_proto.input_type = _input_type;
       Method_descriptor_proto.output_type = _output_type;
       Method_descriptor_proto.options = _options;
     })
and parse_file_options x =
  let x = Piqirun.parse_record x in
  let (_java_package, x) = Piqirun.parse_optional_field 1 parse_string x in
  let (_java_outer_classname, x) =
    Piqirun.parse_optional_field 8 parse_string x in
  let (_optimize_for, x) =
    Piqirun.parse_required_field 9 parse_file_options_optimize_mode x
      ~default: "\b\001" in
  let (_java_multiple_files, x) =
    Piqirun.parse_required_field 10 parse_bool x ~default: "\b\000" in
  let (_cc_generic_services, x) =
    Piqirun.parse_required_field 16 parse_bool x ~default: "\b\001" in
  let (_java_generic_services, x) =
    Piqirun.parse_required_field 17 parse_bool x ~default: "\b\001" in
  let (_py_generic_services, x) =
    Piqirun.parse_required_field 18 parse_bool x ~default: "\b\001" in
  let (_uninterpreted_option, x) =
    Piqirun.parse_repeated_field 999 parse_uninterpreted_option x
  in
    (Piqirun.check_unparsed_fields x;
     {
       File_options.java_package = _java_package;
       File_options.java_outer_classname = _java_outer_classname;
       File_options.optimize_for = _optimize_for;
       File_options.java_multiple_files = _java_multiple_files;
       File_options.cc_generic_services = _cc_generic_services;
       File_options.java_generic_services = _java_generic_services;
       File_options.py_generic_services = _py_generic_services;
       File_options.uninterpreted_option = _uninterpreted_option;
     })
and parse_file_options_optimize_mode x =
  match Piqirun.int32_of_signed_varint x with
  | 1l -> `speed
  | 2l -> `code_size
  | 3l -> `lite_runtime
  | x -> Piqirun.error_enum_const x
and packed_parse_file_options_optimize_mode x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 1l -> `speed
  | 2l -> `code_size
  | 3l -> `lite_runtime
  | x -> Piqirun.error_enum_const x
and parse_message_options x =
  let x = Piqirun.parse_record x in
  let (_message_set_wire_format, x) =
    Piqirun.parse_required_field 1 parse_bool x ~default: "\b\000" in
  let (_no_standard_descriptor_accessor, x) =
    Piqirun.parse_required_field 2 parse_bool x ~default: "\b\000" in
  let (_uninterpreted_option, x) =
    Piqirun.parse_repeated_field 999 parse_uninterpreted_option x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Message_options.message_set_wire_format = _message_set_wire_format;
       Message_options.no_standard_descriptor_accessor =
         _no_standard_descriptor_accessor;
       Message_options.uninterpreted_option = _uninterpreted_option;
     })
and parse_field_options x =
  let x = Piqirun.parse_record x in
  let (_ctype, x) =
    Piqirun.parse_required_field 1 parse_field_options_ctype x
      ~default: "\b\000" in
  let (_packed, x) = Piqirun.parse_optional_field 2 parse_bool x in
  let (_deprecated, x) =
    Piqirun.parse_required_field 3 parse_bool x ~default: "\b\000" in
  let (_experimental_map_key, x) =
    Piqirun.parse_optional_field 9 parse_string x in
  let (_uninterpreted_option, x) =
    Piqirun.parse_repeated_field 999 parse_uninterpreted_option x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Field_options.ctype = _ctype;
       Field_options.packed = _packed;
       Field_options.deprecated = _deprecated;
       Field_options.experimental_map_key = _experimental_map_key;
       Field_options.uninterpreted_option = _uninterpreted_option;
     })
and parse_field_options_ctype x =
  match Piqirun.int32_of_signed_varint x with
  | 0l -> `string
  | 1l -> `cord
  | 2l -> `string_piece
  | x -> Piqirun.error_enum_const x
and packed_parse_field_options_ctype x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 0l -> `string
  | 1l -> `cord
  | 2l -> `string_piece
  | x -> Piqirun.error_enum_const x
and parse_enum_options x =
  let x = Piqirun.parse_record x in
  let (_uninterpreted_option, x) =
    Piqirun.parse_repeated_field 999 parse_uninterpreted_option x
  in
    (Piqirun.check_unparsed_fields x;
     { Enum_options.uninterpreted_option = _uninterpreted_option; })
and parse_enum_value_options x =
  let x = Piqirun.parse_record x in
  let (_uninterpreted_option, x) =
    Piqirun.parse_repeated_field 999 parse_uninterpreted_option x
  in
    (Piqirun.check_unparsed_fields x;
     { Enum_value_options.uninterpreted_option = _uninterpreted_option; })
and parse_service_options x =
  let x = Piqirun.parse_record x in
  let (_uninterpreted_option, x) =
    Piqirun.parse_repeated_field 999 parse_uninterpreted_option x
  in
    (Piqirun.check_unparsed_fields x;
     { Service_options.uninterpreted_option = _uninterpreted_option; })
and parse_method_options x =
  let x = Piqirun.parse_record x in
  let (_uninterpreted_option, x) =
    Piqirun.parse_repeated_field 999 parse_uninterpreted_option x
  in
    (Piqirun.check_unparsed_fields x;
     { Method_options.uninterpreted_option = _uninterpreted_option; })
and parse_uninterpreted_option x =
  let x = Piqirun.parse_record x in
  let (_name, x) =
    Piqirun.parse_repeated_field 2 parse_uninterpreted_option_name_part x in
  let (_identifier_value, x) =
    Piqirun.parse_optional_field 3 parse_string x in
  let (_positive_int_value, x) =
    Piqirun.parse_optional_field 4 parse_uint64 x in
  let (_negative_int_value, x) =
    Piqirun.parse_optional_field 5 parse_protobuf_int64 x in
  let (_double_value, x) = Piqirun.parse_optional_field 6 parse_float64 x in
  let (_string_value, x) = Piqirun.parse_optional_field 7 parse_binary x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Uninterpreted_option.name = _name;
       Uninterpreted_option.identifier_value = _identifier_value;
       Uninterpreted_option.positive_int_value = _positive_int_value;
       Uninterpreted_option.negative_int_value = _negative_int_value;
       Uninterpreted_option.double_value = _double_value;
       Uninterpreted_option.string_value = _string_value;
     })
and parse_uninterpreted_option_name_part x =
  let x = Piqirun.parse_record x in
  let (_name_part, x) = Piqirun.parse_required_field 1 parse_string x in
  let (_is_extension, x) = Piqirun.parse_required_field 2 parse_bool x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Uninterpreted_option_name_part.name_part = _name_part;
       Uninterpreted_option_name_part.is_extension = _is_extension;
     })
  
let rec gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x
and gen__int64 code x = Piqirun.int64_to_zigzag_varint code x
and packed_gen__int64 x = Piqirun.int64_to_packed_zigzag_varint x
and gen__string code x = Piqirun.string_to_block code x
and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x
and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x
and gen__uint64 code x = Piqirun.int64_to_varint code x
and packed_gen__uint64 x = Piqirun.int64_to_packed_varint x
and gen__protobuf_int64 code x = Piqirun.int64_to_signed_varint code x
and packed_gen__protobuf_int64 x = Piqirun.int64_to_packed_signed_varint x
and gen__float64 code x = Piqirun.float_to_fixed64 code x
and packed_gen__float64 x = Piqirun.float_to_packed_fixed64 x
and gen__binary code x = Piqirun.string_to_block code x
and gen__file_descriptor_set code x =
  let _file =
    Piqirun.gen_repeated_field 1 gen__file_descriptor_proto
      x.File_descriptor_set.file
  in Piqirun.gen_record code [ _file ]
and gen__file_descriptor_proto code x =
  let _name =
    Piqirun.gen_optional_field 1 gen__string x.File_descriptor_proto.name in
  let _package =
    Piqirun.gen_optional_field 2 gen__string x.File_descriptor_proto.package in
  let _dependency =
    Piqirun.gen_repeated_field 3 gen__string
      x.File_descriptor_proto.dependency in
  let _message_type =
    Piqirun.gen_repeated_field 4 gen__descriptor_proto
      x.File_descriptor_proto.message_type in
  let _enum_type =
    Piqirun.gen_repeated_field 5 gen__enum_descriptor_proto
      x.File_descriptor_proto.enum_type in
  let _service =
    Piqirun.gen_repeated_field 6 gen__service_descriptor_proto
      x.File_descriptor_proto.service in
  let _extension =
    Piqirun.gen_repeated_field 7 gen__field_descriptor_proto
      x.File_descriptor_proto.extension in
  let _options =
    Piqirun.gen_optional_field 8 gen__file_options
      x.File_descriptor_proto.options
  in
    Piqirun.gen_record code
      [ _name; _package; _dependency; _message_type; _enum_type; _service;
        _extension; _options ]
and gen__descriptor_proto code x =
  let _name =
    Piqirun.gen_optional_field 1 gen__string x.Descriptor_proto.name in
  let _field =
    Piqirun.gen_repeated_field 2 gen__field_descriptor_proto
      x.Descriptor_proto.field in
  let _nested_type =
    Piqirun.gen_repeated_field 3 gen__descriptor_proto
      x.Descriptor_proto.nested_type in
  let _enum_type =
    Piqirun.gen_repeated_field 4 gen__enum_descriptor_proto
      x.Descriptor_proto.enum_type in
  let _extension_range =
    Piqirun.gen_repeated_field 5 gen__descriptor_proto_extension_range
      x.Descriptor_proto.extension_range in
  let _extension =
    Piqirun.gen_repeated_field 6 gen__field_descriptor_proto
      x.Descriptor_proto.extension in
  let _options =
    Piqirun.gen_optional_field 7 gen__message_options
      x.Descriptor_proto.options
  in
    Piqirun.gen_record code
      [ _name; _field; _nested_type; _enum_type; _extension_range;
        _extension; _options ]
and gen__descriptor_proto_extension_range code x =
  let _start =
    Piqirun.gen_optional_field 1 gen__protobuf_int32
      x.Descriptor_proto_extension_range.start in
  let _p_end =
    Piqirun.gen_optional_field 2 gen__protobuf_int32
      x.Descriptor_proto_extension_range.p_end
  in Piqirun.gen_record code [ _start; _p_end ]
and gen__field_descriptor_proto code x =
  let _name =
    Piqirun.gen_optional_field 1 gen__string x.Field_descriptor_proto.name in
  let _extendee =
    Piqirun.gen_optional_field 2 gen__string
      x.Field_descriptor_proto.extendee in
  let _number =
    Piqirun.gen_optional_field 3 gen__protobuf_int32
      x.Field_descriptor_proto.number in
  let _label =
    Piqirun.gen_optional_field 4 gen__field_descriptor_proto_label
      x.Field_descriptor_proto.label in
  let _p_type =
    Piqirun.gen_optional_field 5 gen__field_descriptor_proto_type
      x.Field_descriptor_proto.p_type in
  let _type_name =
    Piqirun.gen_optional_field 6 gen__string
      x.Field_descriptor_proto.type_name in
  let _default_value =
    Piqirun.gen_optional_field 7 gen__string
      x.Field_descriptor_proto.default_value in
  let _options =
    Piqirun.gen_optional_field 8 gen__field_options
      x.Field_descriptor_proto.options
  in
    Piqirun.gen_record code
      [ _name; _extendee; _number; _label; _p_type; _type_name;
        _default_value; _options ]
and gen__field_descriptor_proto_type code x =
  Piqirun.int32_to_signed_varint code
    (match x with
     | `type_double -> 1l
     | `type_float -> 2l
     | `type_int64 -> 3l
     | `type_uint64 -> 4l
     | `type_int32 -> 5l
     | `type_fixed64 -> 6l
     | `type_fixed32 -> 7l
     | `type_bool -> 8l
     | `type_string -> 9l
     | `type_group -> 10l
     | `type_message -> 11l
     | `type_bytes -> 12l
     | `type_uint32 -> 13l
     | `type_enum -> 14l
     | `type_sfixed32 -> 15l
     | `type_sfixed64 -> 16l
     | `type_sint32 -> 17l
     | `type_sint64 -> 18l)
and packed_gen__field_descriptor_proto_type x =
  Piqirun.int32_to_packed_signed_varint
    (match x with
     | `type_double -> 1l
     | `type_float -> 2l
     | `type_int64 -> 3l
     | `type_uint64 -> 4l
     | `type_int32 -> 5l
     | `type_fixed64 -> 6l
     | `type_fixed32 -> 7l
     | `type_bool -> 8l
     | `type_string -> 9l
     | `type_group -> 10l
     | `type_message -> 11l
     | `type_bytes -> 12l
     | `type_uint32 -> 13l
     | `type_enum -> 14l
     | `type_sfixed32 -> 15l
     | `type_sfixed64 -> 16l
     | `type_sint32 -> 17l
     | `type_sint64 -> 18l)
and gen__field_descriptor_proto_label code x =
  Piqirun.int32_to_signed_varint code
    (match x with
     | `label_optional -> 1l
     | `label_required -> 2l
     | `label_repeated -> 3l)
and packed_gen__field_descriptor_proto_label x =
  Piqirun.int32_to_packed_signed_varint
    (match x with
     | `label_optional -> 1l
     | `label_required -> 2l
     | `label_repeated -> 3l)
and gen__enum_descriptor_proto code x =
  let _name =
    Piqirun.gen_optional_field 1 gen__string x.Enum_descriptor_proto.name in
  let _value =
    Piqirun.gen_repeated_field 2 gen__enum_value_descriptor_proto
      x.Enum_descriptor_proto.value in
  let _options =
    Piqirun.gen_optional_field 3 gen__enum_options
      x.Enum_descriptor_proto.options
  in Piqirun.gen_record code [ _name; _value; _options ]
and gen__enum_value_descriptor_proto code x =
  let _name =
    Piqirun.gen_optional_field 1 gen__string
      x.Enum_value_descriptor_proto.name in
  let _number =
    Piqirun.gen_optional_field 2 gen__protobuf_int32
      x.Enum_value_descriptor_proto.number in
  let _options =
    Piqirun.gen_optional_field 3 gen__enum_value_options
      x.Enum_value_descriptor_proto.options
  in Piqirun.gen_record code [ _name; _number; _options ]
and gen__service_descriptor_proto code x =
  let _name =
    Piqirun.gen_optional_field 1 gen__string x.Service_descriptor_proto.name in
  let _p_method =
    Piqirun.gen_repeated_field 2 gen__method_descriptor_proto
      x.Service_descriptor_proto.p_method in
  let _options =
    Piqirun.gen_optional_field 3 gen__service_options
      x.Service_descriptor_proto.options
  in Piqirun.gen_record code [ _name; _p_method; _options ]
and gen__method_descriptor_proto code x =
  let _name =
    Piqirun.gen_optional_field 1 gen__string x.Method_descriptor_proto.name in
  let _input_type =
    Piqirun.gen_optional_field 2 gen__string
      x.Method_descriptor_proto.input_type in
  let _output_type =
    Piqirun.gen_optional_field 3 gen__string
      x.Method_descriptor_proto.output_type in
  let _options =
    Piqirun.gen_optional_field 4 gen__method_options
      x.Method_descriptor_proto.options
  in Piqirun.gen_record code [ _name; _input_type; _output_type; _options ]
and gen__file_options code x =
  let _java_package =
    Piqirun.gen_optional_field 1 gen__string x.File_options.java_package in
  let _java_outer_classname =
    Piqirun.gen_optional_field 8 gen__string
      x.File_options.java_outer_classname in
  let _optimize_for =
    Piqirun.gen_required_field 9 gen__file_options_optimize_mode
      x.File_options.optimize_for in
  let _java_multiple_files =
    Piqirun.gen_required_field 10 gen__bool
      x.File_options.java_multiple_files in
  let _cc_generic_services =
    Piqirun.gen_required_field 16 gen__bool
      x.File_options.cc_generic_services in
  let _java_generic_services =
    Piqirun.gen_required_field 17 gen__bool
      x.File_options.java_generic_services in
  let _py_generic_services =
    Piqirun.gen_required_field 18 gen__bool
      x.File_options.py_generic_services in
  let _uninterpreted_option =
    Piqirun.gen_repeated_field 999 gen__uninterpreted_option
      x.File_options.uninterpreted_option
  in
    Piqirun.gen_record code
      [ _java_package; _java_outer_classname; _optimize_for;
        _java_multiple_files; _cc_generic_services; _java_generic_services;
        _py_generic_services; _uninterpreted_option ]
and gen__file_options_optimize_mode code x =
  Piqirun.int32_to_signed_varint code
    (match x with | `speed -> 1l | `code_size -> 2l | `lite_runtime -> 3l)
and packed_gen__file_options_optimize_mode x =
  Piqirun.int32_to_packed_signed_varint
    (match x with | `speed -> 1l | `code_size -> 2l | `lite_runtime -> 3l)
and gen__message_options code x =
  let _message_set_wire_format =
    Piqirun.gen_required_field 1 gen__bool
      x.Message_options.message_set_wire_format in
  let _no_standard_descriptor_accessor =
    Piqirun.gen_required_field 2 gen__bool
      x.Message_options.no_standard_descriptor_accessor in
  let _uninterpreted_option =
    Piqirun.gen_repeated_field 999 gen__uninterpreted_option
      x.Message_options.uninterpreted_option
  in
    Piqirun.gen_record code
      [ _message_set_wire_format; _no_standard_descriptor_accessor;
        _uninterpreted_option ]
and gen__field_options code x =
  let _ctype =
    Piqirun.gen_required_field 1 gen__field_options_ctype
      x.Field_options.ctype in
  let _packed =
    Piqirun.gen_optional_field 2 gen__bool x.Field_options.packed in
  let _deprecated =
    Piqirun.gen_required_field 3 gen__bool x.Field_options.deprecated in
  let _experimental_map_key =
    Piqirun.gen_optional_field 9 gen__string
      x.Field_options.experimental_map_key in
  let _uninterpreted_option =
    Piqirun.gen_repeated_field 999 gen__uninterpreted_option
      x.Field_options.uninterpreted_option
  in
    Piqirun.gen_record code
      [ _ctype; _packed; _deprecated; _experimental_map_key;
        _uninterpreted_option ]
and gen__field_options_ctype code x =
  Piqirun.int32_to_signed_varint code
    (match x with | `string -> 0l | `cord -> 1l | `string_piece -> 2l)
and packed_gen__field_options_ctype x =
  Piqirun.int32_to_packed_signed_varint
    (match x with | `string -> 0l | `cord -> 1l | `string_piece -> 2l)
and gen__enum_options code x =
  let _uninterpreted_option =
    Piqirun.gen_repeated_field 999 gen__uninterpreted_option
      x.Enum_options.uninterpreted_option
  in Piqirun.gen_record code [ _uninterpreted_option ]
and gen__enum_value_options code x =
  let _uninterpreted_option =
    Piqirun.gen_repeated_field 999 gen__uninterpreted_option
      x.Enum_value_options.uninterpreted_option
  in Piqirun.gen_record code [ _uninterpreted_option ]
and gen__service_options code x =
  let _uninterpreted_option =
    Piqirun.gen_repeated_field 999 gen__uninterpreted_option
      x.Service_options.uninterpreted_option
  in Piqirun.gen_record code [ _uninterpreted_option ]
and gen__method_options code x =
  let _uninterpreted_option =
    Piqirun.gen_repeated_field 999 gen__uninterpreted_option
      x.Method_options.uninterpreted_option
  in Piqirun.gen_record code [ _uninterpreted_option ]
and gen__uninterpreted_option code x =
  let _name =
    Piqirun.gen_repeated_field 2 gen__uninterpreted_option_name_part
      x.Uninterpreted_option.name in
  let _identifier_value =
    Piqirun.gen_optional_field 3 gen__string
      x.Uninterpreted_option.identifier_value in
  let _positive_int_value =
    Piqirun.gen_optional_field 4 gen__uint64
      x.Uninterpreted_option.positive_int_value in
  let _negative_int_value =
    Piqirun.gen_optional_field 5 gen__protobuf_int64
      x.Uninterpreted_option.negative_int_value in
  let _double_value =
    Piqirun.gen_optional_field 6 gen__float64
      x.Uninterpreted_option.double_value in
  let _string_value =
    Piqirun.gen_optional_field 7 gen__binary
      x.Uninterpreted_option.string_value
  in
    Piqirun.gen_record code
      [ _name; _identifier_value; _positive_int_value; _negative_int_value;
        _double_value; _string_value ]
and gen__uninterpreted_option_name_part code x =
  let _name_part =
    Piqirun.gen_required_field 1 gen__string
      x.Uninterpreted_option_name_part.name_part in
  let _is_extension =
    Piqirun.gen_required_field 2 gen__bool
      x.Uninterpreted_option_name_part.is_extension
  in Piqirun.gen_record code [ _name_part; _is_extension ]
  
let gen_int32 x = gen__int32 (-1) x
  
let gen_int64 x = gen__int64 (-1) x
  
let gen_string x = gen__string (-1) x
  
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
  
let gen_bool x = gen__bool (-1) x
  
let gen_uint64 x = gen__uint64 (-1) x
  
let gen_protobuf_int64 x = gen__protobuf_int64 (-1) x
  
let gen_float64 x = gen__float64 (-1) x
  
let gen_binary x = gen__binary (-1) x
  
let gen_file_descriptor_set x = gen__file_descriptor_set (-1) x
  
let gen_file_descriptor_proto x = gen__file_descriptor_proto (-1) x
  
let gen_descriptor_proto x = gen__descriptor_proto (-1) x
  
let gen_descriptor_proto_extension_range x =
  gen__descriptor_proto_extension_range (-1) x
  
let gen_field_descriptor_proto x = gen__field_descriptor_proto (-1) x
  
let gen_field_descriptor_proto_type x =
  gen__field_descriptor_proto_type (-1) x
  
let gen_field_descriptor_proto_label x =
  gen__field_descriptor_proto_label (-1) x
  
let gen_enum_descriptor_proto x = gen__enum_descriptor_proto (-1) x
  
let gen_enum_value_descriptor_proto x =
  gen__enum_value_descriptor_proto (-1) x
  
let gen_service_descriptor_proto x = gen__service_descriptor_proto (-1) x
  
let gen_method_descriptor_proto x = gen__method_descriptor_proto (-1) x
  
let gen_file_options x = gen__file_options (-1) x
  
let gen_file_options_optimize_mode x = gen__file_options_optimize_mode (-1) x
  
let gen_message_options x = gen__message_options (-1) x
  
let gen_field_options x = gen__field_options (-1) x
  
let gen_field_options_ctype x = gen__field_options_ctype (-1) x
  
let gen_enum_options x = gen__enum_options (-1) x
  
let gen_enum_value_options x = gen__enum_value_options (-1) x
  
let gen_service_options x = gen__service_options (-1) x
  
let gen_method_options x = gen__method_options (-1) x
  
let gen_uninterpreted_option x = gen__uninterpreted_option (-1) x
  
let gen_uninterpreted_option_name_part x =
  gen__uninterpreted_option_name_part (-1) x
  
let rec default_int32 () = 0l
and default_int64 () = 0L
and default_string () = ""
and default_protobuf_int32 () = default_int32 ()
and default_bool () = false
and default_uint64 () = 0L
and default_protobuf_int64 () = default_int64 ()
and default_float64 () = 0.0
and default_binary () = ""
and default_file_descriptor_set () = { File_descriptor_set.file = []; }
and default_file_descriptor_proto () =
  {
    File_descriptor_proto.name = None;
    File_descriptor_proto.package = None;
    File_descriptor_proto.dependency = [];
    File_descriptor_proto.message_type = [];
    File_descriptor_proto.enum_type = [];
    File_descriptor_proto.service = [];
    File_descriptor_proto.extension = [];
    File_descriptor_proto.options = None;
  }
and default_descriptor_proto () =
  {
    Descriptor_proto.name = None;
    Descriptor_proto.field = [];
    Descriptor_proto.nested_type = [];
    Descriptor_proto.enum_type = [];
    Descriptor_proto.extension_range = [];
    Descriptor_proto.extension = [];
    Descriptor_proto.options = None;
  }
and default_descriptor_proto_extension_range () =
  {
    Descriptor_proto_extension_range.start = None;
    Descriptor_proto_extension_range.p_end = None;
  }
and default_field_descriptor_proto () =
  {
    Field_descriptor_proto.name = None;
    Field_descriptor_proto.extendee = None;
    Field_descriptor_proto.number = None;
    Field_descriptor_proto.label = None;
    Field_descriptor_proto.p_type = None;
    Field_descriptor_proto.type_name = None;
    Field_descriptor_proto.default_value = None;
    Field_descriptor_proto.options = None;
  }
and default_field_descriptor_proto_type () = `type_double
and default_field_descriptor_proto_label () = `label_optional
and default_enum_descriptor_proto () =
  {
    Enum_descriptor_proto.name = None;
    Enum_descriptor_proto.value = [];
    Enum_descriptor_proto.options = None;
  }
and default_enum_value_descriptor_proto () =
  {
    Enum_value_descriptor_proto.name = None;
    Enum_value_descriptor_proto.number = None;
    Enum_value_descriptor_proto.options = None;
  }
and default_service_descriptor_proto () =
  {
    Service_descriptor_proto.name = None;
    Service_descriptor_proto.p_method = [];
    Service_descriptor_proto.options = None;
  }
and default_method_descriptor_proto () =
  {
    Method_descriptor_proto.name = None;
    Method_descriptor_proto.input_type = None;
    Method_descriptor_proto.output_type = None;
    Method_descriptor_proto.options = None;
  }
and default_file_options () =
  {
    File_options.java_package = None;
    File_options.java_outer_classname = None;
    File_options.optimize_for =
      parse_file_options_optimize_mode (Piqirun.parse_default "\b\001");
    File_options.java_multiple_files =
      parse_bool (Piqirun.parse_default "\b\000");
    File_options.cc_generic_services =
      parse_bool (Piqirun.parse_default "\b\001");
    File_options.java_generic_services =
      parse_bool (Piqirun.parse_default "\b\001");
    File_options.py_generic_services =
      parse_bool (Piqirun.parse_default "\b\001");
    File_options.uninterpreted_option = [];
  }
and default_file_options_optimize_mode () = `speed
and default_message_options () =
  {
    Message_options.message_set_wire_format =
      parse_bool (Piqirun.parse_default "\b\000");
    Message_options.no_standard_descriptor_accessor =
      parse_bool (Piqirun.parse_default "\b\000");
    Message_options.uninterpreted_option = [];
  }
and default_field_options () =
  {
    Field_options.ctype =
      parse_field_options_ctype (Piqirun.parse_default "\b\000");
    Field_options.packed = None;
    Field_options.deprecated = parse_bool (Piqirun.parse_default "\b\000");
    Field_options.experimental_map_key = None;
    Field_options.uninterpreted_option = [];
  }
and default_field_options_ctype () = `string
and default_enum_options () = { Enum_options.uninterpreted_option = []; }
and default_enum_value_options () =
  { Enum_value_options.uninterpreted_option = []; }
and default_service_options () =
  { Service_options.uninterpreted_option = []; }
and default_method_options () = { Method_options.uninterpreted_option = []; }
and default_uninterpreted_option () =
  {
    Uninterpreted_option.name = [];
    Uninterpreted_option.identifier_value = None;
    Uninterpreted_option.positive_int_value = None;
    Uninterpreted_option.negative_int_value = None;
    Uninterpreted_option.double_value = None;
    Uninterpreted_option.string_value = None;
  }
and default_uninterpreted_option_name_part () =
  {
    Uninterpreted_option_name_part.name_part = default_string ();
    Uninterpreted_option_name_part.is_extension = default_bool ();
  }
  
include Descriptor_piqi
  

