module Piqirun = Piqi_piqirun
  
module rec Piqi_piqi :
             sig
               type uint = int
               
               type uint32 = int32
               
               type uint64 = int64
               
               type float64 = float
               
               type float32 = float
               
               type protobuf_int32 = int32
               
               type protobuf_int64 = int64
               
               type binary = string
               
               type piqi_any = Piqi_piqi.any
               
               type int32_fixed = int32
               
               type uint32_fixed = Piqi_piqi.uint32
               
               type int64_fixed = int64
               
               type uint64_fixed = Piqi_piqi.uint64
               
               type float = Piqi_piqi.float64
               
               type word = string
               
               type name = Piqi_piqi.word
               
               type typename = Piqi_piqi.name
               
               type piq_format = [ | `word | `text ]
               
               type protobuf_wire_type =
                 [
                   | `varint
                   | `zigzag_varint
                   | `fixed32
                   | `fixed64
                   | `signed_varint
                   | `signed_fixed32
                   | `signed_fixed64
                   | `block
                 ]
               
               type typedef =
                 [
                   | `record of Piqi_piqi.record
                   | `variant of Piqi_piqi.variant
                   | `enum of Piqi_piqi.enum
                   | `alias of Piqi_piqi.alias
                   | `list of Piqi_piqi.piqi_list
                 ]
               
               type piqi_type =
                 [ | `int | `float | `bool | `string | `binary | `any
                 ]
               
               type field_mode = [ | `required | `optional | `repeated ]
               
               type record = Record.t
               
               type field = Field.t
               
               type variant = Variant.t
               
               type option = Option.t
               
               type enum = Enum.t
               
               type alias = Alias.t
               
               type piqi_list = Piqi_list.t
               
               type piqi = Piqi.t
               
               type import = Import.t
               
               type any = Any.t
               
               type func = Func.t
               
               type piqi_bundle = Piqi_bundle.t
               
             end = Piqi_piqi
and
  Record :
    sig
      type t =
        { mutable name : Piqi_piqi.name;
          mutable field : Piqi_piqi.field list;
          mutable piq_positional : bool option;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable json_name : string option
        }
      
    end = Record
and
  Field :
    sig
      type t =
        { mutable name : Piqi_piqi.name option;
          mutable typename : Piqi_piqi.typename option;
          mutable mode : Piqi_piqi.field_mode;
          mutable default : Piqi_piqi.piqi_any option;
          mutable deprecated : bool;
          mutable piq_format : Piqi_piqi.piq_format option;
          mutable piq_positional : bool option;
          mutable piq_alias : Piqi_piqi.name option;
          mutable protobuf_name : string option; mutable code : int32 option;
          mutable protobuf_packed : bool; mutable json_name : string option;
          mutable json_omit_missing : bool option;
          mutable getopt_letter : Piqi_piqi.word option;
          mutable getopt_doc : string option
        }
      
    end = Field
and
  Variant :
    sig
      type t =
        { mutable name : Piqi_piqi.name;
          mutable option : Piqi_piqi.option list;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable json_name : string option
        }
      
    end = Variant
and
  Option :
    sig
      type t =
        { mutable name : Piqi_piqi.name option;
          mutable typename : Piqi_piqi.typename option;
          mutable deprecated : bool;
          mutable piq_format : Piqi_piqi.piq_format option;
          mutable piq_alias : Piqi_piqi.name option;
          mutable protobuf_name : string option; mutable code : int32 option;
          mutable json_name : string option;
          mutable getopt_letter : Piqi_piqi.word option;
          mutable getopt_doc : string option
        }
      
    end = Option
and
  Enum :
    sig
      type t =
        { mutable name : Piqi_piqi.name;
          mutable option : Piqi_piqi.option list;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable protobuf_prefix : string option;
          mutable json_name : string option
        }
      
    end = Enum
and
  Alias :
    sig
      type t =
        { mutable name : Piqi_piqi.name;
          mutable typename : Piqi_piqi.typename option;
          mutable piqi_type : Piqi_piqi.piqi_type option;
          mutable piq_format : Piqi_piqi.piq_format option;
          mutable protobuf_name : string option;
          mutable protobuf_type : string option;
          mutable protobuf_wire_type : Piqi_piqi.protobuf_wire_type option;
          mutable json_name : string option
        }
      
    end = Alias
and
  Piqi_list :
    sig
      type t =
        { mutable name : Piqi_piqi.name;
          mutable typename : Piqi_piqi.typename;
          mutable piq_format : Piqi_piqi.piq_format option;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable protobuf_packed : bool; mutable json_name : string option
        }
      
    end = Piqi_list
and
  Piqi :
    sig
      type t =
        { mutable modname : Piqi_piqi.word option;
          mutable typedef : Piqi_piqi.typedef list;
          mutable import : Piqi_piqi.import list;
          mutable func : Piqi_piqi.func list;
          mutable custom_field : Piqi_piqi.word list;
          mutable protobuf_custom : string list;
          mutable protobuf_package : string option;
          mutable file : string option
        }
      
    end = Piqi
and
  Import :
    sig
      type t =
        { mutable modname : Piqi_piqi.word;
          mutable name : Piqi_piqi.name option
        }
      
    end = Import
and
  Any :
    sig
      type t =
        { mutable typename : string option;
          mutable protobuf : Piqi_piqi.binary option;
          mutable json : string option; mutable xml : string option
        }
      
    end = Any
and
  Func :
    sig
      type t =
        { mutable name : Piqi_piqi.name;
          mutable input : Piqi_piqi.typename option;
          mutable output : Piqi_piqi.typename option;
          mutable error : Piqi_piqi.typename option
        }
      
    end = Func
and
  Piqi_bundle : sig type t = { mutable piqi : Piqi_piqi.piqi list }
                     end =
    Piqi_bundle
  
let rec parse_piq_format x =
  let (code, x) = Piqirun.parse_variant x
  in
    match code with
    | 251462090 when x = (Piqirun.Varint 1) -> `word
    | 217697453 when x = (Piqirun.Varint 1) -> `text
    | _ -> Piqirun.error_variant x code
and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x
and parse_protobuf_int64 x = Piqirun.int64_of_signed_varint x
and packed_parse_protobuf_int64 x = Piqirun.int64_of_packed_signed_varint x
and parse_protobuf_wire_type x =
  match Piqirun.int32_of_signed_varint x with
  | 329594984l -> `varint
  | 99211597l -> `zigzag_varint
  | 136997651l -> `fixed32
  | 136998322l -> `fixed64
  | 441915897l -> `signed_varint
  | 488499298l -> `signed_fixed32
  | 488499969l -> `signed_fixed64
  | 352089421l -> `block
  | x -> Piqirun.error_enum_const x
and packed_parse_protobuf_wire_type x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 329594984l -> `varint
  | 99211597l -> `zigzag_varint
  | 136997651l -> `fixed32
  | 136998322l -> `fixed64
  | 441915897l -> `signed_varint
  | 488499298l -> `signed_fixed32
  | 488499969l -> `signed_fixed64
  | 352089421l -> `block
  | x -> Piqirun.error_enum_const x
and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x
and parse_string x = Piqirun.string_of_block x
and parse_binary x = Piqirun.string_of_block x
and parse_piqi_any x = parse_any x
and parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x
and parse_uint x = Piqirun.int_of_varint x
and packed_parse_uint x = Piqirun.int_of_packed_varint x
and parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x
and parse_uint32 x = Piqirun.int32_of_varint x
and packed_parse_uint32 x = Piqirun.int32_of_packed_varint x
and parse_int64 x = Piqirun.int64_of_zigzag_varint x
and packed_parse_int64 x = Piqirun.int64_of_packed_zigzag_varint x
and parse_uint64 x = Piqirun.int64_of_varint x
and packed_parse_uint64 x = Piqirun.int64_of_packed_varint x
and parse_float64 x = Piqirun.float_of_fixed64 x
and packed_parse_float64 x = Piqirun.float_of_packed_fixed64 x
and parse_float32 x = Piqirun.float_of_fixed32 x
and packed_parse_float32 x = Piqirun.float_of_packed_fixed32 x
and parse_int32_fixed x = Piqirun.int32_of_signed_fixed32 x
and packed_parse_int32_fixed x = Piqirun.int32_of_packed_signed_fixed32 x
and parse_uint32_fixed x = Piqirun.int32_of_fixed32 x
and packed_parse_uint32_fixed x = Piqirun.int32_of_packed_fixed32 x
and parse_int64_fixed x = Piqirun.int64_of_signed_fixed64 x
and packed_parse_int64_fixed x = Piqirun.int64_of_packed_signed_fixed64 x
and parse_uint64_fixed x = Piqirun.int64_of_fixed64 x
and packed_parse_uint64_fixed x = Piqirun.int64_of_packed_fixed64 x
and parse_float x = parse_float64 x
and packed_parse_float x = packed_parse_float64 x
and parse_word x = parse_string x
and parse_name x = parse_word x
and parse_typedef x =
  let (code, x) = Piqirun.parse_variant x
  in
    match code with
    | 502036113 -> let res = parse_record x in `record res
    | 484589701 -> let res = parse_variant x in `variant res
    | 51800833 -> let res = parse_enum x in `enum res
    | 26300816 -> let res = parse_alias x in `alias res
    | 129178718 -> let res = parse_piqi_list x in `list res
    | _ -> Piqirun.error_variant x code
and parse_piqi_type x =
  match Piqirun.int32_of_signed_varint x with
  | 5246191l -> `int
  | 43435420l -> `float
  | 18580522l -> `bool
  | 288368849l -> `string
  | 218872833l -> `binary
  | 4848364l -> `any
  | x -> Piqirun.error_enum_const x
and packed_parse_piqi_type x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 5246191l -> `int
  | 43435420l -> `float
  | 18580522l -> `bool
  | 288368849l -> `string
  | 218872833l -> `binary
  | 4848364l -> `any
  | x -> Piqirun.error_enum_const x
and parse_typename x = parse_name x
and parse_record x =
  let x = Piqirun.parse_record x in
  let (_field, x) = Piqirun.parse_repeated_field 9671866 parse_field x in
  let (_protobuf_name, x) =
    Piqirun.parse_optional_field 90072013 parse_string x in
  let (_protobuf_custom, x) =
    Piqirun.parse_repeated_field 112352691 parse_string x in
  let (_name, x) = Piqirun.parse_required_field 150958667 parse_name x in
  let (_piq_positional, x) =
    Piqirun.parse_optional_field 197354217 parse_bool x in
  let (_json_name, x) = Piqirun.parse_optional_field 515275216 parse_string x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Record.field = _field;
       Record.protobuf_name = _protobuf_name;
       Record.protobuf_custom = _protobuf_custom;
       Record.name = _name;
       Record.piq_positional = _piq_positional;
       Record.json_name = _json_name;
     })
and parse_field x =
  let x = Piqirun.parse_record x in
  let (_code, x) = Piqirun.parse_optional_field 29667629 parse_int32 x in
  let (_deprecated, x) = Piqirun.parse_flag 69402483 x in
  let (_protobuf_name, x) =
    Piqirun.parse_optional_field 90072013 parse_string x in
  let (_mode, x) =
    Piqirun.parse_required_field 140563299 parse_field_mode x
      ~default: "\b\223\162\138\147\001" in
  let (_name, x) = Piqirun.parse_optional_field 150958667 parse_name x in
  let (_protobuf_packed, x) = Piqirun.parse_flag 179842426 x in
  let (_piq_positional, x) =
    Piqirun.parse_optional_field 197354217 parse_bool x in
  let (_json_omit_missing, x) =
    Piqirun.parse_optional_field 201807079 parse_bool x in
  let (_getopt_letter, x) =
    Piqirun.parse_optional_field 215188758 parse_word x in
  let (_typename, x) =
    Piqirun.parse_optional_field 218690234 parse_typename x in
  let (_piq_format, x) =
    Piqirun.parse_optional_field 296833484 parse_piq_format x in
  let (_piq_alias, x) =
    Piqirun.parse_optional_field 434682011 parse_name x in
  let (_getopt_doc, x) =
    Piqirun.parse_optional_field 442330184 parse_string x in
  let (_default, x) =
    Piqirun.parse_optional_field 465819841 parse_piqi_any x in
  let (_json_name, x) = Piqirun.parse_optional_field 515275216 parse_string x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Field.code = _code;
       Field.deprecated = _deprecated;
       Field.protobuf_name = _protobuf_name;
       Field.mode = _mode;
       Field.name = _name;
       Field.protobuf_packed = _protobuf_packed;
       Field.piq_positional = _piq_positional;
       Field.json_omit_missing = _json_omit_missing;
       Field.getopt_letter = _getopt_letter;
       Field.typename = _typename;
       Field.piq_format = _piq_format;
       Field.piq_alias = _piq_alias;
       Field.getopt_doc = _getopt_doc;
       Field.default = _default;
       Field.json_name = _json_name;
     })
and parse_field_mode x =
  match Piqirun.int32_of_signed_varint x with
  | 308449631l -> `required
  | 510570400l -> `optional
  | 274054266l -> `repeated
  | x -> Piqirun.error_enum_const x
and packed_parse_field_mode x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 308449631l -> `required
  | 510570400l -> `optional
  | 274054266l -> `repeated
  | x -> Piqirun.error_enum_const x
and parse_variant x =
  let x = Piqirun.parse_record x in
  let (_protobuf_name, x) =
    Piqirun.parse_optional_field 90072013 parse_string x in
  let (_protobuf_custom, x) =
    Piqirun.parse_repeated_field 112352691 parse_string x in
  let (_name, x) = Piqirun.parse_required_field 150958667 parse_name x in
  let (_option, x) = Piqirun.parse_repeated_field 192598901 parse_option x in
  let (_json_name, x) = Piqirun.parse_optional_field 515275216 parse_string x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Variant.protobuf_name = _protobuf_name;
       Variant.protobuf_custom = _protobuf_custom;
       Variant.name = _name;
       Variant.option = _option;
       Variant.json_name = _json_name;
     })
and parse_option x =
  let x = Piqirun.parse_record x in
  let (_code, x) = Piqirun.parse_optional_field 29667629 parse_int32 x in
  let (_deprecated, x) = Piqirun.parse_flag 69402483 x in
  let (_protobuf_name, x) =
    Piqirun.parse_optional_field 90072013 parse_string x in
  let (_name, x) = Piqirun.parse_optional_field 150958667 parse_name x in
  let (_getopt_letter, x) =
    Piqirun.parse_optional_field 215188758 parse_word x in
  let (_typename, x) =
    Piqirun.parse_optional_field 218690234 parse_typename x in
  let (_piq_format, x) =
    Piqirun.parse_optional_field 296833484 parse_piq_format x in
  let (_piq_alias, x) =
    Piqirun.parse_optional_field 434682011 parse_name x in
  let (_getopt_doc, x) =
    Piqirun.parse_optional_field 442330184 parse_string x in
  let (_json_name, x) = Piqirun.parse_optional_field 515275216 parse_string x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Option.code = _code;
       Option.deprecated = _deprecated;
       Option.protobuf_name = _protobuf_name;
       Option.name = _name;
       Option.getopt_letter = _getopt_letter;
       Option.typename = _typename;
       Option.piq_format = _piq_format;
       Option.piq_alias = _piq_alias;
       Option.getopt_doc = _getopt_doc;
       Option.json_name = _json_name;
     })
and parse_enum x =
  let x = Piqirun.parse_record x in
  let (_protobuf_name, x) =
    Piqirun.parse_optional_field 90072013 parse_string x in
  let (_protobuf_custom, x) =
    Piqirun.parse_repeated_field 112352691 parse_string x in
  let (_name, x) = Piqirun.parse_required_field 150958667 parse_name x in
  let (_option, x) = Piqirun.parse_repeated_field 192598901 parse_option x in
  let (_protobuf_prefix, x) =
    Piqirun.parse_optional_field 366391188 parse_string x in
  let (_json_name, x) = Piqirun.parse_optional_field 515275216 parse_string x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Enum.protobuf_name = _protobuf_name;
       Enum.protobuf_custom = _protobuf_custom;
       Enum.name = _name;
       Enum.option = _option;
       Enum.protobuf_prefix = _protobuf_prefix;
       Enum.json_name = _json_name;
     })
and parse_alias x =
  let x = Piqirun.parse_record x in
  let (_protobuf_name, x) =
    Piqirun.parse_optional_field 90072013 parse_string x in
  let (_name, x) = Piqirun.parse_required_field 150958667 parse_name x in
  let (_protobuf_type, x) =
    Piqirun.parse_optional_field 157803580 parse_string x in
  let (_protobuf_wire_type, x) =
    Piqirun.parse_optional_field 198202944 parse_protobuf_wire_type x in
  let (_piqi_type, x) =
    Piqirun.parse_optional_field 198318774 parse_piqi_type x in
  let (_typename, x) =
    Piqirun.parse_optional_field 218690234 parse_typename x in
  let (_piq_format, x) =
    Piqirun.parse_optional_field 296833484 parse_piq_format x in
  let (_json_name, x) = Piqirun.parse_optional_field 515275216 parse_string x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Alias.protobuf_name = _protobuf_name;
       Alias.name = _name;
       Alias.protobuf_type = _protobuf_type;
       Alias.protobuf_wire_type = _protobuf_wire_type;
       Alias.piqi_type = _piqi_type;
       Alias.typename = _typename;
       Alias.piq_format = _piq_format;
       Alias.json_name = _json_name;
     })
and parse_piqi_list x =
  let x = Piqirun.parse_record x in
  let (_protobuf_name, x) =
    Piqirun.parse_optional_field 90072013 parse_string x in
  let (_protobuf_custom, x) =
    Piqirun.parse_repeated_field 112352691 parse_string x in
  let (_name, x) = Piqirun.parse_required_field 150958667 parse_name x in
  let (_protobuf_packed, x) = Piqirun.parse_flag 179842426 x in
  let (_typename, x) =
    Piqirun.parse_required_field 218690234 parse_typename x in
  let (_piq_format, x) =
    Piqirun.parse_optional_field 296833484 parse_piq_format x in
  let (_json_name, x) = Piqirun.parse_optional_field 515275216 parse_string x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Piqi_list.protobuf_name = _protobuf_name;
       Piqi_list.protobuf_custom = _protobuf_custom;
       Piqi_list.name = _name;
       Piqi_list.protobuf_packed = _protobuf_packed;
       Piqi_list.typename = _typename;
       Piqi_list.piq_format = _piq_format;
       Piqi_list.json_name = _json_name;
     })
and parse_piqi x =
  let x = Piqirun.parse_record x in
  let (_modname, x) = Piqirun.parse_optional_field 13841580 parse_word x in
  let (_file, x) = Piqirun.parse_optional_field 62639740 parse_string x in
  let (_protobuf_custom, x) =
    Piqirun.parse_repeated_field 112352691 parse_string x in
  let (_import, x) = Piqirun.parse_repeated_field 142778725 parse_import x in
  let (_custom_field, x) =
    Piqirun.parse_repeated_field 162247646 parse_word x in
  let (_func, x) = Piqirun.parse_repeated_field 340962072 parse_func x in
  let (_protobuf_package, x) =
    Piqirun.parse_optional_field 376215364 parse_string x in
  let (_typedef, x) = Piqirun.parse_repeated_field 416823115 parse_typedef x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Piqi.modname = _modname;
       Piqi.file = _file;
       Piqi.protobuf_custom = _protobuf_custom;
       Piqi.import = _import;
       Piqi.custom_field = _custom_field;
       Piqi.func = _func;
       Piqi.protobuf_package = _protobuf_package;
       Piqi.typedef = _typedef;
     })
and parse_import x =
  let x = Piqirun.parse_record x in
  let (_modname, x) = Piqirun.parse_required_field 13841580 parse_word x in
  let (_name, x) = Piqirun.parse_optional_field 150958667 parse_name x
  in
    (Piqirun.check_unparsed_fields x;
     { Import.modname = _modname; Import.name = _name; })
and parse_any x =
  let x = Piqirun.parse_record x in
  let (_xml, x) = Piqirun.parse_optional_field 5991895 parse_string x in
  let (_protobuf, x) = Piqirun.parse_optional_field 6461771 parse_binary x in
  let (_json, x) = Piqirun.parse_optional_field 107495976 parse_string x in
  let (_typename, x) = Piqirun.parse_optional_field 218690234 parse_string x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Any.xml = _xml;
       Any.protobuf = _protobuf;
       Any.json = _json;
       Any.typename = _typename;
     })
and parse_func x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_required_field 150958667 parse_name x in
  let (_output, x) =
    Piqirun.parse_optional_field 209784577 parse_typename x in
  let (_error, x) =
    Piqirun.parse_optional_field 321506248 parse_typename x in
  let (_input, x) = Piqirun.parse_optional_field 505267210 parse_typename x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Func.name = _name;
       Func.output = _output;
       Func.error = _error;
       Func.input = _input;
     })
and parse_piqi_bundle x =
  let x = Piqirun.parse_record x in
  let (_piqi, x) = Piqirun.parse_repeated_field 1 parse_piqi x
  in (Piqirun.check_unparsed_fields x; { Piqi_bundle.piqi = _piqi; })
  
let rec gen__piq_format code (x : Piqi_piqi.piq_format) =
  Piqirun.gen_record code
    [ (match x with
       | `word -> Piqirun.gen_bool_field 251462090 true
       | `text -> Piqirun.gen_bool_field 217697453 true) ]
and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x
and gen__protobuf_int64 code x = Piqirun.int64_to_signed_varint code x
and packed_gen__protobuf_int64 x = Piqirun.int64_to_packed_signed_varint x
and gen__protobuf_wire_type code x =
  Piqirun.int32_to_signed_varint code
    (match x with
     | `varint -> 329594984l
     | `zigzag_varint -> 99211597l
     | `fixed32 -> 136997651l
     | `fixed64 -> 136998322l
     | `signed_varint -> 441915897l
     | `signed_fixed32 -> 488499298l
     | `signed_fixed64 -> 488499969l
     | `block -> 352089421l)
and packed_gen__protobuf_wire_type x =
  Piqirun.int32_to_packed_signed_varint
    (match x with
     | `varint -> 329594984l
     | `zigzag_varint -> 99211597l
     | `fixed32 -> 136997651l
     | `fixed64 -> 136998322l
     | `signed_varint -> 441915897l
     | `signed_fixed32 -> 488499298l
     | `signed_fixed64 -> 488499969l
     | `block -> 352089421l)
and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x
and gen__string code x = Piqirun.string_to_block code x
and gen__binary code x = Piqirun.string_to_block code x
and gen__piqi_any code x = (fun code x -> gen__any code x) code x
and gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x
and gen__uint code x = Piqirun.int_to_varint code x
and packed_gen__uint x = Piqirun.int_to_packed_varint x
and gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x
and gen__uint32 code x = Piqirun.int32_to_varint code x
and packed_gen__uint32 x = Piqirun.int32_to_packed_varint x
and gen__int64 code x = Piqirun.int64_to_zigzag_varint code x
and packed_gen__int64 x = Piqirun.int64_to_packed_zigzag_varint x
and gen__uint64 code x = Piqirun.int64_to_varint code x
and packed_gen__uint64 x = Piqirun.int64_to_packed_varint x
and gen__float64 code x = Piqirun.float_to_fixed64 code x
and packed_gen__float64 x = Piqirun.float_to_packed_fixed64 x
and gen__float32 code x = Piqirun.float_to_fixed32 code x
and packed_gen__float32 x = Piqirun.float_to_packed_fixed32 x
and gen__int32_fixed code x = Piqirun.int32_to_signed_fixed32 code x
and packed_gen__int32_fixed x = Piqirun.int32_to_packed_signed_fixed32 x
and gen__uint32_fixed code x = Piqirun.int32_to_fixed32 code x
and packed_gen__uint32_fixed x = Piqirun.int32_to_packed_fixed32 x
and gen__int64_fixed code x = Piqirun.int64_to_signed_fixed64 code x
and packed_gen__int64_fixed x = Piqirun.int64_to_packed_signed_fixed64 x
and gen__uint64_fixed code x = Piqirun.int64_to_fixed64 code x
and packed_gen__uint64_fixed x = Piqirun.int64_to_packed_fixed64 x
and gen__float code x = gen__float64 code x
and packed_gen__float x = packed_gen__float64 x
and gen__word code x = gen__string code x
and gen__name code x = gen__word code x
and gen__typedef code (x : Piqi_piqi.typedef) =
  Piqirun.gen_record code
    [ (match x with
       | `record x -> gen__record 502036113 x
       | `variant x -> gen__variant 484589701 x
       | `enum x -> gen__enum 51800833 x
       | `alias x -> gen__alias 26300816 x
       | `list x -> gen__piqi_list 129178718 x) ]
and gen__piqi_type code x =
  Piqirun.int32_to_signed_varint code
    (match x with
     | `int -> 5246191l
     | `float -> 43435420l
     | `bool -> 18580522l
     | `string -> 288368849l
     | `binary -> 218872833l
     | `any -> 4848364l)
and packed_gen__piqi_type x =
  Piqirun.int32_to_packed_signed_varint
    (match x with
     | `int -> 5246191l
     | `float -> 43435420l
     | `bool -> 18580522l
     | `string -> 288368849l
     | `binary -> 218872833l
     | `any -> 4848364l)
and gen__typename code x = gen__name code x
and gen__record code x =
  let _field =
    Piqirun.gen_repeated_field 9671866 gen__field x.Record.field in
  let _protobuf_name =
    Piqirun.gen_optional_field 90072013 gen__string x.Record.protobuf_name in
  let _protobuf_custom =
    Piqirun.gen_repeated_field 112352691 gen__string x.Record.protobuf_custom in
  let _name = Piqirun.gen_required_field 150958667 gen__name x.Record.name in
  let _piq_positional =
    Piqirun.gen_optional_field 197354217 gen__bool x.Record.piq_positional in
  let _json_name =
    Piqirun.gen_optional_field 515275216 gen__string x.Record.json_name
  in
    Piqirun.gen_record code
      [ _field; _protobuf_name; _protobuf_custom; _name; _piq_positional;
        _json_name ]
and gen__field code x =
  let _code = Piqirun.gen_optional_field 29667629 gen__int32 x.Field.code in
  let _deprecated = Piqirun.gen_flag 69402483 x.Field.deprecated in
  let _protobuf_name =
    Piqirun.gen_optional_field 90072013 gen__string x.Field.protobuf_name in
  let _mode =
    Piqirun.gen_required_field 140563299 gen__field_mode x.Field.mode in
  let _name = Piqirun.gen_optional_field 150958667 gen__name x.Field.name in
  let _protobuf_packed =
    Piqirun.gen_flag 179842426 x.Field.protobuf_packed in
  let _piq_positional =
    Piqirun.gen_optional_field 197354217 gen__bool x.Field.piq_positional in
  let _json_omit_missing =
    Piqirun.gen_optional_field 201807079 gen__bool x.Field.json_omit_missing in
  let _getopt_letter =
    Piqirun.gen_optional_field 215188758 gen__word x.Field.getopt_letter in
  let _typename =
    Piqirun.gen_optional_field 218690234 gen__typename x.Field.typename in
  let _piq_format =
    Piqirun.gen_optional_field 296833484 gen__piq_format x.Field.piq_format in
  let _piq_alias =
    Piqirun.gen_optional_field 434682011 gen__name x.Field.piq_alias in
  let _getopt_doc =
    Piqirun.gen_optional_field 442330184 gen__string x.Field.getopt_doc in
  let _default =
    Piqirun.gen_optional_field 465819841 gen__piqi_any x.Field.default in
  let _json_name =
    Piqirun.gen_optional_field 515275216 gen__string x.Field.json_name
  in
    Piqirun.gen_record code
      [ _code; _deprecated; _protobuf_name; _mode; _name; _protobuf_packed;
        _piq_positional; _json_omit_missing; _getopt_letter; _typename;
        _piq_format; _piq_alias; _getopt_doc; _default; _json_name ]
and gen__field_mode code x =
  Piqirun.int32_to_signed_varint code
    (match x with
     | `required -> 308449631l
     | `optional -> 510570400l
     | `repeated -> 274054266l)
and packed_gen__field_mode x =
  Piqirun.int32_to_packed_signed_varint
    (match x with
     | `required -> 308449631l
     | `optional -> 510570400l
     | `repeated -> 274054266l)
and gen__variant code x =
  let _protobuf_name =
    Piqirun.gen_optional_field 90072013 gen__string x.Variant.protobuf_name in
  let _protobuf_custom =
    Piqirun.gen_repeated_field 112352691 gen__string
      x.Variant.protobuf_custom in
  let _name =
    Piqirun.gen_required_field 150958667 gen__name x.Variant.name in
  let _option =
    Piqirun.gen_repeated_field 192598901 gen__option x.Variant.option in
  let _json_name =
    Piqirun.gen_optional_field 515275216 gen__string x.Variant.json_name
  in
    Piqirun.gen_record code
      [ _protobuf_name; _protobuf_custom; _name; _option; _json_name ]
and gen__option code x =
  let _code = Piqirun.gen_optional_field 29667629 gen__int32 x.Option.code in
  let _deprecated = Piqirun.gen_flag 69402483 x.Option.deprecated in
  let _protobuf_name =
    Piqirun.gen_optional_field 90072013 gen__string x.Option.protobuf_name in
  let _name = Piqirun.gen_optional_field 150958667 gen__name x.Option.name in
  let _getopt_letter =
    Piqirun.gen_optional_field 215188758 gen__word x.Option.getopt_letter in
  let _typename =
    Piqirun.gen_optional_field 218690234 gen__typename x.Option.typename in
  let _piq_format =
    Piqirun.gen_optional_field 296833484 gen__piq_format x.Option.piq_format in
  let _piq_alias =
    Piqirun.gen_optional_field 434682011 gen__name x.Option.piq_alias in
  let _getopt_doc =
    Piqirun.gen_optional_field 442330184 gen__string x.Option.getopt_doc in
  let _json_name =
    Piqirun.gen_optional_field 515275216 gen__string x.Option.json_name
  in
    Piqirun.gen_record code
      [ _code; _deprecated; _protobuf_name; _name; _getopt_letter; _typename;
        _piq_format; _piq_alias; _getopt_doc; _json_name ]
and gen__enum code x =
  let _protobuf_name =
    Piqirun.gen_optional_field 90072013 gen__string x.Enum.protobuf_name in
  let _protobuf_custom =
    Piqirun.gen_repeated_field 112352691 gen__string x.Enum.protobuf_custom in
  let _name = Piqirun.gen_required_field 150958667 gen__name x.Enum.name in
  let _option =
    Piqirun.gen_repeated_field 192598901 gen__option x.Enum.option in
  let _protobuf_prefix =
    Piqirun.gen_optional_field 366391188 gen__string x.Enum.protobuf_prefix in
  let _json_name =
    Piqirun.gen_optional_field 515275216 gen__string x.Enum.json_name
  in
    Piqirun.gen_record code
      [ _protobuf_name; _protobuf_custom; _name; _option; _protobuf_prefix;
        _json_name ]
and gen__alias code x =
  let _protobuf_name =
    Piqirun.gen_optional_field 90072013 gen__string x.Alias.protobuf_name in
  let _name = Piqirun.gen_required_field 150958667 gen__name x.Alias.name in
  let _protobuf_type =
    Piqirun.gen_optional_field 157803580 gen__string x.Alias.protobuf_type in
  let _protobuf_wire_type =
    Piqirun.gen_optional_field 198202944 gen__protobuf_wire_type
      x.Alias.protobuf_wire_type in
  let _piqi_type =
    Piqirun.gen_optional_field 198318774 gen__piqi_type x.Alias.piqi_type in
  let _typename =
    Piqirun.gen_optional_field 218690234 gen__typename x.Alias.typename in
  let _piq_format =
    Piqirun.gen_optional_field 296833484 gen__piq_format x.Alias.piq_format in
  let _json_name =
    Piqirun.gen_optional_field 515275216 gen__string x.Alias.json_name
  in
    Piqirun.gen_record code
      [ _protobuf_name; _name; _protobuf_type; _protobuf_wire_type;
        _piqi_type; _typename; _piq_format; _json_name ]
and gen__piqi_list code x =
  let _protobuf_name =
    Piqirun.gen_optional_field 90072013 gen__string x.Piqi_list.protobuf_name in
  let _protobuf_custom =
    Piqirun.gen_repeated_field 112352691 gen__string
      x.Piqi_list.protobuf_custom in
  let _name =
    Piqirun.gen_required_field 150958667 gen__name x.Piqi_list.name in
  let _protobuf_packed =
    Piqirun.gen_flag 179842426 x.Piqi_list.protobuf_packed in
  let _typename =
    Piqirun.gen_required_field 218690234 gen__typename x.Piqi_list.typename in
  let _piq_format =
    Piqirun.gen_optional_field 296833484 gen__piq_format
      x.Piqi_list.piq_format in
  let _json_name =
    Piqirun.gen_optional_field 515275216 gen__string x.Piqi_list.json_name
  in
    Piqirun.gen_record code
      [ _protobuf_name; _protobuf_custom; _name; _protobuf_packed; _typename;
        _piq_format; _json_name ]
and gen__piqi code x =
  let _modname =
    Piqirun.gen_optional_field 13841580 gen__word x.Piqi.modname in
  let _file = Piqirun.gen_optional_field 62639740 gen__string x.Piqi.file in
  let _protobuf_custom =
    Piqirun.gen_repeated_field 112352691 gen__string x.Piqi.protobuf_custom in
  let _import =
    Piqirun.gen_repeated_field 142778725 gen__import x.Piqi.import in
  let _custom_field =
    Piqirun.gen_repeated_field 162247646 gen__word x.Piqi.custom_field in
  let _func = Piqirun.gen_repeated_field 340962072 gen__func x.Piqi.func in
  let _protobuf_package =
    Piqirun.gen_optional_field 376215364 gen__string x.Piqi.protobuf_package in
  let _typedef =
    Piqirun.gen_repeated_field 416823115 gen__typedef x.Piqi.typedef
  in
    Piqirun.gen_record code
      [ _modname; _file; _protobuf_custom; _import; _custom_field; _func;
        _protobuf_package; _typedef ]
and gen__import code x =
  let _modname =
    Piqirun.gen_required_field 13841580 gen__word x.Import.modname in
  let _name = Piqirun.gen_optional_field 150958667 gen__name x.Import.name
  in Piqirun.gen_record code [ _modname; _name ]
and gen__any code x =
  let _xml = Piqirun.gen_optional_field 5991895 gen__string x.Any.xml in
  let _protobuf =
    Piqirun.gen_optional_field 6461771 gen__binary x.Any.protobuf in
  let _json = Piqirun.gen_optional_field 107495976 gen__string x.Any.json in
  let _typename =
    Piqirun.gen_optional_field 218690234 gen__string x.Any.typename
  in Piqirun.gen_record code [ _xml; _protobuf; _json; _typename ]
and gen__func code x =
  let _name = Piqirun.gen_required_field 150958667 gen__name x.Func.name in
  let _output =
    Piqirun.gen_optional_field 209784577 gen__typename x.Func.output in
  let _error =
    Piqirun.gen_optional_field 321506248 gen__typename x.Func.error in
  let _input =
    Piqirun.gen_optional_field 505267210 gen__typename x.Func.input
  in Piqirun.gen_record code [ _name; _output; _error; _input ]
and gen__piqi_bundle code x =
  let _piqi = Piqirun.gen_repeated_field 1 gen__piqi x.Piqi_bundle.piqi
  in Piqirun.gen_record code [ _piqi ]
  
let gen_piq_format x = gen__piq_format (-1) x
  
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
  
let gen_protobuf_int64 x = gen__protobuf_int64 (-1) x
  
let gen_protobuf_wire_type x = gen__protobuf_wire_type (-1) x
  
let gen_bool x = gen__bool (-1) x
  
let gen_string x = gen__string (-1) x
  
let gen_binary x = gen__binary (-1) x
  
let gen_piqi_any x = gen__piqi_any (-1) x
  
let gen_int x = gen__int (-1) x
  
let gen_uint x = gen__uint (-1) x
  
let gen_int32 x = gen__int32 (-1) x
  
let gen_uint32 x = gen__uint32 (-1) x
  
let gen_int64 x = gen__int64 (-1) x
  
let gen_uint64 x = gen__uint64 (-1) x
  
let gen_float64 x = gen__float64 (-1) x
  
let gen_float32 x = gen__float32 (-1) x
  
let gen_int32_fixed x = gen__int32_fixed (-1) x
  
let gen_uint32_fixed x = gen__uint32_fixed (-1) x
  
let gen_int64_fixed x = gen__int64_fixed (-1) x
  
let gen_uint64_fixed x = gen__uint64_fixed (-1) x
  
let gen_float x = gen__float (-1) x
  
let gen_word x = gen__word (-1) x
  
let gen_name x = gen__name (-1) x
  
let gen_typedef x = gen__typedef (-1) x
  
let gen_piqi_type x = gen__piqi_type (-1) x
  
let gen_typename x = gen__typename (-1) x
  
let gen_record x = gen__record (-1) x
  
let gen_field x = gen__field (-1) x
  
let gen_field_mode x = gen__field_mode (-1) x
  
let gen_variant x = gen__variant (-1) x
  
let gen_option x = gen__option (-1) x
  
let gen_enum x = gen__enum (-1) x
  
let gen_alias x = gen__alias (-1) x
  
let gen_piqi_list x = gen__piqi_list (-1) x
  
let gen_piqi x = gen__piqi (-1) x
  
let gen_import x = gen__import (-1) x
  
let gen_any x = gen__any (-1) x
  
let gen_func x = gen__func (-1) x
  
let gen_piqi_bundle x = gen__piqi_bundle (-1) x
  
let rec default_piq_format () = `word
and default_protobuf_int32 () = default_int32 ()
and default_protobuf_int64 () = default_int64 ()
and default_protobuf_wire_type () = `varint
and default_bool () = false
and default_string () = ""
and default_binary () = ""
and default_piqi_any () = default_any ()
and default_int () = 0
and default_uint () = 0
and default_int32 () = 0l
and default_uint32 () = 0l
and default_int64 () = 0L
and default_uint64 () = 0L
and default_float64 () = 0.0
and default_float32 () = 0.0
and default_int32_fixed () = default_int32 ()
and default_uint32_fixed () = default_uint32 ()
and default_int64_fixed () = default_int64 ()
and default_uint64_fixed () = default_uint64 ()
and default_float () = default_float64 ()
and default_word () = default_string ()
and default_name () = default_word ()
and default_typedef () = `record (default_record ())
and default_piqi_type () = `int
and default_typename () = default_name ()
and default_record () =
  {
    Record.field = [];
    Record.protobuf_name = None;
    Record.protobuf_custom = [];
    Record.name = default_name ();
    Record.piq_positional = None;
    Record.json_name = None;
  }
and default_field () =
  {
    Field.code = None;
    Field.deprecated = false;
    Field.protobuf_name = None;
    Field.mode =
      parse_field_mode (Piqirun.parse_default "\b\223\162\138\147\001");
    Field.name = None;
    Field.protobuf_packed = false;
    Field.piq_positional = None;
    Field.json_omit_missing = None;
    Field.getopt_letter = None;
    Field.typename = None;
    Field.piq_format = None;
    Field.piq_alias = None;
    Field.getopt_doc = None;
    Field.default = None;
    Field.json_name = None;
  }
and default_field_mode () = `required
and default_variant () =
  {
    Variant.protobuf_name = None;
    Variant.protobuf_custom = [];
    Variant.name = default_name ();
    Variant.option = [];
    Variant.json_name = None;
  }
and default_option () =
  {
    Option.code = None;
    Option.deprecated = false;
    Option.protobuf_name = None;
    Option.name = None;
    Option.getopt_letter = None;
    Option.typename = None;
    Option.piq_format = None;
    Option.piq_alias = None;
    Option.getopt_doc = None;
    Option.json_name = None;
  }
and default_enum () =
  {
    Enum.protobuf_name = None;
    Enum.protobuf_custom = [];
    Enum.name = default_name ();
    Enum.option = [];
    Enum.protobuf_prefix = None;
    Enum.json_name = None;
  }
and default_alias () =
  {
    Alias.protobuf_name = None;
    Alias.name = default_name ();
    Alias.protobuf_type = None;
    Alias.protobuf_wire_type = None;
    Alias.piqi_type = None;
    Alias.typename = None;
    Alias.piq_format = None;
    Alias.json_name = None;
  }
and default_piqi_list () =
  {
    Piqi_list.protobuf_name = None;
    Piqi_list.protobuf_custom = [];
    Piqi_list.name = default_name ();
    Piqi_list.protobuf_packed = false;
    Piqi_list.typename = default_typename ();
    Piqi_list.piq_format = None;
    Piqi_list.json_name = None;
  }
and default_piqi () =
  {
    Piqi.modname = None;
    Piqi.file = None;
    Piqi.protobuf_custom = [];
    Piqi.import = [];
    Piqi.custom_field = [];
    Piqi.func = [];
    Piqi.protobuf_package = None;
    Piqi.typedef = [];
  }
and default_import () =
  { Import.modname = default_word (); Import.name = None; }
and default_any () =
  {
    Any.xml = None;
    Any.protobuf = None;
    Any.json = None;
    Any.typename = None;
  }
and default_func () =
  {
    Func.name = default_name ();
    Func.output = None;
    Func.error = None;
    Func.input = None;
  }
and default_piqi_bundle () = { Piqi_bundle.piqi = []; }
  
include Piqi_piqi
  

