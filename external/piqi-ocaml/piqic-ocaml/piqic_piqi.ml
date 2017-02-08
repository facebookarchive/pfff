module rec Piqic_piqi:
  sig
    type uint = int
    type uint32 = int32
    type uint64 = int64
    type float64 = float
    type float32 = float
    type protobuf_int32 = int32
    type protobuf_int64 = int64
    type binary = string
    type piqi_any = Piqic_piqi.any
    type int32_fixed = int32
    type uint32_fixed = Piqic_piqi.uint32
    type int64_fixed = int64
    type uint64_fixed = Piqic_piqi.uint64
    type float = Piqic_piqi.float64
    type word = string
    type name = Piqic_piqi.word
    type typename = Piqic_piqi.name
    type piq_format =
      [
        | `word
        | `text
      ]
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
        | `record of Piqic_piqi.record
        | `variant of Piqic_piqi.variant
        | `enum of Piqic_piqi.enum
        | `alias of Piqic_piqi.alias
        | `list of Piqic_piqi.piqi_list
      ]
    type piqi_type =
      [
        | `int
        | `float
        | `bool
        | `string
        | `binary
        | `any
      ]
    type field_mode =
      [
        | `required
        | `optional
        | `repeated
      ]
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
  end = Piqic_piqi
and Record:
  sig
    type t = {
      mutable name: Piqic_piqi.name;
      mutable field: Piqic_piqi.field list;
      mutable piq_positional: bool option;
      mutable protobuf_name: string option;
      mutable protobuf_custom: string list;
      mutable json_name: string option;
      mutable ocaml_name: string option;
    }
  end = Record
and Field:
  sig
    type t = {
      mutable name: Piqic_piqi.name option;
      mutable typename: Piqic_piqi.typename option;
      mutable mode: Piqic_piqi.field_mode;
      mutable default: Piqic_piqi.piqi_any option;
      mutable deprecated: bool;
      mutable piq_format: Piqic_piqi.piq_format option;
      mutable piq_positional: bool option;
      mutable piq_alias: Piqic_piqi.name option;
      mutable protobuf_name: string option;
      mutable code: int32 option;
      mutable protobuf_packed: bool;
      mutable json_name: string option;
      mutable json_omit_missing: bool option;
      mutable getopt_letter: Piqic_piqi.word option;
      mutable getopt_doc: string option;
      mutable ocaml_name: string option;
      mutable ocaml_array: bool;
      mutable ocaml_optional: bool;
    }
  end = Field
and Variant:
  sig
    type t = {
      mutable name: Piqic_piqi.name;
      mutable option: Piqic_piqi.option list;
      mutable protobuf_name: string option;
      mutable protobuf_custom: string list;
      mutable json_name: string option;
      mutable ocaml_name: string option;
    }
  end = Variant
and Option:
  sig
    type t = {
      mutable name: Piqic_piqi.name option;
      mutable typename: Piqic_piqi.typename option;
      mutable deprecated: bool;
      mutable piq_format: Piqic_piqi.piq_format option;
      mutable piq_alias: Piqic_piqi.name option;
      mutable protobuf_name: string option;
      mutable code: int32 option;
      mutable json_name: string option;
      mutable getopt_letter: Piqic_piqi.word option;
      mutable getopt_doc: string option;
      mutable ocaml_name: string option;
    }
  end = Option
and Enum:
  sig
    type t = {
      mutable name: Piqic_piqi.name;
      mutable option: Piqic_piqi.option list;
      mutable protobuf_name: string option;
      mutable protobuf_custom: string list;
      mutable protobuf_prefix: string option;
      mutable json_name: string option;
      mutable ocaml_name: string option;
    }
  end = Enum
and Alias:
  sig
    type t = {
      mutable name: Piqic_piqi.name;
      mutable typename: Piqic_piqi.typename option;
      mutable piqi_type: Piqic_piqi.piqi_type option;
      mutable piq_format: Piqic_piqi.piq_format option;
      mutable protobuf_name: string option;
      mutable protobuf_type: string option;
      mutable protobuf_wire_type: Piqic_piqi.protobuf_wire_type option;
      mutable json_name: string option;
      mutable ocaml_name: string option;
      mutable ocaml_type: string option;
    }
  end = Alias
and Piqi_list:
  sig
    type t = {
      mutable name: Piqic_piqi.name;
      mutable typename: Piqic_piqi.typename;
      mutable piq_format: Piqic_piqi.piq_format option;
      mutable protobuf_name: string option;
      mutable protobuf_custom: string list;
      mutable protobuf_packed: bool;
      mutable json_name: string option;
      mutable ocaml_name: string option;
      mutable ocaml_array: bool;
    }
  end = Piqi_list
and Piqi:
  sig
    type t = {
      mutable modname: Piqic_piqi.word option;
      mutable typedef: Piqic_piqi.typedef list;
      mutable import: Piqic_piqi.import list;
      mutable func: Piqic_piqi.func list;
      mutable custom_field: Piqic_piqi.word list;
      mutable protobuf_custom: string list;
      mutable protobuf_package: string option;
      mutable file: string option;
      mutable ocaml_module: string option;
    }
  end = Piqi
and Import:
  sig
    type t = {
      mutable modname: Piqic_piqi.word;
      mutable name: Piqic_piqi.name option;
      mutable ocaml_name: string option;
    }
  end = Import
and Any:
  sig
    type t = {
      mutable typename: string option;
      mutable protobuf: Piqic_piqi.binary option;
      mutable json: string option;
      mutable xml: string option;
    }
  end = Any
and Func:
  sig
    type t = {
      mutable name: Piqic_piqi.name;
      mutable input: Piqic_piqi.typename option;
      mutable output: Piqic_piqi.typename option;
      mutable error: Piqic_piqi.typename option;
      mutable ocaml_name: string option;
    }
  end = Func
and Piqi_bundle:
  sig
    type t = {
      mutable piqi: Piqic_piqi.piqi list;
    }
  end = Piqi_bundle


let rec parse_piq_format x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 251462090 when x = Piqirun.Varint 1 -> `word
    | 217697453 when x = Piqirun.Varint 1 -> `text
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
  let code, x = Piqirun.parse_variant x in
  match code with
    | 502036113 ->
        let res = parse_record x in
        `record res
    | 484589701 ->
        let res = parse_variant x in
        `variant res
    | 51800833 ->
        let res = parse_enum x in
        `enum res
    | 26300816 ->
        let res = parse_alias x in
        `alias res
    | 129178718 ->
        let res = parse_piqi_list x in
        `list res
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
  let _field, x = Piqirun.parse_repeated_field 9671866 parse_field x in
  let _protobuf_name, x = Piqirun.parse_optional_field 90072013 parse_string x in
  let _protobuf_custom, x = Piqirun.parse_repeated_field 112352691 parse_string x in
  let _name, x = Piqirun.parse_required_field 150958667 parse_name x in
  let _piq_positional, x = Piqirun.parse_optional_field 197354217 parse_bool x in
  let _ocaml_name, x = Piqirun.parse_optional_field 351856652 parse_string x in
  let _json_name, x = Piqirun.parse_optional_field 515275216 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Record.field = _field;
    Record.protobuf_name = _protobuf_name;
    Record.protobuf_custom = _protobuf_custom;
    Record.name = _name;
    Record.piq_positional = _piq_positional;
    Record.ocaml_name = _ocaml_name;
    Record.json_name = _json_name;
  }

and parse_field x =
  let x = Piqirun.parse_record x in
  let _code, x = Piqirun.parse_optional_field 29667629 parse_int32 x in
  let _deprecated, x =  Piqirun.parse_flag 69402483 x in
  let _protobuf_name, x = Piqirun.parse_optional_field 90072013 parse_string x in
  let _mode, x = Piqirun.parse_required_field 140563299 parse_field_mode x ~default:"\b\223\162\138\147\001" in
  let _name, x = Piqirun.parse_optional_field 150958667 parse_name x in
  let _protobuf_packed, x =  Piqirun.parse_flag 179842426 x in
  let _piq_positional, x = Piqirun.parse_optional_field 197354217 parse_bool x in
  let _json_omit_missing, x = Piqirun.parse_optional_field 201807079 parse_bool x in
  let _getopt_letter, x = Piqirun.parse_optional_field 215188758 parse_word x in
  let _typename, x = Piqirun.parse_optional_field 218690234 parse_typename x in
  let _piq_format, x = Piqirun.parse_optional_field 296833484 parse_piq_format x in
  let _ocaml_array, x =  Piqirun.parse_flag 333250744 x in
  let _ocaml_name, x = Piqirun.parse_optional_field 351856652 parse_string x in
  let _piq_alias, x = Piqirun.parse_optional_field 434682011 parse_name x in
  let _getopt_doc, x = Piqirun.parse_optional_field 442330184 parse_string x in
  let _default, x = Piqirun.parse_optional_field 465819841 parse_piqi_any x in
  let _ocaml_optional, x =  Piqirun.parse_flag 488413665 x in
  let _json_name, x = Piqirun.parse_optional_field 515275216 parse_string x in
  Piqirun.check_unparsed_fields x;
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
    Field.ocaml_array = _ocaml_array;
    Field.ocaml_name = _ocaml_name;
    Field.piq_alias = _piq_alias;
    Field.getopt_doc = _getopt_doc;
    Field.default = _default;
    Field.ocaml_optional = _ocaml_optional;
    Field.json_name = _json_name;
  }

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
  let _protobuf_name, x = Piqirun.parse_optional_field 90072013 parse_string x in
  let _protobuf_custom, x = Piqirun.parse_repeated_field 112352691 parse_string x in
  let _name, x = Piqirun.parse_required_field 150958667 parse_name x in
  let _option, x = Piqirun.parse_repeated_field 192598901 parse_option x in
  let _ocaml_name, x = Piqirun.parse_optional_field 351856652 parse_string x in
  let _json_name, x = Piqirun.parse_optional_field 515275216 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Variant.protobuf_name = _protobuf_name;
    Variant.protobuf_custom = _protobuf_custom;
    Variant.name = _name;
    Variant.option = _option;
    Variant.ocaml_name = _ocaml_name;
    Variant.json_name = _json_name;
  }

and parse_option x =
  let x = Piqirun.parse_record x in
  let _code, x = Piqirun.parse_optional_field 29667629 parse_int32 x in
  let _deprecated, x =  Piqirun.parse_flag 69402483 x in
  let _protobuf_name, x = Piqirun.parse_optional_field 90072013 parse_string x in
  let _name, x = Piqirun.parse_optional_field 150958667 parse_name x in
  let _getopt_letter, x = Piqirun.parse_optional_field 215188758 parse_word x in
  let _typename, x = Piqirun.parse_optional_field 218690234 parse_typename x in
  let _piq_format, x = Piqirun.parse_optional_field 296833484 parse_piq_format x in
  let _ocaml_name, x = Piqirun.parse_optional_field 351856652 parse_string x in
  let _piq_alias, x = Piqirun.parse_optional_field 434682011 parse_name x in
  let _getopt_doc, x = Piqirun.parse_optional_field 442330184 parse_string x in
  let _json_name, x = Piqirun.parse_optional_field 515275216 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Option.code = _code;
    Option.deprecated = _deprecated;
    Option.protobuf_name = _protobuf_name;
    Option.name = _name;
    Option.getopt_letter = _getopt_letter;
    Option.typename = _typename;
    Option.piq_format = _piq_format;
    Option.ocaml_name = _ocaml_name;
    Option.piq_alias = _piq_alias;
    Option.getopt_doc = _getopt_doc;
    Option.json_name = _json_name;
  }

and parse_enum x =
  let x = Piqirun.parse_record x in
  let _protobuf_name, x = Piqirun.parse_optional_field 90072013 parse_string x in
  let _protobuf_custom, x = Piqirun.parse_repeated_field 112352691 parse_string x in
  let _name, x = Piqirun.parse_required_field 150958667 parse_name x in
  let _option, x = Piqirun.parse_repeated_field 192598901 parse_option x in
  let _ocaml_name, x = Piqirun.parse_optional_field 351856652 parse_string x in
  let _protobuf_prefix, x = Piqirun.parse_optional_field 366391188 parse_string x in
  let _json_name, x = Piqirun.parse_optional_field 515275216 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Enum.protobuf_name = _protobuf_name;
    Enum.protobuf_custom = _protobuf_custom;
    Enum.name = _name;
    Enum.option = _option;
    Enum.ocaml_name = _ocaml_name;
    Enum.protobuf_prefix = _protobuf_prefix;
    Enum.json_name = _json_name;
  }

and parse_alias x =
  let x = Piqirun.parse_record x in
  let _protobuf_name, x = Piqirun.parse_optional_field 90072013 parse_string x in
  let _name, x = Piqirun.parse_required_field 150958667 parse_name x in
  let _protobuf_type, x = Piqirun.parse_optional_field 157803580 parse_string x in
  let _protobuf_wire_type, x = Piqirun.parse_optional_field 198202944 parse_protobuf_wire_type x in
  let _piqi_type, x = Piqirun.parse_optional_field 198318774 parse_piqi_type x in
  let _typename, x = Piqirun.parse_optional_field 218690234 parse_typename x in
  let _piq_format, x = Piqirun.parse_optional_field 296833484 parse_piq_format x in
  let _ocaml_name, x = Piqirun.parse_optional_field 351856652 parse_string x in
  let _ocaml_type, x = Piqirun.parse_optional_field 419588219 parse_string x in
  let _json_name, x = Piqirun.parse_optional_field 515275216 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Alias.protobuf_name = _protobuf_name;
    Alias.name = _name;
    Alias.protobuf_type = _protobuf_type;
    Alias.protobuf_wire_type = _protobuf_wire_type;
    Alias.piqi_type = _piqi_type;
    Alias.typename = _typename;
    Alias.piq_format = _piq_format;
    Alias.ocaml_name = _ocaml_name;
    Alias.ocaml_type = _ocaml_type;
    Alias.json_name = _json_name;
  }

and parse_piqi_list x =
  let x = Piqirun.parse_record x in
  let _protobuf_name, x = Piqirun.parse_optional_field 90072013 parse_string x in
  let _protobuf_custom, x = Piqirun.parse_repeated_field 112352691 parse_string x in
  let _name, x = Piqirun.parse_required_field 150958667 parse_name x in
  let _protobuf_packed, x =  Piqirun.parse_flag 179842426 x in
  let _typename, x = Piqirun.parse_required_field 218690234 parse_typename x in
  let _piq_format, x = Piqirun.parse_optional_field 296833484 parse_piq_format x in
  let _ocaml_array, x =  Piqirun.parse_flag 333250744 x in
  let _ocaml_name, x = Piqirun.parse_optional_field 351856652 parse_string x in
  let _json_name, x = Piqirun.parse_optional_field 515275216 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Piqi_list.protobuf_name = _protobuf_name;
    Piqi_list.protobuf_custom = _protobuf_custom;
    Piqi_list.name = _name;
    Piqi_list.protobuf_packed = _protobuf_packed;
    Piqi_list.typename = _typename;
    Piqi_list.piq_format = _piq_format;
    Piqi_list.ocaml_array = _ocaml_array;
    Piqi_list.ocaml_name = _ocaml_name;
    Piqi_list.json_name = _json_name;
  }

and parse_piqi x =
  let x = Piqirun.parse_record x in
  let _modname, x = Piqirun.parse_optional_field 13841580 parse_word x in
  let _file, x = Piqirun.parse_optional_field 62639740 parse_string x in
  let _protobuf_custom, x = Piqirun.parse_repeated_field 112352691 parse_string x in
  let _import, x = Piqirun.parse_repeated_field 142778725 parse_import x in
  let _custom_field, x = Piqirun.parse_repeated_field 162247646 parse_word x in
  let _func, x = Piqirun.parse_repeated_field 340962072 parse_func x in
  let _ocaml_module, x = Piqirun.parse_optional_field 375807149 parse_string x in
  let _protobuf_package, x = Piqirun.parse_optional_field 376215364 parse_string x in
  let _typedef, x = Piqirun.parse_repeated_field 416823115 parse_typedef x in
  Piqirun.check_unparsed_fields x;
  {
    Piqi.modname = _modname;
    Piqi.file = _file;
    Piqi.protobuf_custom = _protobuf_custom;
    Piqi.import = _import;
    Piqi.custom_field = _custom_field;
    Piqi.func = _func;
    Piqi.ocaml_module = _ocaml_module;
    Piqi.protobuf_package = _protobuf_package;
    Piqi.typedef = _typedef;
  }

and parse_import x =
  let x = Piqirun.parse_record x in
  let _modname, x = Piqirun.parse_required_field 13841580 parse_word x in
  let _name, x = Piqirun.parse_optional_field 150958667 parse_name x in
  let _ocaml_name, x = Piqirun.parse_optional_field 351856652 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Import.modname = _modname;
    Import.name = _name;
    Import.ocaml_name = _ocaml_name;
  }

and parse_any x =
  let x = Piqirun.parse_record x in
  let _xml, x = Piqirun.parse_optional_field 5991895 parse_string x in
  let _protobuf, x = Piqirun.parse_optional_field 6461771 parse_binary x in
  let _json, x = Piqirun.parse_optional_field 107495976 parse_string x in
  let _typename, x = Piqirun.parse_optional_field 218690234 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Any.xml = _xml;
    Any.protobuf = _protobuf;
    Any.json = _json;
    Any.typename = _typename;
  }

and parse_func x =
  let x = Piqirun.parse_record x in
  let _name, x = Piqirun.parse_required_field 150958667 parse_name x in
  let _output, x = Piqirun.parse_optional_field 209784577 parse_typename x in
  let _error, x = Piqirun.parse_optional_field 321506248 parse_typename x in
  let _ocaml_name, x = Piqirun.parse_optional_field 351856652 parse_string x in
  let _input, x = Piqirun.parse_optional_field 505267210 parse_typename x in
  Piqirun.check_unparsed_fields x;
  {
    Func.name = _name;
    Func.output = _output;
    Func.error = _error;
    Func.ocaml_name = _ocaml_name;
    Func.input = _input;
  }

and parse_piqi_bundle x =
  let x = Piqirun.parse_record x in
  let _piqi, x = Piqirun.parse_repeated_field 1 parse_piqi x in
  Piqirun.check_unparsed_fields x;
  {
    Piqi_bundle.piqi = _piqi;
  }


let rec gen__piq_format code (x:Piqic_piqi.piq_format) =
  Piqirun.gen_record code [(match x with
    | `word -> Piqirun.gen_bool_field 251462090 true
    | `text -> Piqirun.gen_bool_field 217697453 true
  )]

and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x

and gen__protobuf_int64 code x = Piqirun.int64_to_signed_varint code x
and packed_gen__protobuf_int64 x = Piqirun.int64_to_packed_signed_varint x

and gen__protobuf_wire_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `varint -> 329594984l
    | `zigzag_varint -> 99211597l
    | `fixed32 -> 136997651l
    | `fixed64 -> 136998322l
    | `signed_varint -> 441915897l
    | `signed_fixed32 -> 488499298l
    | `signed_fixed64 -> 488499969l
    | `block -> 352089421l
  )
and packed_gen__protobuf_wire_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `varint -> 329594984l
    | `zigzag_varint -> 99211597l
    | `fixed32 -> 136997651l
    | `fixed64 -> 136998322l
    | `signed_varint -> 441915897l
    | `signed_fixed32 -> 488499298l
    | `signed_fixed64 -> 488499969l
    | `block -> 352089421l
  )

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

and gen__typedef code (x:Piqic_piqi.typedef) =
  Piqirun.gen_record code [(match x with
    | `record x -> gen__record 502036113 x
    | `variant x -> gen__variant 484589701 x
    | `enum x -> gen__enum 51800833 x
    | `alias x -> gen__alias 26300816 x
    | `list x -> gen__piqi_list 129178718 x
  )]

and gen__piqi_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `int -> 5246191l
    | `float -> 43435420l
    | `bool -> 18580522l
    | `string -> 288368849l
    | `binary -> 218872833l
    | `any -> 4848364l
  )
and packed_gen__piqi_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `int -> 5246191l
    | `float -> 43435420l
    | `bool -> 18580522l
    | `string -> 288368849l
    | `binary -> 218872833l
    | `any -> 4848364l
  )

and gen__typename code x = gen__name code x

and gen__record code x =
  let _field = Piqirun.gen_repeated_field 9671866 gen__field x.Record.field in
  let _protobuf_name = Piqirun.gen_optional_field 90072013 gen__string x.Record.protobuf_name in
  let _protobuf_custom = Piqirun.gen_repeated_field 112352691 gen__string x.Record.protobuf_custom in
  let _name = Piqirun.gen_required_field 150958667 gen__name x.Record.name in
  let _piq_positional = Piqirun.gen_optional_field 197354217 gen__bool x.Record.piq_positional in
  let _ocaml_name = Piqirun.gen_optional_field 351856652 gen__string x.Record.ocaml_name in
  let _json_name = Piqirun.gen_optional_field 515275216 gen__string x.Record.json_name in
  Piqirun.gen_record code (_field :: _protobuf_name :: _protobuf_custom :: _name :: _piq_positional :: _ocaml_name :: _json_name :: [])

and gen__field code x =
  let _code = Piqirun.gen_optional_field 29667629 gen__int32 x.Field.code in
  let _deprecated =  Piqirun.gen_flag 69402483 x.Field.deprecated in
  let _protobuf_name = Piqirun.gen_optional_field 90072013 gen__string x.Field.protobuf_name in
  let _mode = Piqirun.gen_required_field 140563299 gen__field_mode x.Field.mode in
  let _name = Piqirun.gen_optional_field 150958667 gen__name x.Field.name in
  let _protobuf_packed =  Piqirun.gen_flag 179842426 x.Field.protobuf_packed in
  let _piq_positional = Piqirun.gen_optional_field 197354217 gen__bool x.Field.piq_positional in
  let _json_omit_missing = Piqirun.gen_optional_field 201807079 gen__bool x.Field.json_omit_missing in
  let _getopt_letter = Piqirun.gen_optional_field 215188758 gen__word x.Field.getopt_letter in
  let _typename = Piqirun.gen_optional_field 218690234 gen__typename x.Field.typename in
  let _piq_format = Piqirun.gen_optional_field 296833484 gen__piq_format x.Field.piq_format in
  let _ocaml_array =  Piqirun.gen_flag 333250744 x.Field.ocaml_array in
  let _ocaml_name = Piqirun.gen_optional_field 351856652 gen__string x.Field.ocaml_name in
  let _piq_alias = Piqirun.gen_optional_field 434682011 gen__name x.Field.piq_alias in
  let _getopt_doc = Piqirun.gen_optional_field 442330184 gen__string x.Field.getopt_doc in
  let _default = Piqirun.gen_optional_field 465819841 gen__piqi_any x.Field.default in
  let _ocaml_optional =  Piqirun.gen_flag 488413665 x.Field.ocaml_optional in
  let _json_name = Piqirun.gen_optional_field 515275216 gen__string x.Field.json_name in
  Piqirun.gen_record code (_code :: _deprecated :: _protobuf_name :: _mode :: _name :: _protobuf_packed :: _piq_positional :: _json_omit_missing :: _getopt_letter :: _typename :: _piq_format :: _ocaml_array :: _ocaml_name :: _piq_alias :: _getopt_doc :: _default :: _ocaml_optional :: _json_name :: [])

and gen__field_mode code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `required -> 308449631l
    | `optional -> 510570400l
    | `repeated -> 274054266l
  )
and packed_gen__field_mode x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `required -> 308449631l
    | `optional -> 510570400l
    | `repeated -> 274054266l
  )

and gen__variant code x =
  let _protobuf_name = Piqirun.gen_optional_field 90072013 gen__string x.Variant.protobuf_name in
  let _protobuf_custom = Piqirun.gen_repeated_field 112352691 gen__string x.Variant.protobuf_custom in
  let _name = Piqirun.gen_required_field 150958667 gen__name x.Variant.name in
  let _option = Piqirun.gen_repeated_field 192598901 gen__option x.Variant.option in
  let _ocaml_name = Piqirun.gen_optional_field 351856652 gen__string x.Variant.ocaml_name in
  let _json_name = Piqirun.gen_optional_field 515275216 gen__string x.Variant.json_name in
  Piqirun.gen_record code (_protobuf_name :: _protobuf_custom :: _name :: _option :: _ocaml_name :: _json_name :: [])

and gen__option code x =
  let _code = Piqirun.gen_optional_field 29667629 gen__int32 x.Option.code in
  let _deprecated =  Piqirun.gen_flag 69402483 x.Option.deprecated in
  let _protobuf_name = Piqirun.gen_optional_field 90072013 gen__string x.Option.protobuf_name in
  let _name = Piqirun.gen_optional_field 150958667 gen__name x.Option.name in
  let _getopt_letter = Piqirun.gen_optional_field 215188758 gen__word x.Option.getopt_letter in
  let _typename = Piqirun.gen_optional_field 218690234 gen__typename x.Option.typename in
  let _piq_format = Piqirun.gen_optional_field 296833484 gen__piq_format x.Option.piq_format in
  let _ocaml_name = Piqirun.gen_optional_field 351856652 gen__string x.Option.ocaml_name in
  let _piq_alias = Piqirun.gen_optional_field 434682011 gen__name x.Option.piq_alias in
  let _getopt_doc = Piqirun.gen_optional_field 442330184 gen__string x.Option.getopt_doc in
  let _json_name = Piqirun.gen_optional_field 515275216 gen__string x.Option.json_name in
  Piqirun.gen_record code (_code :: _deprecated :: _protobuf_name :: _name :: _getopt_letter :: _typename :: _piq_format :: _ocaml_name :: _piq_alias :: _getopt_doc :: _json_name :: [])

and gen__enum code x =
  let _protobuf_name = Piqirun.gen_optional_field 90072013 gen__string x.Enum.protobuf_name in
  let _protobuf_custom = Piqirun.gen_repeated_field 112352691 gen__string x.Enum.protobuf_custom in
  let _name = Piqirun.gen_required_field 150958667 gen__name x.Enum.name in
  let _option = Piqirun.gen_repeated_field 192598901 gen__option x.Enum.option in
  let _ocaml_name = Piqirun.gen_optional_field 351856652 gen__string x.Enum.ocaml_name in
  let _protobuf_prefix = Piqirun.gen_optional_field 366391188 gen__string x.Enum.protobuf_prefix in
  let _json_name = Piqirun.gen_optional_field 515275216 gen__string x.Enum.json_name in
  Piqirun.gen_record code (_protobuf_name :: _protobuf_custom :: _name :: _option :: _ocaml_name :: _protobuf_prefix :: _json_name :: [])

and gen__alias code x =
  let _protobuf_name = Piqirun.gen_optional_field 90072013 gen__string x.Alias.protobuf_name in
  let _name = Piqirun.gen_required_field 150958667 gen__name x.Alias.name in
  let _protobuf_type = Piqirun.gen_optional_field 157803580 gen__string x.Alias.protobuf_type in
  let _protobuf_wire_type = Piqirun.gen_optional_field 198202944 gen__protobuf_wire_type x.Alias.protobuf_wire_type in
  let _piqi_type = Piqirun.gen_optional_field 198318774 gen__piqi_type x.Alias.piqi_type in
  let _typename = Piqirun.gen_optional_field 218690234 gen__typename x.Alias.typename in
  let _piq_format = Piqirun.gen_optional_field 296833484 gen__piq_format x.Alias.piq_format in
  let _ocaml_name = Piqirun.gen_optional_field 351856652 gen__string x.Alias.ocaml_name in
  let _ocaml_type = Piqirun.gen_optional_field 419588219 gen__string x.Alias.ocaml_type in
  let _json_name = Piqirun.gen_optional_field 515275216 gen__string x.Alias.json_name in
  Piqirun.gen_record code (_protobuf_name :: _name :: _protobuf_type :: _protobuf_wire_type :: _piqi_type :: _typename :: _piq_format :: _ocaml_name :: _ocaml_type :: _json_name :: [])

and gen__piqi_list code x =
  let _protobuf_name = Piqirun.gen_optional_field 90072013 gen__string x.Piqi_list.protobuf_name in
  let _protobuf_custom = Piqirun.gen_repeated_field 112352691 gen__string x.Piqi_list.protobuf_custom in
  let _name = Piqirun.gen_required_field 150958667 gen__name x.Piqi_list.name in
  let _protobuf_packed =  Piqirun.gen_flag 179842426 x.Piqi_list.protobuf_packed in
  let _typename = Piqirun.gen_required_field 218690234 gen__typename x.Piqi_list.typename in
  let _piq_format = Piqirun.gen_optional_field 296833484 gen__piq_format x.Piqi_list.piq_format in
  let _ocaml_array =  Piqirun.gen_flag 333250744 x.Piqi_list.ocaml_array in
  let _ocaml_name = Piqirun.gen_optional_field 351856652 gen__string x.Piqi_list.ocaml_name in
  let _json_name = Piqirun.gen_optional_field 515275216 gen__string x.Piqi_list.json_name in
  Piqirun.gen_record code (_protobuf_name :: _protobuf_custom :: _name :: _protobuf_packed :: _typename :: _piq_format :: _ocaml_array :: _ocaml_name :: _json_name :: [])

and gen__piqi code x =
  let _modname = Piqirun.gen_optional_field 13841580 gen__word x.Piqi.modname in
  let _file = Piqirun.gen_optional_field 62639740 gen__string x.Piqi.file in
  let _protobuf_custom = Piqirun.gen_repeated_field 112352691 gen__string x.Piqi.protobuf_custom in
  let _import = Piqirun.gen_repeated_field 142778725 gen__import x.Piqi.import in
  let _custom_field = Piqirun.gen_repeated_field 162247646 gen__word x.Piqi.custom_field in
  let _func = Piqirun.gen_repeated_field 340962072 gen__func x.Piqi.func in
  let _ocaml_module = Piqirun.gen_optional_field 375807149 gen__string x.Piqi.ocaml_module in
  let _protobuf_package = Piqirun.gen_optional_field 376215364 gen__string x.Piqi.protobuf_package in
  let _typedef = Piqirun.gen_repeated_field 416823115 gen__typedef x.Piqi.typedef in
  Piqirun.gen_record code (_modname :: _file :: _protobuf_custom :: _import :: _custom_field :: _func :: _ocaml_module :: _protobuf_package :: _typedef :: [])

and gen__import code x =
  let _modname = Piqirun.gen_required_field 13841580 gen__word x.Import.modname in
  let _name = Piqirun.gen_optional_field 150958667 gen__name x.Import.name in
  let _ocaml_name = Piqirun.gen_optional_field 351856652 gen__string x.Import.ocaml_name in
  Piqirun.gen_record code (_modname :: _name :: _ocaml_name :: [])

and gen__any code x =
  let _xml = Piqirun.gen_optional_field 5991895 gen__string x.Any.xml in
  let _protobuf = Piqirun.gen_optional_field 6461771 gen__binary x.Any.protobuf in
  let _json = Piqirun.gen_optional_field 107495976 gen__string x.Any.json in
  let _typename = Piqirun.gen_optional_field 218690234 gen__string x.Any.typename in
  Piqirun.gen_record code (_xml :: _protobuf :: _json :: _typename :: [])

and gen__func code x =
  let _name = Piqirun.gen_required_field 150958667 gen__name x.Func.name in
  let _output = Piqirun.gen_optional_field 209784577 gen__typename x.Func.output in
  let _error = Piqirun.gen_optional_field 321506248 gen__typename x.Func.error in
  let _ocaml_name = Piqirun.gen_optional_field 351856652 gen__string x.Func.ocaml_name in
  let _input = Piqirun.gen_optional_field 505267210 gen__typename x.Func.input in
  Piqirun.gen_record code (_name :: _output :: _error :: _ocaml_name :: _input :: [])

and gen__piqi_bundle code x =
  let _piqi = Piqirun.gen_repeated_field 1 gen__piqi x.Piqi_bundle.piqi in
  Piqirun.gen_record code (_piqi :: [])


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
    Record.ocaml_name = None;
    Record.json_name = None;
  }
and default_field () =
  {
    Field.code = None;
    Field.deprecated = false;
    Field.protobuf_name = None;
    Field.mode = parse_field_mode (Piqirun.parse_default "\b\223\162\138\147\001");
    Field.name = None;
    Field.protobuf_packed = false;
    Field.piq_positional = None;
    Field.json_omit_missing = None;
    Field.getopt_letter = None;
    Field.typename = None;
    Field.piq_format = None;
    Field.ocaml_array = false;
    Field.ocaml_name = None;
    Field.piq_alias = None;
    Field.getopt_doc = None;
    Field.default = None;
    Field.ocaml_optional = false;
    Field.json_name = None;
  }
and default_field_mode () = `required
and default_variant () =
  {
    Variant.protobuf_name = None;
    Variant.protobuf_custom = [];
    Variant.name = default_name ();
    Variant.option = [];
    Variant.ocaml_name = None;
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
    Option.ocaml_name = None;
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
    Enum.ocaml_name = None;
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
    Alias.ocaml_name = None;
    Alias.ocaml_type = None;
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
    Piqi_list.ocaml_array = false;
    Piqi_list.ocaml_name = None;
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
    Piqi.ocaml_module = None;
    Piqi.protobuf_package = None;
    Piqi.typedef = [];
  }
and default_import () =
  {
    Import.modname = default_word ();
    Import.name = None;
    Import.ocaml_name = None;
  }
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
    Func.ocaml_name = None;
    Func.input = None;
  }
and default_piqi_bundle () =
  {
    Piqi_bundle.piqi = [];
  }


let piqi = "\226\202\2304\004piqi\226\231\249\238\001\tpiqi.piqi\234\202\203\153\011\nPiqic_piqi\218\244\134\182\012H\170\136\200\184\014B\218\164\238\191\004\npiq-format\170\183\218\222\005\019\232\146\150q\148\135\232\239\001\218\164\238\191\004\004word\170\183\218\222\005\019\232\146\150q\218\178\206\207\001\218\164\238\191\004\004text\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\014protobuf-int32\226\195\252\217\004\005int32\128\228\138\244\005\249\179\220\210\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int32\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\014protobuf-int64\226\195\252\217\004\005int64\128\228\138\244\005\249\179\220\210\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int64\218\244\134\182\012\149\002\138\176\205\197\001\142\002\218\164\238\191\004\018protobuf-wire-type\170\183\218\222\005\021\232\146\150q\208\225\169\186\002\218\164\238\191\004\006varint\170\183\218\222\005\027\232\146\150q\154\229\206^\218\164\238\191\004\rzigzag-varint\170\183\218\222\005\022\232\146\150q\166\172\211\130\001\218\164\238\191\004\007fixed32\170\183\218\222\005\022\232\146\150q\228\182\211\130\001\218\164\238\191\004\007fixed64\170\183\218\222\005\028\232\146\150q\242\231\184\165\003\218\164\238\191\004\rsigned-varint\170\183\218\222\005\029\232\146\150q\196\161\239\209\003\218\164\238\191\004\014signed-fixed32\170\183\218\222\005\029\232\146\150q\130\172\239\209\003\218\164\238\191\004\014signed-fixed64\170\183\218\222\005\020\232\146\150q\154\213\227\207\002\218\164\238\191\004\005block\218\244\134\182\012\024\130\153\170d\019\218\164\238\191\004\004bool\176\171\195\244\005\170\136\238\b\218\244\134\182\012\027\130\153\170d\022\218\164\238\191\004\006string\176\171\195\244\005\209\209\192\137\001\218\244\134\182\012\026\130\153\170d\021\218\164\238\191\004\006binary\176\171\195\244\005\129\248\174h\218\244\134\182\012\028\130\153\170d\023\218\164\238\191\004\bpiqi-any\176\171\195\244\005\236\245\167\002\218\244\134\182\0125\130\153\170d0\218\164\238\191\004\003int\226\195\252\217\004\006sint32\128\228\138\244\005\205\178\167/\176\171\195\244\005\239\153\192\002\218\135\205\192\012\003int\218\244\134\182\0127\130\153\170d2\218\164\238\191\004\004uint\226\195\252\217\004\006uint32\128\228\138\244\005\232\240\148\157\001\176\171\195\244\005\239\153\192\002\218\135\205\192\012\003int\218\244\134\182\0129\130\153\170d4\218\164\238\191\004\005int32\226\195\252\217\004\006sint32\128\228\138\244\005\205\178\167/\176\171\195\244\005\239\153\192\002\218\135\205\192\012\005int32\218\244\134\182\012;\130\153\170d6\218\164\238\191\004\006uint32\226\195\252\217\004\006uint32\128\228\138\244\005\232\240\148\157\001\176\171\195\244\005\239\153\192\002\218\135\205\192\012\005int32\218\244\134\182\0129\130\153\170d4\218\164\238\191\004\005int64\226\195\252\217\004\006sint64\128\228\138\244\005\205\178\167/\176\171\195\244\005\239\153\192\002\218\135\205\192\012\005int64\218\244\134\182\012;\130\153\170d6\218\164\238\191\004\006uint64\226\195\252\217\004\006uint64\128\228\138\244\005\232\240\148\157\001\176\171\195\244\005\239\153\192\002\218\135\205\192\012\005int64\218\244\134\182\012;\130\153\170d6\218\164\238\191\004\007float64\226\195\252\217\004\006double\128\228\138\244\005\178\219\169A\176\171\195\244\005\156\139\219\020\218\135\205\192\012\005float\218\244\134\182\012:\130\153\170d5\218\164\238\191\004\007float32\226\195\252\217\004\005float\128\228\138\244\005\147\214\169A\176\171\195\244\005\156\139\219\020\218\135\205\192\012\005float\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\011int32-fixed\226\195\252\217\004\bsfixed32\128\228\138\244\005\226\208\247\232\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int32\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\012uint32-fixed\226\195\252\217\004\007fixed32\128\228\138\244\005\147\214\169A\176\171\195\244\005\239\153\192\002\210\171\158\194\006\006uint32\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\011int64-fixed\226\195\252\217\004\bsfixed64\128\228\138\244\005\129\214\247\232\001\176\171\195\244\005\239\153\192\002\210\171\158\194\006\005int64\218\244\134\182\012B\130\153\170d=\218\164\238\191\004\012uint64-fixed\226\195\252\217\004\007fixed64\128\228\138\244\005\178\219\169A\176\171\195\244\005\239\153\192\002\210\171\158\194\006\006uint64\218\244\134\182\012&\130\153\170d!\218\164\238\191\004\005float\176\171\195\244\005\156\139\219\020\210\171\158\194\006\007float64\218\244\134\182\012'\130\153\170d\"\218\164\238\191\004\004word\210\171\158\194\006\006string\226\156\170\236\b\006\208\156\160\191\007\001\218\244\134\182\012\025\130\153\170d\020\218\164\238\191\004\004name\210\171\158\194\006\004word\218\244\134\182\012\176\001\170\136\200\184\014\169\001\234\188\204\215\002\012piqi_typedef\218\164\238\191\004\007typedef\170\183\218\222\005\021\232\146\150q\162\218\227\222\003\210\171\158\194\006\006record\170\183\218\222\005\022\232\146\150q\138\130\146\206\003\210\171\158\194\006\007variant\170\183\218\222\005\018\232\146\150q\130\172\1791\210\171\158\194\006\004enum\170\183\218\222\005\019\232\146\150q\160\198\138\025\210\171\158\194\006\005alias\170\183\218\222\005\028\232\146\150q\188\241\152{\210\171\158\194\006\004list\226\128\157\190\n\004list\218\244\134\182\012\187\001\138\176\205\197\001\180\001\218\164\238\191\004\tpiqi-type\170\183\218\222\005\017\232\146\150q\222\179\128\005\218\164\238\191\004\003int\170\183\218\222\005\019\232\146\150q\184\150\182)\218\164\238\191\004\005float\170\183\218\222\005\018\232\146\150q\212\144\220\017\218\164\238\191\004\004bool\170\183\218\222\005\021\232\146\150q\162\163\129\147\002\218\164\238\191\004\006string\170\183\218\222\005\021\232\146\150q\130\240\221\208\001\218\164\238\191\004\006binary\170\183\218\222\005\017\232\146\150q\216\235\207\004\218\164\238\191\004\003any\162\249\213\245\n\npiqi_type_\218\244\134\182\012'\130\153\170d\"\218\164\238\191\004\004type\210\171\158\194\006\004name\226\128\157\190\n\btypename\218\244\134\182\012\226\002\138\233\142\251\014\219\002\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\210\156\t\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\005field\210\203\242$1\232\146\150q\210\139\155\188\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014piq-positional\210\171\158\194\006\004bool\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\218\164\238\191\004\006record\218\244\134\182\012\132\007\138\233\142\251\014\253\006\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004type\210\203\242$S\232\146\150q\198\205\134\134\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004mode\210\171\158\194\006\nfield-mode\138\140\251\240\r \218\148\211\024\006\b\223\162\138\147\001\210\171\158\194\006\015piqi/field-mode\210\203\242$.\232\146\150q\130\227\158\188\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007default\210\171\158\194\006\bpiqi-any\210\203\242$\"\232\146\150q\230\253\151B\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeprecated\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\210\139\155\188\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014piq-positional\210\171\158\194\006\004bool\210\203\242$,\232\146\150q\182\226\197\158\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tpiq-alias\210\171\158\194\006\004name\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$'\232\146\150q\218\196\165\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004code\210\171\158\194\006\005int32\210\203\242$(\232\146\150q\244\181\193\171\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015protobuf-packed\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$4\232\146\150q\206\211\186\192\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017json-omit-missing\210\171\158\194\006\004bool\210\203\242$0\232\146\150q\172\148\156\205\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rgetopt-letter\210\171\158\194\006\004word\210\203\242$/\232\146\150q\144\177\235\165\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ngetopt-doc\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\210\203\242$$\232\146\150q\240\130\232\189\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011ocaml-array\210\203\242$'\232\146\150q\194\231\228\209\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014ocaml-optional\218\164\238\191\004\005field\218\244\134\182\012m\138\176\205\197\001g\218\164\238\191\004\nfield-mode\170\183\218\222\005\023\232\146\150q\190\197\148\166\002\218\164\238\191\004\brequired\170\183\218\222\005\023\232\146\150q\192\190\245\230\003\218\164\238\191\004\boptional\170\183\218\222\005\023\232\146\150q\244\241\173\133\002\218\164\238\191\004\brepeated\218\244\134\182\012\175\002\138\233\142\251\014\168\002\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$\031\232\146\150q\234\205\214\183\001\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\006option\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\218\164\238\191\004\007variant\218\244\134\182\012\137\004\138\233\142\251\014\130\004\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004type\210\203\242$\"\232\146\150q\230\253\151B\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeprecated\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$,\232\146\150q\182\226\197\158\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tpiq-alias\210\171\158\194\006\004name\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$'\232\146\150q\218\196\165\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004code\210\171\158\194\006\005int32\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$0\232\146\150q\172\148\156\205\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rgetopt-letter\210\171\158\194\006\004word\210\203\242$/\232\146\150q\144\177\235\165\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ngetopt-doc\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\218\164\238\191\004\006option\218\244\134\182\012\244\002\138\233\142\251\014\237\002\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$\031\232\146\150q\234\205\214\183\001\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\006option\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$4\232\146\150q\168\190\181\221\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015protobuf-prefix\210\171\158\194\006\006string\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\234\188\204\215\002\tpiqi_enum\218\164\238\191\004\004enum\218\244\134\182\012\221\003\138\233\142\251\014\214\003\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004type\210\203\242$\"\232\146\150q\236\234\144\189\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\tpiqi-type\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$2\232\146\150q\248\144\191\150\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-type\210\171\158\194\006\006string\210\203\242$+\232\146\150q\128\217\130\189\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\018protobuf-wire-type\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\246\161\147\144\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-type\210\171\158\194\006\006string\218\164\238\191\004\005alias\218\244\134\182\012\183\003\138\233\142\251\014\176\003\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$\029\232\146\150q\244\202\199\208\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004type\210\203\242$#\232\146\150q\152\199\138\155\002\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\npiq-format\210\203\242$1\232\146\150q\154\143\243U\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rprotobuf-name\210\171\158\194\006\006string\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$(\232\146\150q\244\181\193\171\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015protobuf-packed\210\203\242$.\232\146\150q\160\231\179\235\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tjson-name\210\171\158\194\006\006string\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\210\203\242$$\232\146\150q\240\130\232\189\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011ocaml-array\218\164\238\191\004\004list\226\128\157\190\n\tpiqi_list\218\244\134\182\012\195\003\138\233\142\251\014\188\003\210\203\242$5\232\146\150q\216\210\153\r\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006module\210\171\158\194\006\004word\226\128\157\190\n\007modname\210\203\242$ \232\146\150q\150\221\193\141\003\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\007typedef\210\203\242$\031\232\146\150q\202\133\149\136\001\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\006import\210\203\242$!\232\146\150q\176\172\149\197\002\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\bfunction\210\203\242$/\232\146\150q\188\207\221\154\001\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\012custom-field\210\171\158\194\006\004word\210\203\242$3\232\146\150q\230\246\146k\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015protobuf-custom\210\171\158\194\006\006string\210\203\242$5\232\146\150q\136\221\228\230\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016protobuf-package\210\171\158\194\006\006string\210\203\242$(\232\146\150q\248\185\222;\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004file\210\171\158\194\006\006string\210\203\242$1\232\146\150q\218\242\178\230\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012ocaml-module\210\171\158\194\006\006string\218\164\238\191\004\004piqi\218\244\134\182\012\163\001\138\233\142\251\014\156\001\210\203\242$5\232\146\150q\216\210\153\r\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006module\210\171\158\194\006\004word\226\128\157\190\n\007modname\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\160\223\186\243\001\210\171\158\194\006\004name\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\218\164\238\191\004\006import\218\244\134\182\012\214\001\138\233\142\251\014\207\001\210\203\242$7\232\146\150q\244\202\199\208\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004type\210\171\158\194\006\006string\226\128\157\190\n\btypename\210\203\242$,\232\146\150q\150\229\148\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bprotobuf\210\171\158\194\006\006binary\210\203\242$(\232\146\150q\208\136\194f\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004json\210\171\158\194\006\006string\210\203\242$'\232\146\150q\174\183\219\005\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003xml\210\171\158\194\006\006string\218\164\238\191\004\003any\218\244\134\182\012\253\001\138\233\142\251\014\246\001\210\203\242$\029\232\146\150q\150\201\251\143\001\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\004name\210\203\242$(\232\146\150q\148\144\238\225\003\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005input\210\171\158\194\006\004type\210\203\242$)\232\146\150q\130\188\136\200\001\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006output\210\171\158\194\006\004type\210\203\242$(\232\146\150q\144\175\206\178\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005error\210\171\158\194\006\004type\210\203\242$/\232\146\150q\152\160\199\207\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nocaml-name\210\171\158\194\006\006string\218\164\238\191\004\bfunction\226\128\157\190\n\004func\218\244\134\182\012D\138\233\142\251\014>\210\203\242$\025\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\210\171\158\194\006\004piqi\218\164\238\191\004\tpiqi-list\226\128\157\190\n\011piqi_bundle"
include Piqic_piqi
