module Piqirun = Piqi_piqirun
  
module rec Piqi_impl_piqi :
             sig
               type uint = int
               
               type uint32 = int32
               
               type uint64 = int64
               
               type float64 = float
               
               type float32 = float
               
               type protobuf_int32 = int32
               
               type protobuf_int64 = int64
               
               type binary = string
               
               type piqi_any = Piqi_impl_piqi.any
               
               type int32_fixed = int32
               
               type uint32_fixed = Piqi_impl_piqi.uint32
               
               type int64_fixed = int64
               
               type uint64_fixed = Piqi_impl_piqi.uint64
               
               type float = Piqi_impl_piqi.float64
               
               type piq_ast = Piq_ast.ast
               
               type word = string
               
               type name = Piqi_impl_piqi.word
               
               type typename = Piqi_impl_piqi.name
               
               type namespace =
                 [
                   | `piqi of Piqi_impl_piqi.piqi
                   | `import of Piqi_impl_piqi.import
                 ]
               
               type typedef =
                 [
                   | `record of Piqi_impl_piqi.record
                   | `variant of Piqi_impl_piqi.variant
                   | `enum of Piqi_impl_piqi.enum
                   | `alias of Piqi_impl_piqi.alias
                   | `list of Piqi_impl_piqi.piqi_list
                 ]
               
               type piqi_type =
                 [ | `int | `float | `bool | `string | `binary | `any
                 ]
               
               type piqtype = [ | typedef | piqi_type ]
               
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
               
               type field_mode = [ | `required | `optional | `repeated ]
               
               type function_param =
                 [
                   | `name of Piqi_impl_piqi.name
                   | `record of Piqi_impl_piqi.record
                   | `variant of Piqi_impl_piqi.variant
                   | `enum of Piqi_impl_piqi.enum
                   | `list of Piqi_impl_piqi.piqi_list
                   | `alias of Piqi_impl_piqi.alias
                 ]
               
               type extend_target =
                 [
                   | `typedef of Piqi_impl_piqi.name
                   | `name of Piqi_impl_piqi.name
                   | `field of Piqi_impl_piqi.name
                   | `option of Piqi_impl_piqi.name
                   | `import of Piqi_impl_piqi.name
                   | `func of Piqi_impl_piqi.name
                 ]
               
               type pib_typehint = Pib_typehint.t
               
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
               
               type includ = Includ.t
               
               type extend = Extend.t
               
             end = Piqi_impl_piqi
and
  Pib_typehint :
    sig
      type t =
        { mutable piqi_type : string;
          mutable typename : Piqi_impl_piqi.typename;
          mutable code : Piqi_impl_piqi.uint
        }
      
    end = Pib_typehint
and
  Record :
    sig
      type t =
        { mutable name : Piqi_impl_piqi.name option;
          mutable field : Piqi_impl_piqi.field list;
          mutable parent : Piqi_impl_piqi.namespace option;
          mutable wire_field : Piqi_impl_piqi.field list;
          mutable is_func_param : bool;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option;
          mutable piq_positional : bool option;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable json_name : string option;
          mutable proto_custom : string list;
          mutable proto_name : string option
        }
      
    end = Record
and
  Field :
    sig
      type t =
        { mutable name : Piqi_impl_piqi.name option;
          mutable typename : Piqi_impl_piqi.typename option;
          mutable mode : Piqi_impl_piqi.field_mode;
          mutable default : Piqi_impl_piqi.piqi_any option;
          mutable deprecated : bool;
          mutable piqtype : Piqi_impl_piqi.piqtype option;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option;
          mutable piq_format : Piqi_impl_piqi.piq_format option;
          mutable piq_positional : bool option;
          mutable piq_alias : Piqi_impl_piqi.name option;
          mutable protobuf_name : string option; mutable code : int32 option;
          mutable protobuf_packed : bool; mutable json_name : string option;
          mutable json_omit_missing : bool option;
          mutable getopt_letter : Piqi_impl_piqi.word option;
          mutable getopt_doc : string option;
          mutable proto_name : string option; mutable wire_packed : bool
        }
      
    end = Field
and
  Variant :
    sig
      type t =
        { mutable name : Piqi_impl_piqi.name option;
          mutable option : Piqi_impl_piqi.option list;
          mutable parent : Piqi_impl_piqi.namespace option;
          mutable is_func_param : bool;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable json_name : string option;
          mutable proto_custom : string list;
          mutable proto_name : string option
        }
      
    end = Variant
and
  Option :
    sig
      type t =
        { mutable name : Piqi_impl_piqi.name option;
          mutable typename : Piqi_impl_piqi.typename option;
          mutable deprecated : bool;
          mutable piqtype : Piqi_impl_piqi.piqtype option;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option;
          mutable piq_format : Piqi_impl_piqi.piq_format option;
          mutable piq_alias : Piqi_impl_piqi.name option;
          mutable protobuf_name : string option; mutable code : int32 option;
          mutable json_name : string option;
          mutable getopt_letter : Piqi_impl_piqi.word option;
          mutable getopt_doc : string option;
          mutable proto_name : string option
        }
      
    end = Option
and
  Enum :
    sig
      type t =
        { mutable name : Piqi_impl_piqi.name option;
          mutable option : Piqi_impl_piqi.option list;
          mutable parent : Piqi_impl_piqi.namespace option;
          mutable is_func_param : bool;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable protobuf_prefix : string option;
          mutable json_name : string option;
          mutable proto_custom : string list;
          mutable proto_name : string option
        }
      
    end = Enum
and
  Alias :
    sig
      type t =
        { mutable name : Piqi_impl_piqi.name option;
          mutable typename : Piqi_impl_piqi.typename option;
          mutable piqi_type : Piqi_impl_piqi.piqi_type option;
          mutable parent : Piqi_impl_piqi.namespace option;
          mutable is_func_param : bool;
          mutable piqtype : Piqi_impl_piqi.piqtype option;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option;
          mutable piq_format : Piqi_impl_piqi.piq_format option;
          mutable protobuf_name : string option;
          mutable protobuf_type : string option;
          mutable protobuf_wire_type :
          Piqi_impl_piqi.protobuf_wire_type option;
          mutable json_name : string option;
          mutable proto_name : string option
        }
      
    end = Alias
and
  Piqi_list :
    sig
      type t =
        { mutable name : Piqi_impl_piqi.name option;
          mutable typename : Piqi_impl_piqi.typename;
          mutable parent : Piqi_impl_piqi.namespace option;
          mutable is_func_param : bool;
          mutable piqtype : Piqi_impl_piqi.piqtype option;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option;
          mutable piq_format : Piqi_impl_piqi.piq_format option;
          mutable protobuf_name : string option;
          mutable protobuf_custom : string list;
          mutable protobuf_packed : bool; mutable json_name : string option;
          mutable proto_custom : string list;
          mutable proto_name : string option; mutable wire_packed : bool
        }
      
    end = Piqi_list
and
  Piqi :
    sig
      type t =
        { mutable modname : Piqi_impl_piqi.word option;
          mutable typedef : Piqi_impl_piqi.typedef list;
          mutable import : Piqi_impl_piqi.import list;
          mutable func : Piqi_impl_piqi.func list;
          mutable custom_field : Piqi_impl_piqi.word list;
          mutable extended_typedef : Piqi_impl_piqi.typedef list;
          mutable func_typedef : Piqi_impl_piqi.typedef list;
          mutable extended_func_typedef : Piqi_impl_piqi.typedef list;
          mutable resolved_typedef : Piqi_impl_piqi.typedef list;
          mutable imported_typedef : Piqi_impl_piqi.typedef list;
          mutable resolved_import : Piqi_impl_piqi.import list;
          mutable extended_import : Piqi_impl_piqi.import list;
          mutable resolved_func : Piqi_impl_piqi.func list;
          mutable extended_func : Piqi_impl_piqi.func list;
          mutable included_piqi : Piqi_impl_piqi.piqi list;
          mutable original_piqi : Piqi_impl_piqi.piqi option;
          mutable ast : Piqi_impl_piqi.piq_ast option;
          mutable is_embedded : bool option;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option;
          mutable protobuf_custom : string list;
          mutable protobuf_package : string option;
          mutable file : string option;
          mutable includ : Piqi_impl_piqi.includ list;
          mutable extend : Piqi_impl_piqi.extend list;
          mutable proto_custom : string list;
          mutable proto_package : string option
        }
      
    end = Piqi
and
  Import :
    sig
      type t =
        { mutable modname : Piqi_impl_piqi.word;
          mutable name : Piqi_impl_piqi.name option;
          mutable piqi : Piqi_impl_piqi.piqi option;
          mutable orig_modname : string option;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option
        }
      
    end = Import
and
  Any :
    sig
      type t =
        { mutable typename : string option;
          mutable protobuf : Piqi_impl_piqi.binary option;
          mutable json : string option; mutable xml : string option;
          mutable ref : int option
        }
      
    end = Any
and
  Func :
    sig
      type t =
        { mutable name : Piqi_impl_piqi.name;
          mutable input : Piqi_impl_piqi.function_param option;
          mutable output : Piqi_impl_piqi.function_param option;
          mutable error : Piqi_impl_piqi.function_param option;
          mutable resolved_input : Piqi_impl_piqi.typedef option;
          mutable resolved_output : Piqi_impl_piqi.typedef option;
          mutable resolved_error : Piqi_impl_piqi.typedef option;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option
        }
      
    end = Func
and
  Piqi_bundle :
    sig type t = { mutable piqi : Piqi_impl_piqi.piqi list }
         end =
    Piqi_bundle
and
  Includ :
    sig
      type t =
        { mutable modname : Piqi_impl_piqi.word;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option
        }
      
    end = Includ
and
  Extend :
    sig
      type t =
        { mutable what : Piqi_impl_piqi.extend_target list;
          mutable override : bool;
          mutable piqi_with : Piqi_impl_piqi.piqi_any list;
          mutable quote : Piqi_impl_piqi.piqi_any list;
          mutable unparsed_piq_ast : Piqi_impl_piqi.uint option
        }
      
    end = Extend
  
let next_count = Piqloc.next_icount
  
let curr_count () = !Piqloc.icount
  
let refer ref obj =
  if not (Obj.is_int (Obj.repr obj)) then Piqloc.addrefret ref obj else obj
  
let incr_count_if_true (((obj, _) as res)) =
  (if obj then ignore (next_count ()) else (); res)
  
let rec parse_namespace x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 173536529 ->
           let res = let count = curr_count () in refer count (parse_piqi x)
           in `piqi res
       | 142778725 ->
           let res =
             let count = curr_count () in refer count (parse_import x)
           in `import res
       | _ -> Piqirun.error_variant x code)
and parse_piqtype x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 416823115 -> (parse_typedef x :> Piqi_impl_piqi.piqtype)
       | 198318774 -> (parse_piqi_type x :> Piqi_impl_piqi.piqtype)
       | _ -> Piqirun.error_variant x code)
and parse_piq_ast x = Piq_ast.ast_of_bool (parse_bool x)
and packed_parse_piq_ast x = Piq_ast.ast_of_bool (packed_parse_bool x)
and parse_pib_typehint x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_piqi_type, x) = Piqirun.parse_required_field 1 parse_string x in
       let (_typename, x) =
         Piqirun.parse_required_field 2 parse_typename x in
       let (_code, x) = Piqirun.parse_required_field 3 parse_uint x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Pib_typehint.piqi_type = _piqi_type;
            Pib_typehint.typename = _typename;
            Pib_typehint.code = _code;
          }))
and parse_piq_format x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 251462090 when x = (Piqirun.Varint 1) ->
           let count = next_count () in refer count `word
       | 217697453 when x = (Piqirun.Varint 1) ->
           let count = next_count () in refer count `text
       | _ -> Piqirun.error_variant x code)
and parse_protobuf_int32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_signed_varint x))
    x
and packed_parse_protobuf_int32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_packed_signed_varint x))
    x
and parse_protobuf_int64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_signed_varint x))
    x
and packed_parse_protobuf_int64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_packed_signed_varint x))
    x
and parse_protobuf_wire_type x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_signed_varint x with
       | 329594984l -> `varint
       | 99211597l -> `zigzag_varint
       | 136997651l -> `fixed32
       | 136998322l -> `fixed64
       | 441915897l -> `signed_varint
       | 488499298l -> `signed_fixed32
       | 488499969l -> `signed_fixed64
       | 352089421l -> `block
       | x -> Piqirun.error_enum_const x)
and packed_parse_protobuf_wire_type x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_packed_signed_varint x with
       | 329594984l -> `varint
       | 99211597l -> `zigzag_varint
       | 136997651l -> `fixed32
       | 136998322l -> `fixed64
       | 441915897l -> `signed_varint
       | 488499298l -> `signed_fixed32
       | 488499969l -> `signed_fixed64
       | 352089421l -> `block
       | x -> Piqirun.error_enum_const x)
and parse_bool x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.bool_of_varint x))
    x
and packed_parse_bool x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.bool_of_packed_varint x))
    x
and parse_string x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.string_of_block x))
    x
and parse_binary x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.string_of_block x))
    x
and parse_piqi_any x = parse_any x
and parse_int x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int_of_zigzag_varint x))
    x
and packed_parse_int x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int_of_packed_zigzag_varint x))
    x
and parse_uint x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.int_of_varint x))
    x
and packed_parse_uint x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int_of_packed_varint x))
    x
and parse_int32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_zigzag_varint x))
    x
and packed_parse_int32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_packed_zigzag_varint x))
    x
and parse_uint32 x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.int32_of_varint x))
    x
and packed_parse_uint32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_packed_varint x))
    x
and parse_int64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_zigzag_varint x))
    x
and packed_parse_int64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_packed_zigzag_varint x))
    x
and parse_uint64 x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.int64_of_varint x))
    x
and packed_parse_uint64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_packed_varint x))
    x
and parse_float64 x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.float_of_fixed64 x))
    x
and packed_parse_float64 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.float_of_packed_fixed64 x))
    x
and parse_float32 x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.float_of_fixed32 x))
    x
and packed_parse_float32 x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.float_of_packed_fixed32 x))
    x
and parse_int32_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_signed_fixed32 x))
    x
and packed_parse_int32_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_packed_signed_fixed32 x))
    x
and parse_uint32_fixed x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.int32_of_fixed32 x))
    x
and packed_parse_uint32_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int32_of_packed_fixed32 x))
    x
and parse_int64_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_signed_fixed64 x))
    x
and packed_parse_int64_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_packed_signed_fixed64 x))
    x
and parse_uint64_fixed x =
  (fun x ->
     let count = next_count () in refer count (Piqirun.int64_of_fixed64 x))
    x
and packed_parse_uint64_fixed x =
  (fun x ->
     let count = next_count ()
     in refer count (Piqirun.int64_of_packed_fixed64 x))
    x
and parse_float x = parse_float64 x
and packed_parse_float x = packed_parse_float64 x
and parse_word x = parse_string x
and parse_name x = parse_word x
and parse_typedef x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 502036113 ->
           let res =
             let count = curr_count () in refer count (parse_record x)
           in `record res
       | 484589701 ->
           let res =
             let count = curr_count () in refer count (parse_variant x)
           in `variant res
       | 51800833 ->
           let res = let count = curr_count () in refer count (parse_enum x)
           in `enum res
       | 26300816 ->
           let res = let count = curr_count () in refer count (parse_alias x)
           in `alias res
       | 129178718 ->
           let res =
             let count = curr_count () in refer count (parse_piqi_list x)
           in `list res
       | _ -> Piqirun.error_variant x code)
and parse_piqi_type x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_signed_varint x with
       | 5246191l -> `int
       | 43435420l -> `float
       | 18580522l -> `bool
       | 288368849l -> `string
       | 218872833l -> `binary
       | 4848364l -> `any
       | x -> Piqirun.error_enum_const x)
and packed_parse_piqi_type x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_packed_signed_varint x with
       | 5246191l -> `int
       | 43435420l -> `float
       | 18580522l -> `bool
       | 288368849l -> `string
       | 218872833l -> `binary
       | 4848364l -> `any
       | x -> Piqirun.error_enum_const x)
and parse_typename x = parse_name x
and parse_record x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_field, x) =
         Piqirun.parse_repeated_field 9671866 parse_field x in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_protobuf_custom, x) =
         Piqirun.parse_repeated_field 112352691 parse_string x in
       let (_wire_field, x) =
         Piqirun.parse_repeated_field 112412530 parse_field x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_piq_positional, x) =
         Piqirun.parse_optional_field 197354217 parse_bool x in
       let (_parent, x) =
         Piqirun.parse_optional_field 226362666 parse_namespace x in
       let (_is_func_param, x) =
         incr_count_if_true (Piqirun.parse_flag 367658567 x) in
       let (_proto_custom, x) =
         Piqirun.parse_repeated_field 405875126 parse_string x in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Record.unparsed_piq_ast = _unparsed_piq_ast;
            Record.field = _field;
            Record.protobuf_name = _protobuf_name;
            Record.protobuf_custom = _protobuf_custom;
            Record.wire_field = _wire_field;
            Record.proto_name = _proto_name;
            Record.name = _name;
            Record.piq_positional = _piq_positional;
            Record.parent = _parent;
            Record.is_func_param = _is_func_param;
            Record.proto_custom = _proto_custom;
            Record.json_name = _json_name;
          }))
and parse_field x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_code, x) =
         Piqirun.parse_optional_field 29667629 parse_int32 x in
       let (_deprecated, x) =
         incr_count_if_true (Piqirun.parse_flag 69402483 x) in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_mode, x) =
         Piqirun.parse_required_field 140563299 parse_field_mode x
           ~default: "\b\223\162\138\147\001" in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_piqtype, x) =
         Piqirun.parse_optional_field 170743570 parse_piqtype x in
       let (_protobuf_packed, x) =
         incr_count_if_true (Piqirun.parse_flag 179842426 x) in
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
       let (_wire_packed, x) =
         incr_count_if_true (Piqirun.parse_flag 422905280 x) in
       let (_piq_alias, x) =
         Piqirun.parse_optional_field 434682011 parse_name x in
       let (_getopt_doc, x) =
         Piqirun.parse_optional_field 442330184 parse_string x in
       let (_default, x) =
         Piqirun.parse_optional_field 465819841 parse_piqi_any x in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Field.unparsed_piq_ast = _unparsed_piq_ast;
            Field.code = _code;
            Field.deprecated = _deprecated;
            Field.protobuf_name = _protobuf_name;
            Field.proto_name = _proto_name;
            Field.mode = _mode;
            Field.name = _name;
            Field.piqtype = _piqtype;
            Field.protobuf_packed = _protobuf_packed;
            Field.piq_positional = _piq_positional;
            Field.json_omit_missing = _json_omit_missing;
            Field.getopt_letter = _getopt_letter;
            Field.typename = _typename;
            Field.piq_format = _piq_format;
            Field.wire_packed = _wire_packed;
            Field.piq_alias = _piq_alias;
            Field.getopt_doc = _getopt_doc;
            Field.default = _default;
            Field.json_name = _json_name;
          }))
and parse_field_mode x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_signed_varint x with
       | 308449631l -> `required
       | 510570400l -> `optional
       | 274054266l -> `repeated
       | x -> Piqirun.error_enum_const x)
and packed_parse_field_mode x =
  let count = next_count ()
  in
    refer count
      (match Piqirun.int32_of_packed_signed_varint x with
       | 308449631l -> `required
       | 510570400l -> `optional
       | 274054266l -> `repeated
       | x -> Piqirun.error_enum_const x)
and parse_variant x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_protobuf_custom, x) =
         Piqirun.parse_repeated_field 112352691 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_option, x) =
         Piqirun.parse_repeated_field 192598901 parse_option x in
       let (_parent, x) =
         Piqirun.parse_optional_field 226362666 parse_namespace x in
       let (_is_func_param, x) =
         incr_count_if_true (Piqirun.parse_flag 367658567 x) in
       let (_proto_custom, x) =
         Piqirun.parse_repeated_field 405875126 parse_string x in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Variant.unparsed_piq_ast = _unparsed_piq_ast;
            Variant.protobuf_name = _protobuf_name;
            Variant.protobuf_custom = _protobuf_custom;
            Variant.proto_name = _proto_name;
            Variant.name = _name;
            Variant.option = _option;
            Variant.parent = _parent;
            Variant.is_func_param = _is_func_param;
            Variant.proto_custom = _proto_custom;
            Variant.json_name = _json_name;
          }))
and parse_option x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_code, x) =
         Piqirun.parse_optional_field 29667629 parse_int32 x in
       let (_deprecated, x) =
         incr_count_if_true (Piqirun.parse_flag 69402483 x) in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_piqtype, x) =
         Piqirun.parse_optional_field 170743570 parse_piqtype x in
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
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Option.unparsed_piq_ast = _unparsed_piq_ast;
            Option.code = _code;
            Option.deprecated = _deprecated;
            Option.protobuf_name = _protobuf_name;
            Option.proto_name = _proto_name;
            Option.name = _name;
            Option.piqtype = _piqtype;
            Option.getopt_letter = _getopt_letter;
            Option.typename = _typename;
            Option.piq_format = _piq_format;
            Option.piq_alias = _piq_alias;
            Option.getopt_doc = _getopt_doc;
            Option.json_name = _json_name;
          }))
and parse_enum x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_protobuf_custom, x) =
         Piqirun.parse_repeated_field 112352691 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_option, x) =
         Piqirun.parse_repeated_field 192598901 parse_option x in
       let (_parent, x) =
         Piqirun.parse_optional_field 226362666 parse_namespace x in
       let (_protobuf_prefix, x) =
         Piqirun.parse_optional_field 366391188 parse_string x in
       let (_is_func_param, x) =
         incr_count_if_true (Piqirun.parse_flag 367658567 x) in
       let (_proto_custom, x) =
         Piqirun.parse_repeated_field 405875126 parse_string x in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Enum.unparsed_piq_ast = _unparsed_piq_ast;
            Enum.protobuf_name = _protobuf_name;
            Enum.protobuf_custom = _protobuf_custom;
            Enum.proto_name = _proto_name;
            Enum.name = _name;
            Enum.option = _option;
            Enum.parent = _parent;
            Enum.protobuf_prefix = _protobuf_prefix;
            Enum.is_func_param = _is_func_param;
            Enum.proto_custom = _proto_custom;
            Enum.json_name = _json_name;
          }))
and parse_alias x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_protobuf_type, x) =
         Piqirun.parse_optional_field 157803580 parse_string x in
       let (_piqtype, x) =
         Piqirun.parse_optional_field 170743570 parse_piqtype x in
       let (_protobuf_wire_type, x) =
         Piqirun.parse_optional_field 198202944 parse_protobuf_wire_type x in
       let (_piqi_type, x) =
         Piqirun.parse_optional_field 198318774 parse_piqi_type x in
       let (_typename, x) =
         Piqirun.parse_optional_field 218690234 parse_typename x in
       let (_parent, x) =
         Piqirun.parse_optional_field 226362666 parse_namespace x in
       let (_piq_format, x) =
         Piqirun.parse_optional_field 296833484 parse_piq_format x in
       let (_is_func_param, x) =
         incr_count_if_true (Piqirun.parse_flag 367658567 x) in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Alias.unparsed_piq_ast = _unparsed_piq_ast;
            Alias.protobuf_name = _protobuf_name;
            Alias.proto_name = _proto_name;
            Alias.name = _name;
            Alias.protobuf_type = _protobuf_type;
            Alias.piqtype = _piqtype;
            Alias.protobuf_wire_type = _protobuf_wire_type;
            Alias.piqi_type = _piqi_type;
            Alias.typename = _typename;
            Alias.parent = _parent;
            Alias.piq_format = _piq_format;
            Alias.is_func_param = _is_func_param;
            Alias.json_name = _json_name;
          }))
and parse_piqi_list x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_protobuf_name, x) =
         Piqirun.parse_optional_field 90072013 parse_string x in
       let (_protobuf_custom, x) =
         Piqirun.parse_repeated_field 112352691 parse_string x in
       let (_proto_name, x) =
         Piqirun.parse_optional_field 139663632 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_piqtype, x) =
         Piqirun.parse_optional_field 170743570 parse_piqtype x in
       let (_protobuf_packed, x) =
         incr_count_if_true (Piqirun.parse_flag 179842426 x) in
       let (_typename, x) =
         Piqirun.parse_required_field 218690234 parse_typename x in
       let (_parent, x) =
         Piqirun.parse_optional_field 226362666 parse_namespace x in
       let (_piq_format, x) =
         Piqirun.parse_optional_field 296833484 parse_piq_format x in
       let (_is_func_param, x) =
         incr_count_if_true (Piqirun.parse_flag 367658567 x) in
       let (_proto_custom, x) =
         Piqirun.parse_repeated_field 405875126 parse_string x in
       let (_wire_packed, x) =
         incr_count_if_true (Piqirun.parse_flag 422905280 x) in
       let (_json_name, x) =
         Piqirun.parse_optional_field 515275216 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Piqi_list.unparsed_piq_ast = _unparsed_piq_ast;
            Piqi_list.protobuf_name = _protobuf_name;
            Piqi_list.protobuf_custom = _protobuf_custom;
            Piqi_list.proto_name = _proto_name;
            Piqi_list.name = _name;
            Piqi_list.piqtype = _piqtype;
            Piqi_list.protobuf_packed = _protobuf_packed;
            Piqi_list.typename = _typename;
            Piqi_list.parent = _parent;
            Piqi_list.piq_format = _piq_format;
            Piqi_list.is_func_param = _is_func_param;
            Piqi_list.proto_custom = _proto_custom;
            Piqi_list.wire_packed = _wire_packed;
            Piqi_list.json_name = _json_name;
          }))
and parse_piqi x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_ast, x) =
         Piqirun.parse_optional_field 4849474 parse_piq_ast x in
       let (_modname, x) =
         Piqirun.parse_optional_field 13841580 parse_word x in
       let (_imported_typedef, x) =
         Piqirun.parse_repeated_field 43698114 parse_typedef x in
       let (_file, x) =
         Piqirun.parse_optional_field 62639740 parse_string x in
       let (_extended_func, x) =
         Piqirun.parse_repeated_field 79393432 parse_func x in
       let (_protobuf_custom, x) =
         Piqirun.parse_repeated_field 112352691 parse_string x in
       let (_resolved_import, x) =
         Piqirun.parse_repeated_field 114029658 parse_import x in
       let (_extend, x) =
         Piqirun.parse_repeated_field 119198170 parse_extend x in
       let (_import, x) =
         Piqirun.parse_repeated_field 142778725 parse_import x in
       let (_included_piqi, x) =
         Piqirun.parse_repeated_field 146026754 parse_piqi x in
       let (_extended_typedef, x) =
         Piqirun.parse_repeated_field 150338679 parse_typedef x in
       let (_custom_field, x) =
         Piqirun.parse_repeated_field 162247646 parse_word x in
       let (_is_embedded, x) =
         Piqirun.parse_optional_field 260007309 parse_bool x in
       let (_resolved_func, x) =
         Piqirun.parse_repeated_field 268445433 parse_func x in
       let (_includ, x) =
         Piqirun.parse_repeated_field 301399592 parse_includ x in
       let (_func_typedef, x) =
         Piqirun.parse_repeated_field 301864450 parse_typedef x in
       let (_proto_package, x) =
         Piqirun.parse_optional_field 333467105 parse_string x in
       let (_func, x) =
         Piqirun.parse_repeated_field 340962072 parse_func x in
       let (_protobuf_package, x) =
         Piqirun.parse_optional_field 376215364 parse_string x in
       let (_proto_custom, x) =
         Piqirun.parse_repeated_field 405875126 parse_string x in
       let (_typedef, x) =
         Piqirun.parse_repeated_field 416823115 parse_typedef x in
       let (_extended_import, x) =
         Piqirun.parse_repeated_field 430482873 parse_import x in
       let (_resolved_typedef, x) =
         Piqirun.parse_repeated_field 448232118 parse_typedef x in
       let (_original_piqi, x) =
         Piqirun.parse_optional_field 455316941 parse_piqi x in
       let (_extended_func_typedef, x) =
         Piqirun.parse_repeated_field 512364886 parse_typedef x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Piqi.unparsed_piq_ast = _unparsed_piq_ast;
            Piqi.ast = _ast;
            Piqi.modname = _modname;
            Piqi.imported_typedef = _imported_typedef;
            Piqi.file = _file;
            Piqi.extended_func = _extended_func;
            Piqi.protobuf_custom = _protobuf_custom;
            Piqi.resolved_import = _resolved_import;
            Piqi.extend = _extend;
            Piqi.import = _import;
            Piqi.included_piqi = _included_piqi;
            Piqi.extended_typedef = _extended_typedef;
            Piqi.custom_field = _custom_field;
            Piqi.is_embedded = _is_embedded;
            Piqi.resolved_func = _resolved_func;
            Piqi.includ = _includ;
            Piqi.func_typedef = _func_typedef;
            Piqi.proto_package = _proto_package;
            Piqi.func = _func;
            Piqi.protobuf_package = _protobuf_package;
            Piqi.proto_custom = _proto_custom;
            Piqi.typedef = _typedef;
            Piqi.extended_import = _extended_import;
            Piqi.resolved_typedef = _resolved_typedef;
            Piqi.original_piqi = _original_piqi;
            Piqi.extended_func_typedef = _extended_func_typedef;
          }))
and parse_import x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_modname, x) =
         Piqirun.parse_required_field 13841580 parse_word x in
       let (_orig_modname, x) =
         Piqirun.parse_optional_field 65590849 parse_string x in
       let (_name, x) =
         Piqirun.parse_optional_field 150958667 parse_name x in
       let (_piqi, x) = Piqirun.parse_optional_field 173536529 parse_piqi x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Import.unparsed_piq_ast = _unparsed_piq_ast;
            Import.modname = _modname;
            Import.orig_modname = _orig_modname;
            Import.name = _name;
            Import.piqi = _piqi;
          }))
and parse_any x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_ref, x) = Piqirun.parse_optional_field 5691731 parse_int x in
       let (_xml, x) = Piqirun.parse_optional_field 5991895 parse_string x in
       let (_protobuf, x) =
         Piqirun.parse_optional_field 6461771 parse_binary x in
       let (_json, x) =
         Piqirun.parse_optional_field 107495976 parse_string x in
       let (_typename, x) =
         Piqirun.parse_optional_field 218690234 parse_string x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Any.ref = _ref;
            Any.xml = _xml;
            Any.protobuf = _protobuf;
            Any.json = _json;
            Any.typename = _typename;
          }))
and parse_func x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_resolved_input, x) =
         Piqirun.parse_optional_field 95864501 parse_typedef x in
       let (_name, x) =
         Piqirun.parse_required_field 150958667 parse_name x in
       let (_resolved_output, x) =
         Piqirun.parse_optional_field 181035510 parse_typedef x in
       let (_output, x) =
         Piqirun.parse_optional_field 209784577 parse_function_param x in
       let (_error, x) =
         Piqirun.parse_optional_field 321506248 parse_function_param x in
       let (_resolved_error, x) =
         Piqirun.parse_optional_field 448974451 parse_typedef x in
       let (_input, x) =
         Piqirun.parse_optional_field 505267210 parse_function_param x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Func.unparsed_piq_ast = _unparsed_piq_ast;
            Func.resolved_input = _resolved_input;
            Func.name = _name;
            Func.resolved_output = _resolved_output;
            Func.output = _output;
            Func.error = _error;
            Func.resolved_error = _resolved_error;
            Func.input = _input;
          }))
and parse_piqi_bundle x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_piqi, x) = Piqirun.parse_repeated_field 1 parse_piqi x
       in (Piqirun.check_unparsed_fields x; { Piqi_bundle.piqi = _piqi; }))
and parse_includ x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_modname, x) = Piqirun.parse_required_field 13841580 parse_word x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Includ.unparsed_piq_ast = _unparsed_piq_ast;
            Includ.modname = _modname;
          }))
and parse_function_param x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 150958667 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `name res
       | 502036113 ->
           let res =
             let count = curr_count () in refer count (parse_record x)
           in `record res
       | 484589701 ->
           let res =
             let count = curr_count () in refer count (parse_variant x)
           in `variant res
       | 51800833 ->
           let res = let count = curr_count () in refer count (parse_enum x)
           in `enum res
       | 129178718 ->
           let res =
             let count = curr_count () in refer count (parse_piqi_list x)
           in `list res
       | 26300816 ->
           let res = let count = curr_count () in refer count (parse_alias x)
           in `alias res
       | _ -> Piqirun.error_variant x code)
and parse_extend x =
  let x = Piqirun.parse_record x in
  let count = next_count ()
  in
    refer count
      (let (_unparsed_piq_ast, x) =
         Piqirun.parse_optional_field 1 parse_uint x in
       let (_override, x) =
         incr_count_if_true (Piqirun.parse_flag 153625164 x) in
       let (_what, x) =
         Piqirun.parse_repeated_field 251110212 parse_extend_target x in
       let (_piqi_with, x) =
         Piqirun.parse_repeated_field 251164166 parse_piqi_any x in
       let (_quote, x) =
         Piqirun.parse_repeated_field 365880944 parse_piqi_any x
       in
         (Piqirun.check_unparsed_fields x;
          {
            Extend.unparsed_piq_ast = _unparsed_piq_ast;
            Extend.override = _override;
            Extend.what = _what;
            Extend.piqi_with = _piqi_with;
            Extend.quote = _quote;
          }))
and parse_extend_target x =
  let (code, x) = Piqirun.parse_variant x in
  let count = next_count ()
  in
    refer count
      (match code with
       | 416823115 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `typedef res
       | 150958667 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `name res
       | 9671866 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `field res
       | 192598901 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `option res
       | 142778725 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `import res
       | 340962072 ->
           let res = let count = curr_count () in refer count (parse_name x)
           in `func res
       | _ -> Piqirun.error_variant x code)
  
let next_count = Piqloc.next_ocount
  
let refer obj =
  let count = next_count ()
  in if not (Obj.is_int (Obj.repr obj)) then Piqloc.addref obj count else ()
  
let reference f code x = (refer x; f code x)
  
let reference1 f x = (refer x; f x)
  
let reference_if_true f code x = if x then reference f code x else f code x
  
let rec gen__namespace code (x : Piqi_impl_piqi.namespace) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | `piqi x -> gen__piqi 173536529 x
        | `import x -> gen__import 142778725 x) ])
and gen__piqtype code (x : Piqi_impl_piqi.piqtype) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | (#Piqi_impl_piqi.typedef as x) -> gen__typedef 416823115 x
        | (#Piqi_impl_piqi.piqi_type as x) -> gen__piqi_type 198318774 x) ])
and gen__piq_ast code x = gen__bool code (Piq_ast.ast_to_bool x)
and packed_gen__piq_ast x = packed_gen__bool (Piq_ast.ast_to_bool x)
and gen__pib_typehint code x =
  (refer x;
   let _piqi_type =
     Piqirun.gen_required_field 1 gen__string x.Pib_typehint.piqi_type in
   let _typename =
     Piqirun.gen_required_field 2 gen__typename x.Pib_typehint.typename in
   let _code = Piqirun.gen_required_field 3 gen__uint x.Pib_typehint.code
   in Piqirun.gen_record code [ _piqi_type; _typename; _code ])
and gen__piq_format code (x : Piqi_impl_piqi.piq_format) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | `word -> (refer x; Piqirun.gen_bool_field 251462090 true)
        | `text -> (refer x; Piqirun.gen_bool_field 217697453 true)) ])
and gen__protobuf_int32 code x =
  reference Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x =
  reference1 Piqirun.int32_to_packed_signed_varint x
and gen__protobuf_int64 code x =
  reference Piqirun.int64_to_signed_varint code x
and packed_gen__protobuf_int64 x =
  reference1 Piqirun.int64_to_packed_signed_varint x
and gen__protobuf_wire_type code x =
  (refer x;
   Piqirun.int32_to_signed_varint code
     (match x with
      | `varint -> 329594984l
      | `zigzag_varint -> 99211597l
      | `fixed32 -> 136997651l
      | `fixed64 -> 136998322l
      | `signed_varint -> 441915897l
      | `signed_fixed32 -> 488499298l
      | `signed_fixed64 -> 488499969l
      | `block -> 352089421l))
and packed_gen__protobuf_wire_type x =
  (refer x;
   Piqirun.int32_to_packed_signed_varint
     (match x with
      | `varint -> 329594984l
      | `zigzag_varint -> 99211597l
      | `fixed32 -> 136997651l
      | `fixed64 -> 136998322l
      | `signed_varint -> 441915897l
      | `signed_fixed32 -> 488499298l
      | `signed_fixed64 -> 488499969l
      | `block -> 352089421l))
and gen__bool code x = reference Piqirun.bool_to_varint code x
and packed_gen__bool x = reference1 Piqirun.bool_to_packed_varint x
and gen__string code x = reference Piqirun.string_to_block code x
and gen__binary code x = reference Piqirun.string_to_block code x
and gen__piqi_any code x = (fun code x -> gen__any code x) code x
and gen__int code x = reference Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = reference1 Piqirun.int_to_packed_zigzag_varint x
and gen__uint code x = reference Piqirun.int_to_varint code x
and packed_gen__uint x = reference1 Piqirun.int_to_packed_varint x
and gen__int32 code x = reference Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = reference1 Piqirun.int32_to_packed_zigzag_varint x
and gen__uint32 code x = reference Piqirun.int32_to_varint code x
and packed_gen__uint32 x = reference1 Piqirun.int32_to_packed_varint x
and gen__int64 code x = reference Piqirun.int64_to_zigzag_varint code x
and packed_gen__int64 x = reference1 Piqirun.int64_to_packed_zigzag_varint x
and gen__uint64 code x = reference Piqirun.int64_to_varint code x
and packed_gen__uint64 x = reference1 Piqirun.int64_to_packed_varint x
and gen__float64 code x = reference Piqirun.float_to_fixed64 code x
and packed_gen__float64 x = reference1 Piqirun.float_to_packed_fixed64 x
and gen__float32 code x = reference Piqirun.float_to_fixed32 code x
and packed_gen__float32 x = reference1 Piqirun.float_to_packed_fixed32 x
and gen__int32_fixed code x =
  reference Piqirun.int32_to_signed_fixed32 code x
and packed_gen__int32_fixed x =
  reference1 Piqirun.int32_to_packed_signed_fixed32 x
and gen__uint32_fixed code x = reference Piqirun.int32_to_fixed32 code x
and packed_gen__uint32_fixed x = reference1 Piqirun.int32_to_packed_fixed32 x
and gen__int64_fixed code x =
  reference Piqirun.int64_to_signed_fixed64 code x
and packed_gen__int64_fixed x =
  reference1 Piqirun.int64_to_packed_signed_fixed64 x
and gen__uint64_fixed code x = reference Piqirun.int64_to_fixed64 code x
and packed_gen__uint64_fixed x = reference1 Piqirun.int64_to_packed_fixed64 x
and gen__float code x = gen__float64 code x
and packed_gen__float x = packed_gen__float64 x
and gen__word code x = gen__string code x
and gen__name code x = gen__word code x
and gen__typedef code (x : Piqi_impl_piqi.typedef) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | `record x -> gen__record 502036113 x
        | `variant x -> gen__variant 484589701 x
        | `enum x -> gen__enum 51800833 x
        | `alias x -> gen__alias 26300816 x
        | `list x -> gen__piqi_list 129178718 x) ])
and gen__piqi_type code x =
  (refer x;
   Piqirun.int32_to_signed_varint code
     (match x with
      | `int -> 5246191l
      | `float -> 43435420l
      | `bool -> 18580522l
      | `string -> 288368849l
      | `binary -> 218872833l
      | `any -> 4848364l))
and packed_gen__piqi_type x =
  (refer x;
   Piqirun.int32_to_packed_signed_varint
     (match x with
      | `int -> 5246191l
      | `float -> 43435420l
      | `bool -> 18580522l
      | `string -> 288368849l
      | `binary -> 218872833l
      | `any -> 4848364l))
and gen__typename code x = gen__name code x
and gen__record code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Record.unparsed_piq_ast in
   let _field =
     Piqirun.gen_repeated_field 9671866 gen__field x.Record.field in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Record.protobuf_name in
   let _protobuf_custom =
     Piqirun.gen_repeated_field 112352691 gen__string
       x.Record.protobuf_custom in
   let _wire_field =
     Piqirun.gen_repeated_field 112412530 gen__field x.Record.wire_field in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Record.proto_name in
   let _name =
     Piqirun.gen_optional_field 150958667 gen__name x.Record.name in
   let _piq_positional =
     Piqirun.gen_optional_field 197354217 gen__bool x.Record.piq_positional in
   let _parent =
     Piqirun.gen_optional_field 226362666 gen__namespace x.Record.parent in
   let _is_func_param =
     reference_if_true Piqirun.gen_flag 367658567 x.Record.is_func_param in
   let _proto_custom =
     Piqirun.gen_repeated_field 405875126 gen__string x.Record.proto_custom in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Record.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _field; _protobuf_name; _protobuf_custom;
         _wire_field; _proto_name; _name; _piq_positional; _parent;
         _is_func_param; _proto_custom; _json_name ])
and gen__field code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Field.unparsed_piq_ast in
   let _code = Piqirun.gen_optional_field 29667629 gen__int32 x.Field.code in
   let _deprecated =
     reference_if_true Piqirun.gen_flag 69402483 x.Field.deprecated in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Field.protobuf_name in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Field.proto_name in
   let _mode =
     Piqirun.gen_required_field 140563299 gen__field_mode x.Field.mode in
   let _name = Piqirun.gen_optional_field 150958667 gen__name x.Field.name in
   let _piqtype =
     Piqirun.gen_optional_field 170743570 gen__piqtype x.Field.piqtype in
   let _protobuf_packed =
     reference_if_true Piqirun.gen_flag 179842426 x.Field.protobuf_packed in
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
   let _wire_packed =
     reference_if_true Piqirun.gen_flag 422905280 x.Field.wire_packed in
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
       [ _unparsed_piq_ast; _code; _deprecated; _protobuf_name; _proto_name;
         _mode; _name; _piqtype; _protobuf_packed; _piq_positional;
         _json_omit_missing; _getopt_letter; _typename; _piq_format;
         _wire_packed; _piq_alias; _getopt_doc; _default; _json_name ])
and gen__field_mode code x =
  (refer x;
   Piqirun.int32_to_signed_varint code
     (match x with
      | `required -> 308449631l
      | `optional -> 510570400l
      | `repeated -> 274054266l))
and packed_gen__field_mode x =
  (refer x;
   Piqirun.int32_to_packed_signed_varint
     (match x with
      | `required -> 308449631l
      | `optional -> 510570400l
      | `repeated -> 274054266l))
and gen__variant code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Variant.unparsed_piq_ast in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Variant.protobuf_name in
   let _protobuf_custom =
     Piqirun.gen_repeated_field 112352691 gen__string
       x.Variant.protobuf_custom in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Variant.proto_name in
   let _name =
     Piqirun.gen_optional_field 150958667 gen__name x.Variant.name in
   let _option =
     Piqirun.gen_repeated_field 192598901 gen__option x.Variant.option in
   let _parent =
     Piqirun.gen_optional_field 226362666 gen__namespace x.Variant.parent in
   let _is_func_param =
     reference_if_true Piqirun.gen_flag 367658567 x.Variant.is_func_param in
   let _proto_custom =
     Piqirun.gen_repeated_field 405875126 gen__string x.Variant.proto_custom in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Variant.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _protobuf_name; _protobuf_custom; _proto_name;
         _name; _option; _parent; _is_func_param; _proto_custom; _json_name ])
and gen__option code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Option.unparsed_piq_ast in
   let _code =
     Piqirun.gen_optional_field 29667629 gen__int32 x.Option.code in
   let _deprecated =
     reference_if_true Piqirun.gen_flag 69402483 x.Option.deprecated in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Option.protobuf_name in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Option.proto_name in
   let _name =
     Piqirun.gen_optional_field 150958667 gen__name x.Option.name in
   let _piqtype =
     Piqirun.gen_optional_field 170743570 gen__piqtype x.Option.piqtype in
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
       [ _unparsed_piq_ast; _code; _deprecated; _protobuf_name; _proto_name;
         _name; _piqtype; _getopt_letter; _typename; _piq_format; _piq_alias;
         _getopt_doc; _json_name ])
and gen__enum code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Enum.unparsed_piq_ast in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Enum.protobuf_name in
   let _protobuf_custom =
     Piqirun.gen_repeated_field 112352691 gen__string x.Enum.protobuf_custom in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Enum.proto_name in
   let _name = Piqirun.gen_optional_field 150958667 gen__name x.Enum.name in
   let _option =
     Piqirun.gen_repeated_field 192598901 gen__option x.Enum.option in
   let _parent =
     Piqirun.gen_optional_field 226362666 gen__namespace x.Enum.parent in
   let _protobuf_prefix =
     Piqirun.gen_optional_field 366391188 gen__string x.Enum.protobuf_prefix in
   let _is_func_param =
     reference_if_true Piqirun.gen_flag 367658567 x.Enum.is_func_param in
   let _proto_custom =
     Piqirun.gen_repeated_field 405875126 gen__string x.Enum.proto_custom in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Enum.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _protobuf_name; _protobuf_custom; _proto_name;
         _name; _option; _parent; _protobuf_prefix; _is_func_param;
         _proto_custom; _json_name ])
and gen__alias code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Alias.unparsed_piq_ast in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string x.Alias.protobuf_name in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Alias.proto_name in
   let _name = Piqirun.gen_optional_field 150958667 gen__name x.Alias.name in
   let _protobuf_type =
     Piqirun.gen_optional_field 157803580 gen__string x.Alias.protobuf_type in
   let _piqtype =
     Piqirun.gen_optional_field 170743570 gen__piqtype x.Alias.piqtype in
   let _protobuf_wire_type =
     Piqirun.gen_optional_field 198202944 gen__protobuf_wire_type
       x.Alias.protobuf_wire_type in
   let _piqi_type =
     Piqirun.gen_optional_field 198318774 gen__piqi_type x.Alias.piqi_type in
   let _typename =
     Piqirun.gen_optional_field 218690234 gen__typename x.Alias.typename in
   let _parent =
     Piqirun.gen_optional_field 226362666 gen__namespace x.Alias.parent in
   let _piq_format =
     Piqirun.gen_optional_field 296833484 gen__piq_format x.Alias.piq_format in
   let _is_func_param =
     reference_if_true Piqirun.gen_flag 367658567 x.Alias.is_func_param in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Alias.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _protobuf_name; _proto_name; _name;
         _protobuf_type; _piqtype; _protobuf_wire_type; _piqi_type;
         _typename; _parent; _piq_format; _is_func_param; _json_name ])
and gen__piqi_list code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Piqi_list.unparsed_piq_ast in
   let _protobuf_name =
     Piqirun.gen_optional_field 90072013 gen__string
       x.Piqi_list.protobuf_name in
   let _protobuf_custom =
     Piqirun.gen_repeated_field 112352691 gen__string
       x.Piqi_list.protobuf_custom in
   let _proto_name =
     Piqirun.gen_optional_field 139663632 gen__string x.Piqi_list.proto_name in
   let _name =
     Piqirun.gen_optional_field 150958667 gen__name x.Piqi_list.name in
   let _piqtype =
     Piqirun.gen_optional_field 170743570 gen__piqtype x.Piqi_list.piqtype in
   let _protobuf_packed =
     reference_if_true Piqirun.gen_flag 179842426 x.Piqi_list.protobuf_packed in
   let _typename =
     Piqirun.gen_required_field 218690234 gen__typename x.Piqi_list.typename in
   let _parent =
     Piqirun.gen_optional_field 226362666 gen__namespace x.Piqi_list.parent in
   let _piq_format =
     Piqirun.gen_optional_field 296833484 gen__piq_format
       x.Piqi_list.piq_format in
   let _is_func_param =
     reference_if_true Piqirun.gen_flag 367658567 x.Piqi_list.is_func_param in
   let _proto_custom =
     Piqirun.gen_repeated_field 405875126 gen__string
       x.Piqi_list.proto_custom in
   let _wire_packed =
     reference_if_true Piqirun.gen_flag 422905280 x.Piqi_list.wire_packed in
   let _json_name =
     Piqirun.gen_optional_field 515275216 gen__string x.Piqi_list.json_name
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _protobuf_name; _protobuf_custom; _proto_name;
         _name; _piqtype; _protobuf_packed; _typename; _parent; _piq_format;
         _is_func_param; _proto_custom; _wire_packed; _json_name ])
and gen__piqi code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Piqi.unparsed_piq_ast in
   let _ast = Piqirun.gen_optional_field 4849474 gen__piq_ast x.Piqi.ast in
   let _modname =
     Piqirun.gen_optional_field 13841580 gen__word x.Piqi.modname in
   let _imported_typedef =
     Piqirun.gen_repeated_field 43698114 gen__typedef x.Piqi.imported_typedef in
   let _file = Piqirun.gen_optional_field 62639740 gen__string x.Piqi.file in
   let _extended_func =
     Piqirun.gen_repeated_field 79393432 gen__func x.Piqi.extended_func in
   let _protobuf_custom =
     Piqirun.gen_repeated_field 112352691 gen__string x.Piqi.protobuf_custom in
   let _resolved_import =
     Piqirun.gen_repeated_field 114029658 gen__import x.Piqi.resolved_import in
   let _extend =
     Piqirun.gen_repeated_field 119198170 gen__extend x.Piqi.extend in
   let _import =
     Piqirun.gen_repeated_field 142778725 gen__import x.Piqi.import in
   let _included_piqi =
     Piqirun.gen_repeated_field 146026754 gen__piqi x.Piqi.included_piqi in
   let _extended_typedef =
     Piqirun.gen_repeated_field 150338679 gen__typedef
       x.Piqi.extended_typedef in
   let _custom_field =
     Piqirun.gen_repeated_field 162247646 gen__word x.Piqi.custom_field in
   let _is_embedded =
     Piqirun.gen_optional_field 260007309 gen__bool x.Piqi.is_embedded in
   let _resolved_func =
     Piqirun.gen_repeated_field 268445433 gen__func x.Piqi.resolved_func in
   let _includ =
     Piqirun.gen_repeated_field 301399592 gen__includ x.Piqi.includ in
   let _func_typedef =
     Piqirun.gen_repeated_field 301864450 gen__typedef x.Piqi.func_typedef in
   let _proto_package =
     Piqirun.gen_optional_field 333467105 gen__string x.Piqi.proto_package in
   let _func = Piqirun.gen_repeated_field 340962072 gen__func x.Piqi.func in
   let _protobuf_package =
     Piqirun.gen_optional_field 376215364 gen__string x.Piqi.protobuf_package in
   let _proto_custom =
     Piqirun.gen_repeated_field 405875126 gen__string x.Piqi.proto_custom in
   let _typedef =
     Piqirun.gen_repeated_field 416823115 gen__typedef x.Piqi.typedef in
   let _extended_import =
     Piqirun.gen_repeated_field 430482873 gen__import x.Piqi.extended_import in
   let _resolved_typedef =
     Piqirun.gen_repeated_field 448232118 gen__typedef
       x.Piqi.resolved_typedef in
   let _original_piqi =
     Piqirun.gen_optional_field 455316941 gen__piqi x.Piqi.original_piqi in
   let _extended_func_typedef =
     Piqirun.gen_repeated_field 512364886 gen__typedef
       x.Piqi.extended_func_typedef
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _ast; _modname; _imported_typedef; _file;
         _extended_func; _protobuf_custom; _resolved_import; _extend;
         _import; _included_piqi; _extended_typedef; _custom_field;
         _is_embedded; _resolved_func; _includ; _func_typedef;
         _proto_package; _func; _protobuf_package; _proto_custom; _typedef;
         _extended_import; _resolved_typedef; _original_piqi;
         _extended_func_typedef ])
and gen__import code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Import.unparsed_piq_ast in
   let _modname =
     Piqirun.gen_required_field 13841580 gen__word x.Import.modname in
   let _orig_modname =
     Piqirun.gen_optional_field 65590849 gen__string x.Import.orig_modname in
   let _name =
     Piqirun.gen_optional_field 150958667 gen__name x.Import.name in
   let _piqi = Piqirun.gen_optional_field 173536529 gen__piqi x.Import.piqi
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _modname; _orig_modname; _name; _piqi ])
and gen__any code x =
  (refer x;
   let _ref = Piqirun.gen_optional_field 5691731 gen__int x.Any.ref in
   let _xml = Piqirun.gen_optional_field 5991895 gen__string x.Any.xml in
   let _protobuf =
     Piqirun.gen_optional_field 6461771 gen__binary x.Any.protobuf in
   let _json = Piqirun.gen_optional_field 107495976 gen__string x.Any.json in
   let _typename =
     Piqirun.gen_optional_field 218690234 gen__string x.Any.typename
   in Piqirun.gen_record code [ _ref; _xml; _protobuf; _json; _typename ])
and gen__func code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Func.unparsed_piq_ast in
   let _resolved_input =
     Piqirun.gen_optional_field 95864501 gen__typedef x.Func.resolved_input in
   let _name = Piqirun.gen_required_field 150958667 gen__name x.Func.name in
   let _resolved_output =
     Piqirun.gen_optional_field 181035510 gen__typedef x.Func.resolved_output in
   let _output =
     Piqirun.gen_optional_field 209784577 gen__function_param x.Func.output in
   let _error =
     Piqirun.gen_optional_field 321506248 gen__function_param x.Func.error in
   let _resolved_error =
     Piqirun.gen_optional_field 448974451 gen__typedef x.Func.resolved_error in
   let _input =
     Piqirun.gen_optional_field 505267210 gen__function_param x.Func.input
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _resolved_input; _name; _resolved_output;
         _output; _error; _resolved_error; _input ])
and gen__piqi_bundle code x =
  (refer x;
   let _piqi = Piqirun.gen_repeated_field 1 gen__piqi x.Piqi_bundle.piqi
   in Piqirun.gen_record code [ _piqi ])
and gen__includ code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Includ.unparsed_piq_ast in
   let _modname =
     Piqirun.gen_required_field 13841580 gen__word x.Includ.modname
   in Piqirun.gen_record code [ _unparsed_piq_ast; _modname ])
and gen__function_param code (x : Piqi_impl_piqi.function_param) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | `name x -> gen__name 150958667 x
        | `record x -> gen__record 502036113 x
        | `variant x -> gen__variant 484589701 x
        | `enum x -> gen__enum 51800833 x
        | `list x -> gen__piqi_list 129178718 x
        | `alias x -> gen__alias 26300816 x) ])
and gen__extend code x =
  (refer x;
   let _unparsed_piq_ast =
     Piqirun.gen_optional_field 1 gen__uint x.Extend.unparsed_piq_ast in
   let _override =
     reference_if_true Piqirun.gen_flag 153625164 x.Extend.override in
   let _what =
     Piqirun.gen_repeated_field 251110212 gen__extend_target x.Extend.what in
   let _piqi_with =
     Piqirun.gen_repeated_field 251164166 gen__piqi_any x.Extend.piqi_with in
   let _quote =
     Piqirun.gen_repeated_field 365880944 gen__piqi_any x.Extend.quote
   in
     Piqirun.gen_record code
       [ _unparsed_piq_ast; _override; _what; _piqi_with; _quote ])
and gen__extend_target code (x : Piqi_impl_piqi.extend_target) =
  (refer x;
   Piqirun.gen_record code
     [ (match x with
        | `typedef x -> gen__name 416823115 x
        | `name x -> gen__name 150958667 x
        | `field x -> gen__name 9671866 x
        | `option x -> gen__name 192598901 x
        | `import x -> gen__name 142778725 x
        | `func x -> gen__name 340962072 x) ])
  
let gen_namespace x = gen__namespace (-1) x
  
let gen_piqtype x = gen__piqtype (-1) x
  
let gen_piq_ast x = gen__piq_ast (-1) x
  
let gen_pib_typehint x = gen__pib_typehint (-1) x
  
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
  
let gen_includ x = gen__includ (-1) x
  
let gen_function_param x = gen__function_param (-1) x
  
let gen_extend x = gen__extend (-1) x
  
let gen_extend_target x = gen__extend_target (-1) x
  
let rec default_namespace () = `piqi (default_piqi ())
and default_piqtype () = (default_typedef () :> Piqi_impl_piqi.piqtype)
and default_piq_ast () = Piq_ast.ast_of_bool (default_bool ())
and default_pib_typehint () =
  {
    Pib_typehint.piqi_type = default_string ();
    Pib_typehint.typename = default_typename ();
    Pib_typehint.code = default_uint ();
  }
and default_piq_format () = `word
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
    Record.unparsed_piq_ast = None;
    Record.field = [];
    Record.protobuf_name = None;
    Record.protobuf_custom = [];
    Record.wire_field = [];
    Record.proto_name = None;
    Record.name = None;
    Record.piq_positional = None;
    Record.parent = None;
    Record.is_func_param = false;
    Record.proto_custom = [];
    Record.json_name = None;
  }
and default_field () =
  {
    Field.unparsed_piq_ast = None;
    Field.code = None;
    Field.deprecated = false;
    Field.protobuf_name = None;
    Field.proto_name = None;
    Field.mode =
      parse_field_mode (Piqirun.parse_default "\b\223\162\138\147\001");
    Field.name = None;
    Field.piqtype = None;
    Field.protobuf_packed = false;
    Field.piq_positional = None;
    Field.json_omit_missing = None;
    Field.getopt_letter = None;
    Field.typename = None;
    Field.piq_format = None;
    Field.wire_packed = false;
    Field.piq_alias = None;
    Field.getopt_doc = None;
    Field.default = None;
    Field.json_name = None;
  }
and default_field_mode () = `required
and default_variant () =
  {
    Variant.unparsed_piq_ast = None;
    Variant.protobuf_name = None;
    Variant.protobuf_custom = [];
    Variant.proto_name = None;
    Variant.name = None;
    Variant.option = [];
    Variant.parent = None;
    Variant.is_func_param = false;
    Variant.proto_custom = [];
    Variant.json_name = None;
  }
and default_option () =
  {
    Option.unparsed_piq_ast = None;
    Option.code = None;
    Option.deprecated = false;
    Option.protobuf_name = None;
    Option.proto_name = None;
    Option.name = None;
    Option.piqtype = None;
    Option.getopt_letter = None;
    Option.typename = None;
    Option.piq_format = None;
    Option.piq_alias = None;
    Option.getopt_doc = None;
    Option.json_name = None;
  }
and default_enum () =
  {
    Enum.unparsed_piq_ast = None;
    Enum.protobuf_name = None;
    Enum.protobuf_custom = [];
    Enum.proto_name = None;
    Enum.name = None;
    Enum.option = [];
    Enum.parent = None;
    Enum.protobuf_prefix = None;
    Enum.is_func_param = false;
    Enum.proto_custom = [];
    Enum.json_name = None;
  }
and default_alias () =
  {
    Alias.unparsed_piq_ast = None;
    Alias.protobuf_name = None;
    Alias.proto_name = None;
    Alias.name = None;
    Alias.protobuf_type = None;
    Alias.piqtype = None;
    Alias.protobuf_wire_type = None;
    Alias.piqi_type = None;
    Alias.typename = None;
    Alias.parent = None;
    Alias.piq_format = None;
    Alias.is_func_param = false;
    Alias.json_name = None;
  }
and default_piqi_list () =
  {
    Piqi_list.unparsed_piq_ast = None;
    Piqi_list.protobuf_name = None;
    Piqi_list.protobuf_custom = [];
    Piqi_list.proto_name = None;
    Piqi_list.name = None;
    Piqi_list.piqtype = None;
    Piqi_list.protobuf_packed = false;
    Piqi_list.typename = default_typename ();
    Piqi_list.parent = None;
    Piqi_list.piq_format = None;
    Piqi_list.is_func_param = false;
    Piqi_list.proto_custom = [];
    Piqi_list.wire_packed = false;
    Piqi_list.json_name = None;
  }
and default_piqi () =
  {
    Piqi.unparsed_piq_ast = None;
    Piqi.ast = None;
    Piqi.modname = None;
    Piqi.imported_typedef = [];
    Piqi.file = None;
    Piqi.extended_func = [];
    Piqi.protobuf_custom = [];
    Piqi.resolved_import = [];
    Piqi.extend = [];
    Piqi.import = [];
    Piqi.included_piqi = [];
    Piqi.extended_typedef = [];
    Piqi.custom_field = [];
    Piqi.is_embedded = None;
    Piqi.resolved_func = [];
    Piqi.includ = [];
    Piqi.func_typedef = [];
    Piqi.proto_package = None;
    Piqi.func = [];
    Piqi.protobuf_package = None;
    Piqi.proto_custom = [];
    Piqi.typedef = [];
    Piqi.extended_import = [];
    Piqi.resolved_typedef = [];
    Piqi.original_piqi = None;
    Piqi.extended_func_typedef = [];
  }
and default_import () =
  {
    Import.unparsed_piq_ast = None;
    Import.modname = default_word ();
    Import.orig_modname = None;
    Import.name = None;
    Import.piqi = None;
  }
and default_any () =
  {
    Any.ref = None;
    Any.xml = None;
    Any.protobuf = None;
    Any.json = None;
    Any.typename = None;
  }
and default_func () =
  {
    Func.unparsed_piq_ast = None;
    Func.resolved_input = None;
    Func.name = default_name ();
    Func.resolved_output = None;
    Func.output = None;
    Func.error = None;
    Func.resolved_error = None;
    Func.input = None;
  }
and default_piqi_bundle () = { Piqi_bundle.piqi = []; }
and default_includ () =
  { Includ.unparsed_piq_ast = None; Includ.modname = default_word (); }
and default_function_param () = `name (default_name ())
and default_extend () =
  {
    Extend.unparsed_piq_ast = None;
    Extend.override = false;
    Extend.what = [];
    Extend.piqi_with = [];
    Extend.quote = [];
  }
and default_extend_target () = `typedef (default_name ())
  
include Piqi_impl_piqi
  

