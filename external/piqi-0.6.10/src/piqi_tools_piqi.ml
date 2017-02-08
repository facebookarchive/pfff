module Piqirun = Piqi_piqirun
  
module rec Piqi_tools_piqi :
             sig
               type binary = string
               
               type add_piqi_error = string
               
               type convert_error = string
               
               type format = [ | `piq | `json | `pb | `xml ]
               
               type add_piqi_input = Add_piqi_input.t
               
               type convert_input = Convert_input.t
               
               type convert_output = Convert_output.t
               
             end = Piqi_tools_piqi
and
  Add_piqi_input :
    sig
      type t =
        { mutable format : Piqi_tools_piqi.format;
          mutable data : Piqi_tools_piqi.binary list
        }
      
    end = Add_piqi_input
and
  Convert_input :
    sig
      type t =
        { mutable type_name : string; mutable data : Piqi_tools_piqi.binary;
          mutable input_format : Piqi_tools_piqi.format;
          mutable output_format : Piqi_tools_piqi.format;
          mutable pretty_print : bool;
          mutable json_omit_missing_fields : bool;
          mutable use_strict_parsing : bool;
          mutable piq_frameless_output : bool;
          mutable piq_frameless_input : bool;
          mutable piq_relaxed_parsing : bool
        }
      
    end = Convert_input
and
  Convert_output :
    sig type t = { mutable data : Piqi_tools_piqi.binary }
         end =
    Convert_output
  
let rec parse_binary x = Piqirun.string_of_block x
and parse_string x = Piqirun.string_of_block x
and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x
and parse_format x =
  match Piqirun.int32_of_signed_varint x with
  | 1l -> `piq
  | 2l -> `json
  | 3l -> `pb
  | 4l -> `xml
  | x -> Piqirun.error_enum_const x
and packed_parse_format x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 1l -> `piq
  | 2l -> `json
  | 3l -> `pb
  | 4l -> `xml
  | x -> Piqirun.error_enum_const x
and parse_add_piqi_input x =
  let x = Piqirun.parse_record x in
  let (_format, x) = Piqirun.parse_required_field 1 parse_format x in
  let (_data, x) = Piqirun.parse_repeated_field 2 parse_binary x
  in
    (Piqirun.check_unparsed_fields x;
     { Add_piqi_input.format = _format; Add_piqi_input.data = _data; })
and parse_add_piqi_error x = parse_string x
and parse_convert_input x =
  let x = Piqirun.parse_record x in
  let (_type_name, x) = Piqirun.parse_required_field 1 parse_string x in
  let (_data, x) = Piqirun.parse_required_field 2 parse_binary x in
  let (_input_format, x) = Piqirun.parse_required_field 3 parse_format x in
  let (_output_format, x) = Piqirun.parse_required_field 4 parse_format x in
  let (_pretty_print, x) =
    Piqirun.parse_required_field 5 parse_bool x ~default: "\b\001" in
  let (_json_omit_missing_fields, x) =
    Piqirun.parse_required_field 6 parse_bool x ~default: "\b\001" in
  let (_use_strict_parsing, x) =
    Piqirun.parse_required_field 7 parse_bool x ~default: "\b\000" in
  let (_piq_frameless_output, x) =
    Piqirun.parse_required_field 8 parse_bool x ~default: "\b\000" in
  let (_piq_frameless_input, x) =
    Piqirun.parse_required_field 9 parse_bool x ~default: "\b\000" in
  let (_piq_relaxed_parsing, x) =
    Piqirun.parse_required_field 10 parse_bool x ~default: "\b\000"
  in
    (Piqirun.check_unparsed_fields x;
     {
       Convert_input.type_name = _type_name;
       Convert_input.data = _data;
       Convert_input.input_format = _input_format;
       Convert_input.output_format = _output_format;
       Convert_input.pretty_print = _pretty_print;
       Convert_input.json_omit_missing_fields = _json_omit_missing_fields;
       Convert_input.use_strict_parsing = _use_strict_parsing;
       Convert_input.piq_frameless_output = _piq_frameless_output;
       Convert_input.piq_frameless_input = _piq_frameless_input;
       Convert_input.piq_relaxed_parsing = _piq_relaxed_parsing;
     })
and parse_convert_output x =
  let x = Piqirun.parse_record x in
  let (_data, x) = Piqirun.parse_required_field 1 parse_binary x
  in (Piqirun.check_unparsed_fields x; { Convert_output.data = _data; })
and parse_convert_error x = parse_string x
  
let rec gen__binary code x = Piqirun.string_to_block code x
and gen__string code x = Piqirun.string_to_block code x
and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x
and gen__format code x =
  Piqirun.int32_to_signed_varint code
    (match x with | `piq -> 1l | `json -> 2l | `pb -> 3l | `xml -> 4l)
and packed_gen__format x =
  Piqirun.int32_to_packed_signed_varint
    (match x with | `piq -> 1l | `json -> 2l | `pb -> 3l | `xml -> 4l)
and gen__add_piqi_input code x =
  let _format =
    Piqirun.gen_required_field 1 gen__format x.Add_piqi_input.format in
  let _data = Piqirun.gen_repeated_field 2 gen__binary x.Add_piqi_input.data
  in Piqirun.gen_record code [ _format; _data ]
and gen__add_piqi_error code x = gen__string code x
and gen__convert_input code x =
  let _type_name =
    Piqirun.gen_required_field 1 gen__string x.Convert_input.type_name in
  let _data =
    Piqirun.gen_required_field 2 gen__binary x.Convert_input.data in
  let _input_format =
    Piqirun.gen_required_field 3 gen__format x.Convert_input.input_format in
  let _output_format =
    Piqirun.gen_required_field 4 gen__format x.Convert_input.output_format in
  let _pretty_print =
    Piqirun.gen_required_field 5 gen__bool x.Convert_input.pretty_print in
  let _json_omit_missing_fields =
    Piqirun.gen_required_field 6 gen__bool
      x.Convert_input.json_omit_missing_fields in
  let _use_strict_parsing =
    Piqirun.gen_required_field 7 gen__bool x.Convert_input.use_strict_parsing in
  let _piq_frameless_output =
    Piqirun.gen_required_field 8 gen__bool
      x.Convert_input.piq_frameless_output in
  let _piq_frameless_input =
    Piqirun.gen_required_field 9 gen__bool
      x.Convert_input.piq_frameless_input in
  let _piq_relaxed_parsing =
    Piqirun.gen_required_field 10 gen__bool
      x.Convert_input.piq_relaxed_parsing
  in
    Piqirun.gen_record code
      [ _type_name; _data; _input_format; _output_format; _pretty_print;
        _json_omit_missing_fields; _use_strict_parsing;
        _piq_frameless_output; _piq_frameless_input; _piq_relaxed_parsing ]
and gen__convert_output code x =
  let _data = Piqirun.gen_required_field 1 gen__binary x.Convert_output.data
  in Piqirun.gen_record code [ _data ]
and gen__convert_error code x = gen__string code x
  
let gen_binary x = gen__binary (-1) x
  
let gen_string x = gen__string (-1) x
  
let gen_bool x = gen__bool (-1) x
  
let gen_format x = gen__format (-1) x
  
let gen_add_piqi_input x = gen__add_piqi_input (-1) x
  
let gen_add_piqi_error x = gen__add_piqi_error (-1) x
  
let gen_convert_input x = gen__convert_input (-1) x
  
let gen_convert_output x = gen__convert_output (-1) x
  
let gen_convert_error x = gen__convert_error (-1) x
  
let rec default_binary () = ""
and default_string () = ""
and default_bool () = false
and default_format () = `piq
and default_add_piqi_input () =
  { Add_piqi_input.format = default_format (); Add_piqi_input.data = []; }
and default_add_piqi_error () = default_string ()
and default_convert_input () =
  {
    Convert_input.type_name = default_string ();
    Convert_input.data = default_binary ();
    Convert_input.input_format = default_format ();
    Convert_input.output_format = default_format ();
    Convert_input.pretty_print = parse_bool (Piqirun.parse_default "\b\001");
    Convert_input.json_omit_missing_fields =
      parse_bool (Piqirun.parse_default "\b\001");
    Convert_input.use_strict_parsing =
      parse_bool (Piqirun.parse_default "\b\000");
    Convert_input.piq_frameless_output =
      parse_bool (Piqirun.parse_default "\b\000");
    Convert_input.piq_frameless_input =
      parse_bool (Piqirun.parse_default "\b\000");
    Convert_input.piq_relaxed_parsing =
      parse_bool (Piqirun.parse_default "\b\000");
  }
and default_convert_output () = { Convert_output.data = default_binary (); }
and default_convert_error () = default_string ()
  
let piqi =
  "\226\202\2304\npiqi_tools\226\231\249\238\001\015piqi_tools.piqi\194\177\213\148\n.\218\164\238\191\004\badd-piqi\194\188\185\202\t\006string\210\192\184\135\015\014add-piqi-input\194\177\213\148\n@\218\164\238\191\004\007convert\138\240\161\160\006\014convert-output\194\188\185\202\t\006string\210\192\184\135\015\rconvert-input\194\177\213\148\n\n\218\164\238\191\004\004ping\218\244\134\182\012b\138\176\205\197\001\\\218\164\238\191\004\006format\170\183\218\222\005\014\232\146\150q\002\218\164\238\191\004\003piq\170\183\218\222\005\015\232\146\150q\004\218\164\238\191\004\004json\170\183\218\222\005\r\232\146\150q\006\218\164\238\191\004\002pb\170\183\218\222\005\014\232\146\150q\b\218\164\238\191\004\003xml\218\244\134\182\012d\138\233\142\251\014^\210\203\242$\027\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\210\171\158\194\006\006format\210\203\242$%\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\004data\210\171\158\194\006\006binary\218\164\238\191\004\014add-piqi-input\218\244\134\182\012%\130\153\170d \218\164\238\191\004\014add-piqi-error\210\171\158\194\006\006string\218\244\134\182\012\170\005\138\233\142\251\014\163\005\210\203\242$*\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ttype-name\210\171\158\194\006\006string\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004data\210\171\158\194\006\006binary\210\203\242$-\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012input-format\210\171\158\194\006\006format\210\203\242$.\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\routput-format\210\171\158\194\006\006format\210\203\242$B\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012pretty-print\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\001\210\171\158\194\006\004bool\210\203\242$N\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\024json-omit-missing-fields\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\001\210\171\158\194\006\004bool\210\203\242$H\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\018use-strict-parsing\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$J\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\020piq-frameless-output\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$I\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\019piq-frameless-input\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$I\232\146\150q\020\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\019piq-relaxed-parsing\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\218\164\238\191\004\rconvert-input\218\244\134\182\012D\138\233\142\251\014>\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004data\210\171\158\194\006\006binary\218\164\238\191\004\014convert-output\218\244\134\182\012$\130\153\170d\031\218\164\238\191\004\rconvert-error\210\171\158\194\006\006string"
  
include Piqi_tools_piqi
  

