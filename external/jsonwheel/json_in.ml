type t = Json_type.t
open Json_type


let filter_result x =
  Browse.assert_object_or_array x;
  x


let check_channel_is_utf8 ic =
  let start = pos_in ic in
  let encoding =
    try
      let c1 = input_char ic in
      let c2 = input_char ic in
      let c3 = input_char ic in
      let c4 = input_char ic in
      Json_lexer.detect_encoding c1 c2 c3 c4
    with End_of_file -> `UTF8 in
  if encoding <> `UTF8 then
    json_error "Only UTF-8 encoding is supported";
  (try seek_in ic start
   with _ -> json_error "Not a regular file")

(* from_channel and from_channel4 work 
   only on seekable devices (regular files) *)
let from_channel p recursive file ic = 
  check_channel_is_utf8 ic;
  let lexbuf = Lexing.from_channel ic in
  Json_lexer.set_file_name lexbuf file;
  let j =
    Json_parser.main
      (Json_lexer.token p)
      lexbuf
  in
  if recursive then j
  else filter_result j

let load_json
    ?allow_comments ?allow_nan ?big_int_mode ?(recursive = false)
    file = 
  let ic = open_in file in
  let x =
    let p =
      Json_lexer.make_param ?allow_comments ?allow_nan ?big_int_mode () in
    try `Result (from_channel p recursive file ic)
    with e -> `Exn e in
  close_in ic;
  match x with
      `Result x -> x
    | `Exn e -> raise e
