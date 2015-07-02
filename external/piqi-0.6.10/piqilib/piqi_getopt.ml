(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


(*
 * Interpreting command-line arguments as Piq data
 *)


(* @doc

   Piqi getopt uses different option syntax than Posix/GNU getopt, because their
   syntax is way too relaxed and imprecise. These are examples of GNU getopt
   options and their possible meanings:

        --c-long=10 // c-long = 10
        -c 10 // c, 10
        -c10 // c = 10
        -ac10 // a, c = 10
        -ca10 // c = a10

   In Piqi getopt, both short and long options are supported. Both type of
   options must be seprated from a value by whitespace, e.g.

        -c 10
        --c-long 10

   Short options start with '-' character followed by one or more letters. In
   the latter case, each letter is treated as if it was specified separaterly.
   For example,

        -abc 10

        is equivalent to

        -a -b -c 10

   '-' followed by a <number> is normally treated as a negative number, e.g.

        -10
        -0.nan
        -0.0
        -0.infinity

    Words will be treated either as Piq strings or binaries or words, depending
    on the expected type. Examples of words:

        a

        foo

    Strings or binaries can be specified explicitly using Piq string syntax.

        '"a"'

        '"foo\u0000"'

        '"\x00\n\r"'

    Lists can be specified using regular Piq syntax, but '[' and ']' characters
    can be specified as separate arguments and not as a part of other arguments.
    Examples:

        []

        [ a b ] // this is correct
        [a b]   // this is incorrect

        [ a b 10 -1 ]

        [ a b [ c d ] ]


    Values for the arguments that start with '@' character will be loaded from a
    file which names follows the '@' character. For example:

        @foo  // string or binary value will be loaded from file "foo"

TODO:   @-    // string or binary value will be loaded from stdin
*)


module C = Piqi_common
open C


(*
 * Set "alt-name" fields for Piqi options and fields based on "getopt-name"
 * fields provided by user in the Piqi spec.
 *
 * "alt-name" field is specific to the library implementation while
 * "getopt-name" field is a part of public Piqi specification.
 *)

let check_getopt_letter s =
  let error err =
    error s ("invalid getopt-letter " ^ U.quote s ^ ": " ^ err)
  in
  (* NOTE: getopt-letter is a Piq word and, therefore, it can't be empty -- so
   * there's no need to check for that *)

  if String.length s > 1
  then error "must contain exactly one letter";

  match s.[0] with
    | 'a'..'z' | 'A'..'Z' -> ()
    | c -> error "must be lower- or upper-case alphabet letter"


let getopt_name_field x =
  let open Field in
  let letter = x.getopt_letter in
  match letter with
    | None -> ()
    | Some n ->
        check_getopt_letter n;
        x.piq_alias <- letter


let getopt_name_option x =
  let open Option in
  let letter = x.getopt_letter in
  match letter with
    | None -> ()
    | Some n ->
        check_getopt_letter n;
        x.piq_alias <- letter


(* name fields and options *)
let getopt_name_record x =
   List.iter getopt_name_field x.R.field

let getopt_name_variant x =
   List.iter getopt_name_option x.V.option

let getopt_name_enum x =
   List.iter getopt_name_option x.E.option

let getopt_name_typedef = function
  | `record x -> getopt_name_record x
  | `variant x -> getopt_name_variant x
  | `enum x -> getopt_name_enum x
  | _ -> ()


let getopt_name_defs defs =
    (* name fields and options *)
    List.iter getopt_name_typedef defs


let getopt_name_piqi _idtable (piqi:T.piqi) =
  let open P in
  getopt_name_defs piqi.resolved_typedef


(* NOTE: this function is called only in case if a getopt-related operation is
 * performed (e.g. "piqi getopt" or "piqi call". We don't need this startup
 * overhead otherwise *)
let init () =
  trace "init getopt\n";
  Piqi.register_processing_hook getopt_name_piqi


(**)


(* fake filename for error reporting *)
let getopt_filename = "argv"


let error s =
  (* using fake location here, the actual location (i.e. the index of the
   * argument) will be correctly provided by the exception handler below *)
  let loc = (0,0) in
  raise (Piq_lexer.Error (s, loc))


let parse_string_arg s =
  let lexbuf = Piq_lexer.init_from_string s in
  let token () =
    try
      Piq_lexer.token lexbuf
    with
      Piq_lexer.Error (err, _loc) -> error (err ^ ": " ^ s)
  in
  let res = token () in
  match res with
    | Piq_lexer.String _ ->
        (* there must be no other literal after the string *)
        if token() = Piq_lexer.EOF
        then res
        else
          (* s is alread quoted *)
          error ("trailing characters after string: " ^ s)
    | _ ->
        assert false (* something that starts with '"' have to be a string *)


let parse_word_arg s =
  if Piq_lexer.is_valid_word s
  then
    Piq_lexer.Word s
  else
    (* Raw binary -- just a sequence of bytes: may be parsed as binary or utf8
     * string *)
    Piq_lexer.Raw_binary s


let parse_name_arg s =
  (* cut the leading '-' and check if what we got is a valid Piq name *)
  let n = String.sub s 1 (String.length s - 1) in
  if Piqi_name.is_valid_name n ~allow:"."
  then (
    s.[0] <- '.'; (* replace '-' with '.' to turn it into a Piq name *)
    Piq_lexer.Word s
  )
  else error ("invalid name: " ^ U.quote s)


let read_file filename =
  let ch = open_in_bin filename in
  let len = in_channel_length ch in
  let buf = Buffer.create len in
  Buffer.add_channel buf ch len;
  close_in ch;
  Buffer.contents buf


let read_file filename =
  try read_file filename
  with Sys_error s ->
    error ("error reading file argument: " ^ s)


let parse_arg s =
  let len = String.length s in

  match s with
    (* NOTE: we don't support '(' and ')' and '[]' is handeled separately below *)
    | "[" -> Piq_lexer.Lbr
    | "]" -> Piq_lexer.Rbr
    | s when s.[0] = '"' -> parse_string_arg s
    | s when s.[0] = '@' ->
        let filename = String.sub s 1 (len - 1) in
        let content = read_file filename in
        (* Raw binary -- just a sequence of bytes: may be parsed as either
         * binary or utf8 string *)
        Piq_lexer.Raw_binary content

    (* parsing long options starting with "--"
     *
     * NOTE: it is safe to check s.[1] because a single '-' case is eliminated
     * in the calling function *)
    | s when s.[0] = '-' && s.[1] = '-' ->
        let name = String.sub s 1 (len - 1) in (* skip first '-' *)
        parse_name_arg name

    | s when s.[0] = '.' ->
        parse_name_arg s (* XXX: allowing Piq -style names *)

    (* XXX: support typenames and, possibly, other literals? *)
    | s ->
        parse_word_arg s


let parse_argv start =
  let error i err =
    C.error_at (getopt_filename, 0, i) err
  in
  let make_token i tok =
    (* 1-based token position in the argv starting from the position after "--" *)
    let loc = (0, i - start + 1) in
    (tok, loc)
  in
  let parse_make_arg i x =
    let tok =
      try parse_arg x
      with Piq_lexer.Error (err, _loc) -> error i err
    in
    make_token i tok
  in
  let parse_letter_args i s =
    let len = String.length s in
    let rec aux j =
      if j = len
      then [] (* end of string *)
      else
        let c = s.[j] in
        match c with
          (* only letters are allowed as single-letter options *)
          | 'a'..'z' | 'A'..'Z' ->
              (* creating Piq name: '.' followed by the letter *)
              let word = String.create 2 in
              word.[0] <- '.'; word.[1] <- c;
              let tok = Piq_lexer.Word word in
              (make_token i tok) :: (aux (j+1))
          | _ ->
              error i ("invalid single-letter argument: " ^ Char.escaped c)
    in
    aux 1 (* start at position 1 skipping the leading '-' *)
  in
  let len = Array.length Sys.argv in
  let rec aux i =
    if i >= len
    then [make_token i Piq_lexer.EOF]
    else
      let a = Sys.argv.(i) in
      match a with
        | "" ->
            error i "empty argument"

        | "-" | "--" ->
            error i ("invalid argument: " ^ a)

        | "[]" -> (* split it into two tokens '[' and ']' *)
            (parse_make_arg i "[") :: (parse_make_arg i "]") :: (aux (i+1))

        (* After skipping negative integers, and those arguments that start with
         * '--', we end up having '-' followed by one or more characters. We
         * treat those characters as single-letter arguments.
         *
         * NOTE: it is safe to check s.[1] because a single '-' case is
         * eliminated above *)
        | s when s.[0] = '-' && s.[1] <> '-' && (s.[1] < '0' || s.[1] > '9') ->
            (parse_letter_args i s) @ (aux (i+1))

        | s ->
            (parse_make_arg i s) :: (aux (i+1))
  in
  aux start


(* index of the "--" element in argv array *)
let argv_start_index = ref 0


(* find the position of the first argument after "--" *)
let rest_fun arg =
  if !argv_start_index = 0 (* first argument after first occurence of "--" *)
  then argv_start_index := !Arg.current + 1
  else ()


let arg__rest =
    "--", Arg.Rest rest_fun,
    "separator between piqi command-line arguments and data arguments"


let getopt_piq () :piq_ast list =
  let start =
    if !argv_start_index = 0 (* "--" is not present in the list of arguments *)
    then Array.length Sys.argv
    else !argv_start_index
  in
  let tokens = parse_argv start in
  let piq_parser = Piq_parser.init_from_token_list getopt_filename tokens in
  let piq_ast_list =
    U.with_bool Config.piq_relaxed_parsing true
    (fun () -> Piq_parser.read_all piq_parser)
  in
  piq_ast_list


let parse_args (piqtype: T.piqtype) (args: piq_ast list) :Piqobj.obj =
  let ast =
    match args with
      | [x] when not (C.is_container_type piqtype) ->  (* scalar type? *)
          x
      | l ->
          let res = `list l in
          (* set the location *)
          let loc = (getopt_filename, 0, 1) in
          Piqloc.addlocret loc res
  in
  let piqobj = U.with_bool Config.piq_relaxed_parsing true
    (fun () -> Piqobj_of_piq.parse_obj piqtype ast)
  in
  piqobj

