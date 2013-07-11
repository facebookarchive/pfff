open Common
open Ast_php

module PI=Parse_info
module V=Visitor_php

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The expected arguments after a format string could be either type
 * string, or type integer. A list of expected_argument will be
 * construced during reading the format string, and later used for adding
 * missing arguments.
 *)
type expected_argument = PercentD | PercentS

exception No_format_string
exception Bad_format_string
exception Need_more_argument of expected_argument list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* http://www.php.net/manual/en/function.sprintf.php : type specifier*)
let is_valid_integer_specifier char =
  match char with
  | 'b'| 'c'| 'd'| 'e'| 'E'
  | 'u'| 'f'| 'F'| 'g'| 'G' 
  | 'o'| 'x'| 'X'
    -> true
  | _ -> false

(* An automata that reads a string, and output expected argument/type
   infomation *)
let rec start_state (acc : expected_argument list)
    (string_in_char_list : char list) : expected_argument list =
  match string_in_char_list with
  | [] -> List.rev acc
  | '\\'::t -> in_escape_state acc t
  | '%'::t -> in_percent_state acc t
  | _::t -> start_state acc t

and in_percent_state acc string_in_char_list =
  match string_in_char_list with
  | [] -> raise Bad_format_string
  | '%'::t -> start_state acc t
  | 's'::t -> start_state (PercentS::acc) t
  | c::t when is_valid_integer_specifier c -> start_state (PercentD::acc) t
  | _::t -> in_percent_state acc t
    
and in_escape_state acc string_in_char_list =
  match string_in_char_list with
  | [] -> raise Bad_format_string
  | _::t -> start_state acc t

let delete_argument arg =  
  match arg with
  | Left a ->
    let ii = Lib_parsing_php.ii_of_any (Argument a) in
    ii +> List.iter (fun info ->
      info.PI.transfo <- PI.Remove
    )      
  | Right tok ->
    tok.PI.transfo <- PI.Remove

(* Convert an expected_argument list to a string.
 * The string will be added before the right parenthese of this
 * function call to make format string and aguments match each other.
 *)
let rec expect_to_string (expect : expected_argument list) : string =
  match expect with
  | [] -> ""
  | PercentS::t -> ", \'\'" ^ expect_to_string t
  | PercentD::t -> ", 0" ^ expect_to_string t

let add_argument expect tok =
  let expect_str = expect_to_string expect in
  tok.PI.transfo <- PI.AddBefore (PI.AddStr expect_str)

(* Try to fix args, knowing the expected argument *)
let rec adjust_argument expect args =
  match (expect, args) with
  | ([], []) -> ()
  (* e = expect, a = args, h = head, t = tail, r = right, l = left *)
  | (eh::et, ar::al::at) -> adjust_argument et at
  | ([], _) -> args +> List.iter delete_argument
  | (_, []) -> raise (Need_more_argument expect)
  | _ -> failwith "bad argument for adjust_argument"

let fix_format_string args =
  match args with
  | [] -> raise No_format_string
  | Left(Arg(Sc(C(String((h, _))))))::t ->
    let char_list = Common2.list_of_string h in
    let expect = start_state [] char_list in
    adjust_argument expect t
  | _ -> failwith "bad argument for fix_format_string"

(* skip first n arguments *)      
let rec fix_format_string_skip_first_n n args =
  match (n, args) with
  | (n, _) when (n < 0) -> failwith "bad argument for fix_format_string_skip_first_n"
  | (0, args) -> fix_format_string args
  | (1, [l]) -> raise No_format_string
  | (n, l::r::t) -> fix_format_string_skip_first_n (n-1) t
  | _ -> failwith "bad argument for fix_format_string_skip_first_n"

let try_auto_fix error =
  match error.Error_php.typ with
  | Error_php.FormatStringMismatch func_name ->
    let file = PI.file_of_info error.Error_php.loc in
    pr2 (spf "%s: Function %s" file func_name);
    let (ast, toks) = Parse_php.ast_and_tokens file in
    let visitor = V.mk_visitor { V.default_visitor with
      V.kexpr = (fun (k,_) e ->
        (match e with
        | Call(Id(XName[QI(Name((func_name, _)))]), (_ , args, right_tok)) ->
          (try
             let n = List.assoc func_name Check_misc_php.printf_like_functions_list in
             fix_format_string_skip_first_n n args
           with 
           | Not_found -> pr2 (spf "Cannot associate function name %s" func_name)
           | Bad_format_string ->
             pr2 (spf "Bad format string: %s:%d (%s)"
                    file (PI.line_of_info error.Error_php.loc) func_name)
           | No_format_string ->
             right_tok.PI.transfo <- PI.AddBefore (PI.AddStr ", \'\'")
           | Need_more_argument expect ->
             add_argument expect right_tok
          )
        | _ -> ()
        );
        k e
      );
    }
    in
    visitor (Program ast);
    let s = Unparse_php.string_of_program_with_comments_using_transfo 
      (ast, toks) in    
    let tmpfile = new_temp_file "trans" ".php" in
    write_file ~file:tmpfile s;
    let diff = Common2.unix_diff file tmpfile in
    diff +> List.iter pr2;
  | _ -> ()
    
