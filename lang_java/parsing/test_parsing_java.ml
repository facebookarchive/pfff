(* Copyright (C) 2008 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Parser_java

let score_path = "/home/pad/c-yacfe/tmp"
let tmpfile = "/tmp/output.java" 

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse xs  =
  let ext = "java" in

  let fullxs = Common.files_of_dir_or_files_no_vcs ext xs in

  let stat_list = ref [] in
  let newscore  = Common.empty_score () in

  Common.check_stack_nbfiles (List.length fullxs);

  fullxs +> List.iter (fun file -> 

    pr2 "";
    pr2 ("PARSING: " ^ file);

    let (xs, stat) = Parse_java.parse_java file in

    Common.push2 stat stat_list;
    let s = sprintf "bad = %d" stat.Parse_java.bad in
    if stat.Parse_java.bad = 0
    then Hashtbl.add newscore file (Common.Ok)
    else Hashtbl.add newscore file (Common.Pb s)
    ;
  );
  flush stdout; flush stderr;

  Parse_java.print_parsing_stat_list !stat_list;

  let dirname_opt = 
    match xs with
    | [x] when is_directory x -> Some x
    | _ -> None
  in
  dirname_opt +> Common.do_option (fun dirname -> 
    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    let str = Str.global_replace (Str.regexp "/") "__" dirname in
    Common.regression_testing newscore 
      (Filename.concat score_path
       ("score_parsing__" ^str ^ ext ^ ".marshalled"))
  );
  ()




(* ---------------------------------------------------------------------- *)
let to_string = function
  | TUnknown i -> "UNKNOWN"
  | TComment i -> "COMMENT"
  | TCommentSpace i -> "SPACE"


  | IDENTIFIER (id,ii) -> "Identifier " ^ id
  | PRIMITIVE_TYPE (s, ii) -> "PrimitiveType " ^ s
  | LITERAL (s, ii) -> "Literal " ^ s

  (* 3.11 Separators *)
  | LP ii -> "("
  | RP ii -> ")"
  | LC ii -> "{"
  | RC ii -> "}"
  | LB ii -> "["
  | RB ii -> "]"
  | SM ii -> ";"
  | CM ii -> ","
  | DOT ii -> "."

  (* 3.12 Operators *)
  | EQ ii -> "="
  | GT ii -> ">"
  | LT ii -> "<"
  | NOT ii -> "!"
  | COMPL ii -> "~"
  | COND ii -> "?"
  | COLON ii -> ":"
  | EQ_EQ ii -> "=="
  | LE ii -> "<="
  | GE ii -> ">="
  | NOT_EQ ii -> "!="
  | AND_AND ii -> "&&"
  | OR_OR ii -> "||"
  | INCR ii -> "++"
  | DECR ii -> "--"
  | PLUS ii -> "+"
  | MINUS ii -> "-"
  | TIMES ii -> "*"
  | DIV ii -> "/"
  | AND ii -> "&"
  | OR ii -> "|"
  | XOR ii -> "^"
  | MOD ii -> "%"
  | LS ii -> "<<"
  | SRS ii -> ">>"
  | URS ii -> ">>>"
  | OPERATOR_EQ (op,ii) -> op

  | ABSTRACT ii -> "abstract"
  | BOOLEAN ii -> "boolean"
  | BREAK ii -> "break"
  | BYTE ii -> "byte"
  | CASE ii -> "case"
  | CATCH ii -> "catch"
  | CHAR ii -> "char"
  | CLASS ii -> "class"
  | CONST ii -> "const"
  | CONTINUE ii -> "continue"
  | DEFAULT ii -> "default"
  | DO ii -> "do"
  | DOUBLE ii -> "double"
  | ELSE ii -> "else"
  | EXTENDS ii -> "extends"
  | FINAL ii -> "final"
  | FINALLY ii -> "finally"
  | FLOAT ii -> "float"
  | FOR ii -> "for"
  | GOTO ii -> "goto"
  | IF ii -> "if"
  | IMPLEMENTS ii -> "implements"
  | IMPORT ii -> "import"
  | INSTANCEOF ii -> "instanceof"
  | INT ii -> "int"
  | INTERFACE ii -> "interface"
  | LONG ii -> "long"
  | NATIVE ii -> "native"
  | NEW ii -> "new"
  | PACKAGE ii -> "package"
  | PRIVATE ii -> "private"
  | PROTECTED ii -> "protected"
  | PUBLIC ii -> "public"
  | RETURN ii -> "return"
  | SHORT ii -> "short"
  | STATIC ii -> "static"
  | STRICTFP ii -> "strictfp"
  | SUPER ii -> "super"
  | SWITCH ii -> "switch"
  | SYNCHRONIZED ii -> "synchronized"
  | THIS ii -> "this"
  | THROW ii -> "throw"
  | THROWS ii -> "throws"
  | TRANSIENT ii -> "transient"
  | TRY ii -> "try"
  | VOID ii -> "void"
  | VOLATILE ii -> "volatile"
  | WHILE ii -> "while"

  | ASSERT ii -> "assert"

  | EOF ii -> "EOF"

let test_lexer file =
  let lexbuf = Lexing.from_channel (open_in file) in
  while true do
    let result = Lexer_java.token lexbuf in
    print_endline (to_string result);
    if Token_helpers_java.is_eof result then
      exit 0
  done


(* ---------------------------------------------------------------------- *)

let test_visitor file = 
  let bigf = { Visitor_java.default_visitor_s with
    Visitor_java.kexpr_s = (fun (k, bigf) e -> 
      match Ast_java.unwrap e with
      | Ast_java.Literal s -> 
          pr2 ("lit:" ^ s);
          k e
      | Ast_java.Dot (e, s) -> 
          pr2 "dot: s";
          k e
      | _ -> k e
    );
  } in

  let ((xs,info_item), stat) = Parse_java.parse_java file in
  match xs with
  | Left cu -> Visitor_java.compilation_unit bigf cu +> ignore
  | Right _ -> pr2 "error parsing"

  



(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_java", "   <file>", 
  Common.mk_action_1_arg test_lexer;
  "-parse_java", "   <file or dir>", 
  Common.mk_action_n_arg test_parse;
  "-visitor_java", "   <file>", 
  Common.mk_action_1_arg test_visitor;
]
