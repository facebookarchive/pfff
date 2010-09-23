type token =
  | STRING of (string)
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | OBJSTART
  | OBJEND
  | ARSTART
  | AREND
  | NULL
  | COMMA
  | COLON
  | EOF

open Parsing;;
# 2 "json_parser.mly"
  (* 
     Notes about error messages and error locations in ocamlyacc:
     
     1) There is a predefined "error" symbol which can be used as a catch-all,
     in order to get the location of the token that shouldn't be there.
     
     2) Additional rules that match common errors are added, so that when
     they are matched, a nice, handcrafted error message is produced.
     
     3) Token locations are retrieved using functions from the Parsing
     module, which relies on a global state. If you want your error locations
     to be reliable, don't run two ocamlyacc parsers simultaneously.
     

     In the end, the error messages are nicer than the ones that a camlp4
     parser (extensible grammar) would produce because we write them
     manually. However camlp4's messages are all automatic, 
     i.e. they tell you which tokens were expected at a given location.

     For the file/line/char locations to be correct, 
     the lexbuf must be adjusted by the lexer when the file name
     changes or a new line is encountered. This is not performed automatically
     by ocamllex, see file json_lexer.mll.
  *)

  open Printf
  open Json_type
			
  let rhs_loc n = (Parsing.rhs_start_pos n, Parsing.rhs_end_pos n)
		    
  let unclosed opening_name opening_num closing_name closing_num =
    let msg = 
      sprintf "%s:\nSyntax error: '%s' expected.\n\
               %s:\nThis '%s' might be unmatched."
	(string_of_loc (rhs_loc closing_num)) closing_name
	(string_of_loc (rhs_loc opening_num)) opening_name in
    json_error msg

  let syntax_error s num =
    let msg = sprintf "%s:\n%s" (string_of_loc (rhs_loc num)) s in
    json_error msg

# 60 "json_parser.ml"
let yytransl_const = [|
  261 (* OBJSTART *);
  262 (* OBJEND *);
  263 (* ARSTART *);
  264 (* AREND *);
  265 (* NULL *);
  266 (* COMMA *);
  267 (* COLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* STRING *);
  258 (* INT *);
  259 (* FLOAT *);
  260 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\004\000\
\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\001\000\003\000\002\000\003\000\003\000\
\002\000\003\000\002\000\003\000\003\000\002\000\001\000\001\000\
\001\000\001\000\001\000\005\000\005\000\004\000\003\000\003\000\
\003\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\004\000\015\000\018\000\019\000\016\000\000\000\
\000\000\017\000\003\000\028\000\000\000\009\000\000\000\006\000\
\000\000\014\000\011\000\000\000\000\000\002\000\001\000\000\000\
\008\000\005\000\007\000\000\000\026\000\013\000\010\000\012\000\
\000\000\025\000\024\000\022\000\000\000\021\000\020\000"

let yydgoto = "\002\000\
\012\000\020\000\017\000\021\000"

let yysindex = "\003\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\002\255\
\024\255\000\000\000\000\000\000\011\000\000\000\251\254\000\000\
\012\000\000\000\000\000\033\255\007\000\000\000\000\000\052\255\
\000\000\000\000\000\000\043\255\000\000\000\000\000\000\000\000\
\004\255\000\000\000\000\000\000\009\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\235\255\245\255"

let yytablesize = 275
let yytable = "\013\000\
\011\000\014\000\015\000\001\000\036\000\024\000\032\000\016\000\
\027\000\015\000\023\000\027\000\023\000\037\000\038\000\039\000\
\035\000\000\000\029\000\000\000\000\000\000\000\033\000\018\000\
\004\000\005\000\006\000\007\000\008\000\000\000\009\000\019\000\
\010\000\004\000\005\000\006\000\007\000\008\000\000\000\009\000\
\000\000\010\000\028\000\004\000\005\000\006\000\007\000\008\000\
\000\000\009\000\034\000\010\000\004\000\005\000\006\000\007\000\
\008\000\000\000\009\000\000\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\000\004\000\005\000\006\000\007\000\008\000\030\000\009\000\
\027\000\010\000\022\000\025\000\023\000\000\000\031\000\000\000\
\027\000\026\000\023\000"

let yycheck = "\001\000\
\000\000\000\001\001\001\001\000\001\001\011\001\000\000\006\001\
\000\000\001\001\000\000\000\000\000\000\010\001\006\001\037\000\
\028\000\255\255\020\000\255\255\255\255\255\255\024\000\000\001\
\001\001\002\001\003\001\004\001\005\001\255\255\007\001\008\001\
\009\001\001\001\002\001\003\001\004\001\005\001\255\255\007\001\
\255\255\009\001\010\001\001\001\002\001\003\001\004\001\005\001\
\255\255\007\001\008\001\009\001\001\001\002\001\003\001\004\001\
\005\001\255\255\007\001\255\255\009\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\001\001\002\001\003\001\004\001\005\001\000\001\007\001\
\000\001\009\001\000\001\000\001\000\001\255\255\008\001\255\255\
\008\001\006\001\006\001"

let yynames_const = "\
  OBJSTART\000\
  OBJEND\000\
  ARSTART\000\
  AREND\000\
  NULL\000\
  COMMA\000\
  COLON\000\
  EOF\000\
  "

let yynames_block = "\
  STRING\000\
  INT\000\
  FLOAT\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    Obj.repr(
# 55 "json_parser.mly"
               ( _1 )
# 218 "json_parser.ml"
               : Json_type.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    Obj.repr(
# 56 "json_parser.mly"
               ( syntax_error "Junk after end of data" 2 )
# 225 "json_parser.ml"
               : Json_type.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "json_parser.mly"
               ( syntax_error "Empty data" 1 )
# 231 "json_parser.ml"
               : Json_type.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "json_parser.mly"
               ( syntax_error "Syntax error" 1 )
# 237 "json_parser.ml"
               : Json_type.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pair_list) in
    Obj.repr(
# 61 "json_parser.mly"
                            ( Object _2 )
# 244 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "json_parser.mly"
                            ( Object [] )
# 250 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pair_list) in
    Obj.repr(
# 63 "json_parser.mly"
                            ( unclosed "{" 1 "}" 3 )
# 257 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pair_list) in
    Obj.repr(
# 64 "json_parser.mly"
                            ( unclosed "{" 1 "}" 3 )
# 264 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "json_parser.mly"
                            ( syntax_error 
				"Expecting a comma-separated sequence \
                                 of string:value pairs" 2 )
# 272 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 68 "json_parser.mly"
                            ( Array _2 )
# 279 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "json_parser.mly"
                            ( Array [] )
# 285 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 70 "json_parser.mly"
                            ( unclosed "[" 1 "]" 3 )
# 292 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'value_list) in
    Obj.repr(
# 71 "json_parser.mly"
                            ( unclosed "[" 1 "]" 3 )
# 299 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "json_parser.mly"
                            ( syntax_error 
				"Expecting a comma-separated sequence \
                                 of values" 2 )
# 307 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "json_parser.mly"
         ( String _1 )
# 314 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 76 "json_parser.mly"
       ( Bool _1 )
# 321 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "json_parser.mly"
       ( Null )
# 327 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "json_parser.mly"
       ( Int _1 )
# 334 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 79 "json_parser.mly"
        ( Float _1 )
# 341 "json_parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'pair_list) in
    Obj.repr(
# 82 "json_parser.mly"
                                     ( (_1, _3) :: _5 )
# 350 "json_parser.ml"
               : 'pair_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    Obj.repr(
# 84 "json_parser.mly"
                             ( syntax_error 
				"End-of-object commas are illegal" 4 )
# 359 "json_parser.ml"
               : 'pair_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "json_parser.mly"
                             ( syntax_error "Missing ','" 4 )
# 368 "json_parser.ml"
               : 'pair_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 87 "json_parser.mly"
                             ( [ (_1, _3) ] )
# 376 "json_parser.ml"
               : 'pair_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value_list) in
    Obj.repr(
# 90 "json_parser.mly"
                         ( _1 :: _3 )
# 384 "json_parser.ml"
               : 'value_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    Obj.repr(
# 91 "json_parser.mly"
                         ( syntax_error 
			     "End-of-array commas are illegal" 2 )
# 392 "json_parser.ml"
               : 'value_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'value) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 93 "json_parser.mly"
                         ( syntax_error "Missing ',' before this value" 2 )
# 400 "json_parser.ml"
               : 'value_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 94 "json_parser.mly"
                         ( [ _1 ] )
# 407 "json_parser.ml"
               : 'value_list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Json_type.t)
