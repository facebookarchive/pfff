(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

open Ast_clang
open Parser_clang

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo: this is not super elegant, hacking parsing ... 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* todo: look at ast_cpp.ml? *)
type type_clang = 
  | Builtin of string
  | Typename of string

  (* pointer or array *)
  | Pointer of type_clang
  | Function of type_clang (* TODO and params? analyze Param *)

  | StructName of string
  | UnionName of string
  | EnumName of string

  | AnonStuff
  | TypeofStuff

  | Other of Parser_clang.token list

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let builtin_types = Common.hashset_of_list [
  "char";
  "int";"short";"long";
  "float";"double";
  "void";
  (*"unsigned";"signed"; *)
  (* "const";"restrict";"volatile"; *)
  (*"noreturn";"__attribute__";*)

  (* clang *)
  "__int128";
  "__va_list_tag";
  (* todo: ugly, because of stdbool.h skip? but now that use ExpansionLoc,
   * don't need that anymore? apparently still need it :(
   *)
  "_Bool";

  (* otherwise get wierd edges from EXTERNAL to the source. e.g. in byacc *)
  "__builtin_va_list";
]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo? could use parse_cpp.type_of_string? hmm but clang uses some
 * special syntax for anon struct or typeof.
 *)
let extract_type_of_tokens loc xs =
  let rec aux xs =
    match xs with
    | [] -> Errors_clang.error loc "empty type string?"
    (* todo: anonymous struct? enum? parse the pathname?
     * or just look at preceding type def before the VarDecl,
     * probably the anon struct
     *)
    | TLowerIdent"struct"::TInf _::TLowerIdent "anonymous"::_rest ->
        AnonStuff
    | TLowerIdent"union"::TInf _::TLowerIdent "anonymous"::_rest ->
        AnonStuff
    | TLowerIdent"enum"::TInf _::TLowerIdent "anonymous"::_rest ->
        AnonStuff
          
    | TLowerIdent "struct"::(TLowerIdent s | TUpperIdent s)::rest ->
        aux2 (StructName s) rest
    | TLowerIdent "union"::(TLowerIdent s | TUpperIdent s)::rest ->
        aux2 (UnionName s) rest
    | TLowerIdent "enum"::(TLowerIdent s | TUpperIdent s)::rest ->
        aux2 (EnumName s) rest

    (* todo: see fixDeclSpecForDecl in lang_cpp/parsing *)
    | TLowerIdent ("unsigned" | "signed")::rest ->
        aux rest
    | TLowerIdent "long"::TLowerIdent "long"::rest ->
        aux2 (Builtin "longlong") rest
    | TLowerIdent "long"::TLowerIdent "double"::rest ->
        aux2 (Builtin "longdouble") rest
    | TLowerIdent ("const" | "volatile" | "restrict")::rest ->
        aux rest
     (* todo: sparse has such code, why clang does not unsugar?
      * it does in 'a':'b' 'b' is unsugared!
      *)
    | TLowerIdent "typeof"::_rest ->
        TypeofStuff
          
    | (TLowerIdent s | TUpperIdent s)::rest ->
        aux2 (if Hashtbl.mem builtin_types s
        then Builtin s
        else Typename s
        ) rest
    | x::_xs ->
        Errors_clang.error loc (spf "unhandled type prefix: %s" (Common.dump x))

  and aux2 acc = function
    | [] -> acc
    | TOBracket _::rest ->
        skip_until_closing_bracket acc rest
    (* todo: analyze params? for type deps, analyze the Param in the 
     * mean time
     *)
    | TOPar _::_rest ->
        Function acc
    | TLowerIdent ("const" | "volatile")::rest -> aux2 acc rest
    (* todo: can have 'union Sym::<anonymous at /Users/yoann/...' in tiny-cc 
     * or struct header::<anonymous in umalloc.c
     *)
    | TColon::TColon::_rest ->
        AnonStuff
    | TStar::rest ->
        aux2 (Pointer acc) rest
    | x::_xs -> 
        Errors_clang.error loc (spf "unhandled type suffix: %s" (Common.dump x))

  and skip_until_closing_bracket acc = function
    | [] -> acc
    | TCBracket::xs -> aux2 (Pointer acc) xs
    | _x::xs -> skip_until_closing_bracket acc xs

  in  
  aux xs


let extract_canonical_type_of_sexp loc sexp =
  match sexp with
  | Paren (_enum, _l, xs) ->
      (match xs with
      | _loc::Brace (toks, _toks_opt)::_rest ->
          extract_type_of_tokens loc toks
      | _loc::(T (TString _s))::_rest ->
          failwith "use old AST dumper format, apply latest patch"
      | _ -> Errors_clang.error loc "didn't find type"
      )
  | _ -> Errors_clang.error loc "not a paren exp"
