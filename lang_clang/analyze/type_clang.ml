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
  | Function of type_clang (* todo: and params? analyze Param *)

  | StructName of string
  | UnionName of string
  | EnumName of string

  | AnonStuff
  | TypeofStuff

  | Other of Parser_clang.token list

(*****************************************************************************)
(* Type -> string *)
(*****************************************************************************)

(* used for prolog type/2 *)
let rec string_of_type_clang t =
  match t with
  | Builtin s | Typename s -> s
  | Pointer t -> spf "pointer(%s)" (string_of_type_clang t)
  | Function t -> spf "function(%s)" (string_of_type_clang t)
  | StructName s | UnionName s | EnumName s -> s

  | AnonStuff -> "_anon_"
  | TypeofStuff -> "_typeof_"
  | Other _ -> "_other_"

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let builtin_types = Common.hashset_of_list [
  "void";
  "char";
  "int"; "short"; "long";
  "float"; "double";
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
(* Typedefs *)
(*****************************************************************************)

let rec expand_typedefs typedefs t =
  match t with
  | Builtin _ 
  | StructName _ | UnionName _ | EnumName _ 
  | TypeofStuff | AnonStuff
  | Other _
    -> t

  | Typename s ->
      if Hashtbl.mem typedefs s
      then 
        let t' = (Hashtbl.find typedefs s) in
        (* right now 'typedef enum { ... } X' results in X being
         * typedefed to ... itself
         *)
        if t' =*= t
        then t
        else expand_typedefs typedefs t'
      else t

  | Pointer x -> Pointer (expand_typedefs typedefs x)
   (* todo: should analyze parameters *)
  | Function x -> Function (expand_typedefs typedefs x)

(*****************************************************************************)
(* Sexp -> tokens *)
(*****************************************************************************)

(* todo: actually clang does some typedef expansion just at depth 0,
 * if you have a 'X *', it will not expand 'X' so maybe simpler to
 * always return toks_origin here (especially since with 
 * 'typedef enum { ... } X' it returns the wrong thing).
 *)
let tokens_of_brace_sexp typedefs_dependencies loc typ = 
  match typ with
  (* In a codegraph context, do we want to use the original type or the
   * final type? It depends if we want to create dependencies to typedefs.
   * In the plan9 context where typedefs are used for forward declaring
   * it's better to look at the final type.
   *)
  | Brace (toks_origin, toks_after_typedef_expansion) ->
      if typedefs_dependencies
      then toks_origin
      else toks_after_typedef_expansion ||| toks_origin
  | T (TString _s) ->
      failwith "you're using an old version of the AST dumper, apply patch"
  | _ ->
      Errors_clang.error loc "wrong type format"

let tokens_of_paren_sexp loc sexp =
  match sexp with
  | Paren (_enum, _l, xs) ->
      (match xs with
      | _loc::typ::_rest ->
       (* this function is used mostly to get the type of a field access, so
        * we do the typedef expansion
        *)
        tokens_of_brace_sexp true loc typ
      | _ -> Errors_clang.error loc "didn't find type"
      )
  | _ -> Errors_clang.error loc "not a paren exp"


(*****************************************************************************)
(* Tokens -> type *)
(*****************************************************************************)

(* todo? could use parse_cpp.type_of_string? hmm but clang uses some
 * special syntax for anon struct or typeof.
 *)
let type_of_tokens loc xs =
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
        aux2 (if Hashtbl.mem builtin_types s then Builtin s else Typename s) rest
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
