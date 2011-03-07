(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* spec: http://www.ecmascript.org/ and the ecma-262 document.
 *
 * See also http://en.wikipedia.org/wiki/ECMAScript
 *)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type pinfo = Parse_info.token
type info = Parse_info.info
and tok = info

(* a shortcut to annotate some information with token/position information *)
and 'a wrap = 'a * info

and 'a paren   = tok * 'a * tok
and 'a brace   = tok * 'a * tok
and 'a bracket = tok * 'a * tok 
and 'a comma_list = ('a, tok (* the comma *)) Common.either list

(* semicolon. Can be None when was implicitely inserted during parsing *)
and sc = tok option

 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Name. See also analyze_js/namespace_js.ml  *)
(* ------------------------------------------------------------------------- *)
type name = string wrap
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Type. For type analysis see type_js.ml  *)
(* ------------------------------------------------------------------------- *)

module Type_js = struct
    type jstype = unit (* todo *)
end

(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
type expr = exprbis * exp_info 
  (* semantic: *)
  and exp_info = { 
     mutable t: Type_js.jstype;
  }
 and exprbis = 
   | L of litteral
   | V of name
   | This of tok

   (* includes new/delete/... *)
   | U of unop wrap * expr

   | B of expr * binop wrap * expr

   | Bracket of expr * expr bracket
   | Period of expr * tok (* . *) * name

   | Object of field comma_list brace
   (* The comma_list can have successive Left because of "elison" *)
   | Array of expr comma_list bracket

   | Apply of expr * expr comma_list paren
   | Conditional of expr * tok (* ? *) * expr * tok (* : *) * expr

   (* bad language, should be in statements *)
   | Assign of expr * assignment_operator wrap * expr
   | Seq of expr * tok (* , *) * expr

   | Function of func_decl

   | Extra of extra
   (* unparser: *)
   | Paren of expr paren

  and extra = 
    | DanglingComma

  and litteral =
    | Bool of bool wrap
    | Num of string wrap
    (* todo?  | Float of float | Int of int32 *)

    | String of string wrap
    | Regexp of string wrap (* todo? should split the flags *)
    | Null of tok

    | Undefined (* ?? *)

  and unop =
    | U_new
    | U_delete

    | U_void
    | U_typeof

    | U_bitnot
    | U_pre_increment  | U_pre_decrement
    | U_post_increment | U_post_decrement
    | U_plus | U_minus | U_not

  and binop =
    | B_instanceof  | B_in

    | B_mul  | B_div  | B_mod  | B_add  | B_sub
    | B_le  | B_ge  | B_lt  | B_gt
    | B_lsr  | B_asr  | B_lsl
    | B_equal
    | B_notequal  | B_physequal  | B_physnotequal
    | B_bitand  | B_bitor  | B_bitxor
    | B_and  | B_or

  and property_name =
    | PN_String of string wrap
    | PN_Num of string (* todo? PN_Float of float | PN_Int of int32 *) wrap
    | PN_Empty (* ?? *)
        
  and assignment_operator =
    | A_eq
    | A_mul  | A_div  | A_mod  | A_add  | A_sub
    | A_lsl  | A_lsr  | A_asr
    | A_and  | A_xor  | A_or

 and field =
    (property_name * tok (* : *) * expr)
(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
and st =
  | Variable of tok (* var *) * variable_declaration comma_list * sc
  | Const of tok (* const *) * variable_declaration comma_list * sc

  | Block of toplevel list brace
  | Nop of sc
  | Expr of expr * sc

  | If of tok * expr paren * st * (tok (* else *) * st) option
  | Do of tok * st * tok (* while *) * expr paren * sc
  | While of tok * expr paren * st
  | For of tok * tok (* ( *) *
      lhs_or_var option * tok (* ; *) * 
      expr option * tok (* ; *) * 
      expr option * 
      tok (* ) *) *
      st
  | ForIn of tok * tok (* ( *) * lhs_or_var * tok (* in *) * 
      expr * tok (* ) *) * st
  | Switch of tok * expr paren *
      case_clause list brace (* was   (case_clause list * st) list *)

  | Continue of tok * label option * sc
  | Break of tok * label option * sc

  | Return of tok * expr option * sc

  | With of tok * expr paren * st
  | Labeled of label * tok (*:*) * st

  | Throw of tok * expr * sc
  | Try of tok * st (* always a block *) * 
      (tok * arg paren * st) option * (* catch *)
      (tok * st) option (* finally *)

and label = string wrap

and lhs_or_var =
  | LHS of expr
  | Vars of tok * variable_declaration comma_list

and case_clause = 
  | Default of tok * tok (*:*) * toplevel list 
  | Case of tok * expr * tok (*:*) * toplevel list

and arg = string wrap

(* ------------------------------------------------------------------------- *)
(* Function definition *)
(* ------------------------------------------------------------------------- *)
and func_decl = tok * name option * name comma_list paren * toplevel list brace

(* ------------------------------------------------------------------------- *)
(* Variables definition *)
(* ------------------------------------------------------------------------- *)

and variable_declaration = name * (tok (*=*) * expr) option

(* ------------------------------------------------------------------------- *)
(* The toplevels elements *)
(* ------------------------------------------------------------------------- *)

and toplevel =
  | St of st
  | FunDecl of func_decl

  | NotParsedCorrectly of info list
  | FinalDef of info (* EOF *)

 and program = toplevel list
 (* with tarzan *)

(*****************************************************************************)
(* Comments *)
(*****************************************************************************)

(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)

let fakeInfo () = 
  { PI.
    token = PI.FakeTokStr ("FAKE", None);
    transfo = PI.NoTransfo;
    comments = ();
  }

let noType () = 
  { t = ();
  }

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

let unwrap = fst

let unparen (a,b,c) = b
let unbrace = unparen
let unbracket = unparen

let uncomma xs = Common.map_filter (function
  | Left e -> Some e
  | Right info -> None
  ) xs

let map_paren f (lp, x, rp) = (lp, f x, rp)
let map_comma_list f xs = List.map (fun x ->
  match x with
  | Left e -> Left (f e)
  | Right tok -> Right tok
  )
  xs

let untype (e, xinfo) = e

let parse_info_of_info = PI.parse_info_of_info
(* todo: return a Real | Virt position ? *)
let pos_of_info  = PI.pos_of_info
let str_of_info  = PI.str_of_info
let file_of_info = PI.file_of_info
let line_of_info = PI.line_of_info
let col_of_info  = PI.col_of_info

let pinfo_of_info = PI.pinfo_of_info

let rewrap_str =  PI.rewrap_str
(*
let rewrap_parse_info pi ii =  
  {ii with pinfo =
    (match ii.pinfo with
    | OriginTok _oldpi -> OriginTok pi
    | FakeTokStr _  | Ab | ExpandedTok _ -> 
        failwith "rewrap_parseinfo: no OriginTok"
    )
  }
*)

(* for error reporting *) 
let string_of_info ii = 
  Parse_info.string_of_parse_info (parse_info_of_info ii)

let is_origintok = Parse_info.is_origintok

let info_of_name (s, info) = info



let get_type (e: expr) = (snd e).t
let set_type (e: expr) (ty: Type_js.jstype) = 
  (snd e).t <- ty

(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* When we have extended the AST to add some info about the tokens,
 * such as its line number in the file, we can not use anymore the
 * ocaml '=' to compare Ast elements. To overcome this problem, to be
 * able to use again '=', we just have to get rid of all those extra
 * information, to "abstract those line" (al) information.
 *)

let al_info x = 
  { x with PI.token = PI.Ab }

(*****************************************************************************)
(* Views *)
(*****************************************************************************)

(* examples: 
 * inline more static funcall in expr type or variable type
 * 
 *)

(*****************************************************************************)
(* Helpers, could also be put in lib_parsing.ml instead *)
(*****************************************************************************)

let fakeInfoAttach info = 
  let info = rewrap_str "FAKE" info in
  let pinfo = parse_info_of_info info in
  { PI.
    token = PI.FakeTokStr ("FAKE", Some (pinfo, -1));
    transfo = PI.NoTransfo;
    comments = ();
  }

let remove_quotes_if_present s =

  (* for JX the entity names are passed as strings when
   * defining the entity (e.g. JX.Install('Typeahead'. ...)
   * but use normally (e.g. var x = JX.Typeahead(...))
   * so here we normalize.
   *)
  match s with
  | _ when s =~ "'\\(.*\\)'$" -> Common.matched1 s
  | _ when s =~ "\"\\(.*\\)\"$" -> Common.matched1 s
  | _ -> s
