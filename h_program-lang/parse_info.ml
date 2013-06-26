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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*
 * Currently core/lexing.ml does not handle the line number position.
 * Even if there are certain fields in the lexing structure, they are not
 * maintained by the lexing engine so the following code does not work:
 *
 *   let pos = Lexing.lexeme_end_p lexbuf in
 *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
 *      (pos.pos_cnum - pos.pos_bol) in
 *
 * Hence those functions to overcome the previous limitation.
 *)
type token_location = {
    str: string;
    charpos: int;

    line: int;
    column: int;
    file: filename;
  }
  (* with tarzan *)

let fake_token_location = {
  charpos = -1; str = ""; line = -1; column = -1; file = "";
}


(*
 * The token type below is used to represent the leaves of ASTs, the token.
 *
 * To be perfectly correct this is not really a token as a token usually
 * also have a category, e.g. TNumber or TIdent, but this would
 * be specific to a programming language and lexer, which is
 * what we try to avoid here.
 *)
type token_origin =
    (* Present both in the AST and list of tokens *)
    | OriginTok  of token_location

    (* Present only in the AST and generated after parsing. Can be used
     * when building some extra AST elements. *)
    | FakeTokStr of string (* to help the generic pretty printer *) *
        (* Sometimes we generate fake tokens close to existing
         * origin tokens. This can be useful when have to give an error
         * message that involves a fakeToken. The int is a kind of
         * virtual position, an offset. See compare_pos below.
         *)
        (token_location * int) option

    (* In the case of a XHP file, we could preprocess it and incorporate
     * the tokens of the preprocessed code with the tokens from
     * the original file. We want to mark those "expanded" tokens
     * with a special tag so that if someone do some transformation on
     * those expanded tokens they will get a warning (because we may have
     * trouble back-propagating the transformation back to the original file).
     *)
    | ExpandedTok of
        (* refers to the preprocessed file, e.g. /tmp/pp-xxxx.pphp *)
        token_location  *
       (* kind of virtual position. This info refers to the last token
        * before a serie of expanded tokens and the int is an offset.
        * The goal is to be able to compare the position of tokens
        * between then, even for expanded tokens. See compare_pos
        * below.
        *)
        token_location * int

    (* The Ab constructor is (ab)used to call '=' to compare
     * big AST portions. Indeed as we keep the token information in the AST,
     * if we have an expression in the code like "1+1" and want to test if
     * it's equal to another code like "1+1" located elsewhere, then
     * the Pervasives.'=' of OCaml will not return true because
     * when it recursively goes down to compare the leaf of the AST, that is
     * the token_location, there will be some differences of positions. If instead
     * all leaves use Ab, then there is no position information and we can
     * use '='. See also the 'al_info' function below.
     *
     * Ab means AbstractLineTok. I Use a short name to not
     * polluate in debug mode.
     *)
    | Ab

   (* with tarzan *)

type token_mutable = {
  (* contains among other things the position of the token through
   * the token_location embedded inside the token_origin type.
   *)
  token : token_origin;
  mutable transfo: transformation;
  mutable comments: unit; (* TODO *)
}

(* poor's man refactoring *)
and transformation =
  | NoTransfo
  | Remove
  | AddBefore of add
  | AddAfter of add
  | Replace of add

  and add =
    | AddStr of string
    | AddNewlineAndIdent

 (* with tarzan *)

(* shortcut *)
type info = token_mutable


let info_start_file file = {
  token = OriginTok {
      str = "";
      charpos = 1;
      line = 1;
      column = 1;
      file = file;
  };
  comments = ();
  transfo = NoTransfo;
}


type parsing_stat = {
  filename: Common.filename;
  mutable correct: int;
  mutable bad: int;
}
let default_stat file =  {
    filename = file;
    correct = 0; bad = 0;
(*
    have_timeout = false;
    commentized = 0;
    problematic_lines = [];
*)
}


(* Many parsers need to interact with the lexer, or use tricks around
 * the stream of tokens, or do some error recovery, or just need to
 * pass certain tokens (like the comments token) which requires
 * to have access to this stream of remaining tokens.
 * The token_state type helps.
 *)
type 'tok tokens_state = {
  mutable rest:         'tok list;
  mutable current:      'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed:       'tok list;
  (* if want to do some lalr(k) hacking ... cf yacfe.
   * mutable passed_clean : 'tok list;
   * mutable rest_clean :   'tok list;
   *)
}
let mk_tokens_state toks = {
    rest       = toks;
    current    = (List.hd toks);
    passed = [];
    (* passed_clean = [];
     * rest_clean = (toks +> List.filter TH.is_not_comment);
     *)
  }

(*****************************************************************************)
(* Lexer helpers *)
(*****************************************************************************)

let tokinfo_str_pos str pos =
  {
    token = OriginTok {
      charpos = pos;
      str     = str;

      (* info filled in a post-lexing phase, cf Parse_php.tokens *)
      line = -1;
      column = -1;
      file = "";
    };
    comments = ();
    transfo = NoTransfo;
  }


let rewrap_str s ii =
  {ii with token =
    (match ii.token with
    | OriginTok pi -> OriginTok { pi with str = s;}
    | FakeTokStr (s, info) -> FakeTokStr (s, info)
    | Ab -> Ab
    | ExpandedTok _ ->
        (* ExpandedTok ({ pi with Common.str = s;},vpi) *)
        failwith "rewrap_str: ExpandedTok not allowed here"
    )
  }
(*
val rewrap_token_location : token_location.token_location -> info -> info

let rewrap_token_location pi ii =
  {ii with pinfo =
    (match ii.pinfo with
    | OriginTok _oldpi -> OriginTok pi
    | FakeTokStr _  | Ab | ExpandedTok _ ->
        failwith "rewrap_parseinfo: no OriginTok"
    )
  }
*)
let token_location_of_info ii =
  match ii.token with
  | OriginTok pinfo -> pinfo
  (* TODO ? dangerous ? *)
  | ExpandedTok (pinfo_pp, pinfo_orig, offset) -> pinfo_pp
  | FakeTokStr (_, (Some (pi, _))) -> pi

  | FakeTokStr (_, None)
  | Ab
    -> failwith "token_location_of_info: no OriginTok"

(* for error reporting *)
(*
let string_of_token_location x =
  spf "%s at %s:%d:%d" x.str x.file x.line x.column
*)
let string_of_token_location x =
  spf "%s:%d:%d" x.file x.line x.column

let string_of_info x =
  string_of_token_location (token_location_of_info x)

let str_of_info  ii = (token_location_of_info ii).str
let file_of_info ii = (token_location_of_info ii).file
let line_of_info ii = (token_location_of_info ii).line
let col_of_info  ii = (token_location_of_info ii).column

(* todo: return a Real | Virt position ? *)
let pos_of_info  ii = (token_location_of_info ii).charpos

let pinfo_of_info ii = ii.token

let is_origintok ii =
  match ii.token with
  | OriginTok pi -> true
  | _ -> false

(*
let opos_of_info ii = 
  PI.get_orig_info (function x -> x.PI.charpos) ii

val pos_of_tok     : Parser_cpp.token -> int
val str_of_tok     : Parser_cpp.token -> string
val file_of_tok    : Parser_cpp.token -> Common.filename

let pos_of_tok x =  Ast.opos_of_info (info_of_tok x)
let str_of_tok x =  Ast.str_of_info (info_of_tok x)
let file_of_tok x = Ast.file_of_info (info_of_tok x)
let pinfo_of_tok x = Ast.pinfo_of_info (info_of_tok x)

val is_origin : Parser_cpp.token -> bool
val is_expanded : Parser_cpp.token -> bool
val is_fake : Parser_cpp.token -> bool
val is_abstract : Parser_cpp.token -> bool


let is_origin x =
  match pinfo_of_tok x with Parse_info.OriginTok _ -> true | _ -> false
let is_expanded x =
  match pinfo_of_tok x with Parse_info.ExpandedTok _ -> true | _ -> false
let is_fake x =
  match pinfo_of_tok x with Parse_info.FakeTokStr _ -> true | _ -> false
let is_abstract x =
  match pinfo_of_tok x with Parse_info.Ab -> true | _ -> false
*)

let tok_add_s s ii  =
  rewrap_str ((str_of_info ii) ^ s) ii

(* info about the current location *)
(*
let get_pi = function
  | OriginTok pi -> pi
  | ExpandedTok (_,pi,_) -> pi
  | FakeTokStr (_,(Some (pi,_))) -> pi
  | FakeTokStr (_,None) ->
      failwith "FakeTokStr None"
  | Ab ->
      failwith "Ab"
*)

(* original info *)
let get_original_token_location = function
  | OriginTok pi -> pi
  | ExpandedTok (pi,_, _) -> pi
  | FakeTokStr (_,_) -> failwith "no position information"
  | Ab -> failwith "Ab"

(* used by token_helpers *)
(*
let get_info f ii =
  match ii.token with
  | OriginTok pi -> f pi
  | ExpandedTok (_,pi,_) -> f pi
  | FakeTokStr (_,Some (pi,_)) -> f pi
  | FakeTokStr (_,None) ->
      failwith "FakeTokStr None"
  | Ab ->
      failwith "Ab"
*)
(*
let get_orig_info f ii =
  match ii.token with
  | OriginTok pi -> f pi
  | ExpandedTok (pi,_, _) -> f pi
  | FakeTokStr (_,Some (pi,_)) -> f pi
  | FakeTokStr (_,None ) ->
      failwith "FakeTokStr None"
  | Ab ->
      failwith "Ab"
*)

(* not used but used to be useful in coccinelle *)
type posrv =
  | Real of token_location
  | Virt of
      token_location (* last real info before expanded tok *) *
      int (* virtual offset *)

let compare_pos ii1 ii2 =
  let get_pos = function
    | OriginTok pi -> Real pi
(* todo? I have this for lang_php/
    | FakeTokStr (s, Some (pi_orig, offset)) ->
        Virt (pi_orig, offset)
*)
    | FakeTokStr _
    | Ab
      -> failwith "get_pos: Ab or FakeTok"
    | ExpandedTok (pi_pp, pi_orig, offset) ->
        Virt (pi_orig, offset)
  in
  let pos1 = get_pos (pinfo_of_info ii1) in
  let pos2 = get_pos (pinfo_of_info ii2) in
  match (pos1,pos2) with
  | (Real p1, Real p2) ->
      compare p1.charpos p2.charpos
  | (Virt (p1,_), Real p2) ->
      if (compare p1.charpos p2.charpos) =|= (-1)
      then (-1)
      else 1
  | (Real p1, Virt (p2,_)) ->
      if (compare p1.charpos p2.charpos) =|= 1
      then 1
      else (-1)
  | (Virt (p1,o1), Virt (p2,o2)) ->
      let poi1 = p1.charpos in
      let poi2 = p2.charpos in
      match compare poi1 poi2 with
      |	-1 -> -1
      |	0 -> compare o1 o2
      |	1 -> 1
      | _ -> raise Impossible


let min_max_ii_by_pos xs =
  match xs with
  | [] -> failwith "empty list, max_min_ii_by_pos"
  | [x] -> (x, x)
  | x::xs ->
      let pos_leq p1 p2 = (compare_pos p1 p2) =|= (-1) in
      xs +> List.fold_left (fun (minii,maxii) e ->
        let maxii' = if pos_leq maxii e then e else maxii in
        let minii' = if pos_leq e minii then e else minii in
        minii', maxii'
      ) (x,x)



let mk_info_item2 ~info_of_tok toks  =
  let buf = Buffer.create 100 in
  let s =
    (* old: get_slice_file filename (line1, line2) *)
    begin
      toks +> List.iter (fun tok ->
        let info = info_of_tok tok in
        match info.token with
        | OriginTok _
        | ExpandedTok _ ->
            Buffer.add_string buf (str_of_info info)

        (* the virtual semicolon *)
        | FakeTokStr _ ->
            ()
        | Ab  -> raise Impossible
      );
      Buffer.contents buf
    end
  in
  (s, toks)

let mk_info_item_DEPRECATED ~info_of_tok a =
  Common.profile_code "Parsing.mk_info_item"
    (fun () -> mk_info_item2 ~info_of_tok a)


let lexbuf_to_strpos lexbuf     =
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)

(*****************************************************************************)
(* vtoken -> ocaml *)
(*****************************************************************************)
let vof_filename v = Ocaml.vof_string v

let vof_token_location {
                     str = v_str;
                     charpos = v_charpos;
                     line = v_line;
                     column = v_column;
                     file = v_file
                   } =
  let bnds = [] in
  let arg = vof_filename v_file in
  let bnd = ("file", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_column in
  let bnd = ("column", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_line in
  let bnd = ("line", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_charpos in
  let bnd = ("charpos", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_string v_str in
  let bnd = ("str", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds


let vof_token_origin =
  function
  | OriginTok v1 ->
      let v1 = vof_token_location v1 in Ocaml.VSum (("OriginTok", [ v1 ]))
  | FakeTokStr (v1, opt) ->
      let v1 = Ocaml.vof_string v1 in
      let opt = Ocaml.vof_option (fun (p1, i) ->
        Ocaml.VTuple [vof_token_location p1; Ocaml.vof_int i]
      ) opt
      in
      Ocaml.VSum (("FakeTokStr", [ v1; opt ]))
  | Ab -> Ocaml.VSum (("Ab", []))
  | ExpandedTok (v1, v2, v3) ->
      let v1 = vof_token_location v1 in
      let v2 = vof_token_location v2 in
      let v3 = Ocaml.vof_int v3 in
      Ocaml.VSum (("ExpandedTok", [ v1; v2; v3 ]))


let rec vof_transformation =
  function
  | NoTransfo -> Ocaml.VSum (("NoTransfo", []))
  | Remove -> Ocaml.VSum (("Remove", []))
  | AddBefore v1 -> let v1 = vof_add v1 in Ocaml.VSum (("AddBefore", [ v1 ]))
  | AddAfter v1 -> let v1 = vof_add v1 in Ocaml.VSum (("AddAfter", [ v1 ]))
  | Replace v1 -> let v1 = vof_add v1 in Ocaml.VSum (("Replace", [ v1 ]))
and vof_add =
  function
  | AddStr v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("AddStr", [ v1 ]))
  | AddNewlineAndIdent -> Ocaml.VSum (("AddNewlineAndIdent", []))

let rec vof_info
 { token = v_token; comments = v_comments; transfo = v_transfo } =
  let bnds = [] in
  let arg = vof_transformation v_transfo in
  let bnd = ("transfo", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_unit v_comments in
  let bnd = ("comments", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_token_origin v_token in
  let bnd = ("token", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds

(*****************************************************************************)
(* ocaml -> vtoken *)
(*****************************************************************************)

(*
(* todo: should move this in commons/ *)
let filename_ofv__ =
  let _loc = "Xxx.filename" in fun sexp -> Ocaml.string_ofv sexp

let filename_ofv sexp =
  filename_ofv__ sexp


let token_location_ofv__ =
  let _loc = "Xxx.token_location"
  in
    function
    | (Ocaml.VDict field_sexps as sexp) ->
        let str_field = ref None and charpos_field = ref None
        and line_field = ref None and column_field = ref None
        and file_field = ref None and duplicates = ref []
        and extra = ref [] in
        let rec iter =
          (function
           | (field_name, field_sexp) :: tail ->
               ((match field_name with
                 | "str" ->
                     (match !str_field with
                      | None ->
                          let fvalue = Ocaml.string_ofv field_sexp
                          in str_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "charpos" ->
                     (match !charpos_field with
                      | None ->
                          let fvalue = Ocaml.int_ofv field_sexp
                          in charpos_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "line" ->
                     (match !line_field with
                      | None ->
                          let fvalue = Ocaml.int_ofv field_sexp
                          in line_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "column" ->
                     (match !column_field with
                      | None ->
                          let fvalue = Ocaml.int_ofv field_sexp
                          in column_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "file" ->
                     (match !file_field with
                      | None ->
                          let fvalue = filename_ofv field_sexp
                          in file_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | [] -> ())
        in
          (iter field_sexps;
           if !duplicates <> []
           then Ocaml.record_duplicate_fields _loc !duplicates sexp
           else
             if !extra <> []
             then Ocaml.record_extra_fields _loc !extra sexp
             else
               (match ((!str_field), (!charpos_field), (!line_field),
                       (!column_field), (!file_field))
                with
                | (Some str_value, Some charpos_value, Some line_value,
                   Some column_value, Some file_value) ->
                    {
                      str = str_value;
                      charpos = charpos_value;
                      line = line_value;
                      column = column_value;
                      file = file_value;
                    }
                | _ ->
                    Ocaml.record_undefined_elements _loc sexp
                      [ ((!str_field = None), "str");
                        ((!charpos_field = None), "charpos");
                        ((!line_field = None), "line");
                        ((!column_field = None), "column");
                        ((!file_field = None), "file") ]))
    | sexp -> Ocaml.record_list_instead_atom _loc sexp

let token_location_ofv sexp = token_location_ofv__ sexp


let pinfo_ofv__ =
  let _loc = "Xxx.pinfo"
  in
    function
    | (Ocaml.VSum (((("OriginTok" as tag)), sexp_args)) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = token_location_ofv v1 in OriginTok v1
         | _ -> Ocaml.stag_incorrect_n_args _loc tag sexp)

    | (Ocaml.VSum (((("FakeTokStr" as tag)), sexp_args)) as sexp) ->
        (match sexp_args with
         | [ v1; v2 ] ->
             let v1 = Ocaml.string_ofv v1
             and v2 =
               Ocaml.option_ofv
                 (function
                  | Ocaml.VList ([ v1; v2 ]) ->
                      let v1 = token_location_ofv v1
                      and v2 = Ocaml.int_ofv v2
                      in (v1, v2)
                  | sexp -> Ocaml.tuple_of_size_n_expected _loc 2 sexp)
                 v2
             in FakeTokStr ((v1, v2))
         | _ -> Ocaml.stag_incorrect_n_args _loc tag sexp)

    | Ocaml.VSum ("Ab", []) -> Ab
    | (Ocaml.VSum (((("ExpandedTok" as tag)), sexp_args)) as sexp) ->
        (match sexp_args with
         | [ v1; v2; v3 ] ->
             let v1 = token_location_ofv v1
             and v2 = token_location_ofv v2
             and v3 = Ocaml.int_ofv v3
             in ExpandedTok ((v1, v2, v3))
         | _ -> Ocaml.stag_incorrect_n_args _loc tag sexp)
    | sexp -> Ocaml.unexpected_stag _loc sexp

let token_ofv sexp = pinfo_ofv__ sexp
*)

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

(*
let v_token_location x = ()
let v_string (s:string) = ()

let rec v_pinfo =
  function
  | OriginTok v1 -> let _v1 = v_token_location v1 in ()
  | FakeTokStr ((v1, v2)) ->
      let _v1 = v_string v1
      and _v2 = Ocaml.v_option v_token_location v2
      in
      ()
  | Ab -> ()
  | ExpandedTok ((v1, v2, v3)) ->
      (* TODO ? not sure what behavior we want about expanded tokens.
      *)
      let _v1 = v_token_location v1
      and _v2 = v_token_location v2
      and _v3 = Ocaml.v_int v3
      in ()

let rec v_transformation =
  function
  | NoTransfo -> ()
  | Remove -> ()
  | AddBefore v1 -> let _v1 = v_add v1 in ()
  | AddAfter v1 -> let _v1 = v_add v1 in ()
  | Replace v1 -> let _v1 = v_add v1 in ()
and v_add =
  function
  | AddStr v1 -> let _v1 = v_string v1 in ()
  | AddNewlineAndIdent -> ()

*)

(*
let map_pinfo =
  function
  | OriginTok v1 -> let v1 = Common.map_token_location v1
    in OriginTok ((v1))
  | FakeTokStr (v1, opt) ->
      (* TODO? do something with opt ? *)
      let v1 = map_of_string v1 in
      FakeTokStr ((v1, opt))
  | Ab -> Ab
  | ExpandedTok _ ->
      failwith "map: ExpandedTok: TODO"
*)

(*
open Ocaml

let sexp_of_token_location {
                         str = v_str;
                         charpos = v_charpos;
                         line = v_line;
                         column = v_column;
                         file = v_file
                       } =
  let bnds = [] in
  let arg = Conv.sexp_of_string v_file in
  let bnd = Sexp.List [ Sexp.Atom "file:"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Conv.sexp_of_int v_column in
  let bnd = Sexp.List [ Sexp.Atom "column:"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Conv.sexp_of_int v_line in
  let bnd = Sexp.List [ Sexp.Atom "line:"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Conv.sexp_of_int v_charpos in
  let bnd = Sexp.List [ Sexp.Atom "charpos:"; arg ] in
  let bnds = bnd :: bnds in
  let arg = Conv.sexp_of_string v_str in
  let bnd = Sexp.List [ Sexp.Atom "str:"; arg ] in
  let bnds = bnd :: bnds in Sexp.List bnds

let map_token_location {
                     str = v_str;
                     charpos = v_charpos;
                     line = v_line;
                     column = v_column;
                     file = v_file
                   } =
  let v_file = map_of_string v_file in
  let v_column = map_of_int v_column in
  let v_line = map_of_int v_line in
  let v_charpos = map_of_int v_charpos in
  let v_str = map_of_string v_str in
  {
    str = v_str;
    charpos = v_charpos;
    line = v_line;
    column = v_column;
    file = v_file
  }
*)


(*****************************************************************************)
(* Error location report *)
(*****************************************************************************)

(* A changen is a stand-in for a file for the underlying code.  We use
   channels in the underlying parsing code as this avoids loading
   potentially very large source files directly into memory before we
   even parse them, but this makes it difficult to parse small chunks of
   code.  The changen works around this problem by providing a channel,
   size and source for underlying data.  This allows us to wrap a string
   in a channel, or pass a file, depending on our needs. *)
type changen = unit -> (in_channel * int * Common.filename)

(* Many functions in parse_php were implemented in terms of files and
   are now adapted to work in terms of changens.  However, we wish to
   provide the original API to users.  This wraps changen-based functions
   and makes them operate on filenames again. *)
let file_wrap_changen : (changen -> 'a) -> (Common.filename -> 'a) = fun f ->
  (fun file ->
    f (fun () -> (open_in file, Common2.filesize file, file)))

let (info_from_charpos2: int -> filename -> (int * int * string)) =
  fun charpos filename ->

  (* Currently lexing.ml does not handle the line number position.
   * Even if there is some fields in the lexing structure, they are not
   * maintained by the lexing engine :( So the following code does not work:
   *   let pos = Lexing.lexeme_end_p lexbuf in
   *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
   *      (pos.pos_cnum - pos.pos_bol) in
   * Hence this function to overcome the previous limitation.
   *)
  let chan = open_in filename in
  let linen  = ref 0 in
  let posl   = ref 0 in
  let rec charpos_to_pos_aux last_valid =
    let s =
      try Some (input_line chan)
      with End_of_file when charpos =|= last_valid -> None in
    incr linen;
    match s with
      Some s ->
        let s = s ^ "\n" in
        if (!posl + String.length s > charpos)
        then begin
          close_in chan;
          (!linen, charpos - !posl, s)
        end
        else begin
          posl := !posl + String.length s;
          charpos_to_pos_aux !posl;
        end
    | None -> (!linen, charpos - !posl, "\n")
  in
  let res = charpos_to_pos_aux 0 in
  close_in chan;
  res

let info_from_charpos a b =
  profile_code "Common.info_from_charpos" (fun () -> info_from_charpos2 a b)

let full_charpos_to_pos_from_changen changen =
  let (chan, chansize, _) = changen () in

  let size = (chansize + 2) in

    let arr = Array.create size  (0,0) in

    let charpos   = ref 0 in
    let line  = ref 0 in

    let rec full_charpos_to_pos_aux () =
     try
       let s = (input_line chan) in
       incr line;

       (* '... +1 do'  cos input_line dont return the trailing \n *)
       for i = 0 to (String.length s - 1) + 1 do
         arr.(!charpos + i) <- (!line, i);
       done;
       charpos := !charpos + String.length s + 1;
       full_charpos_to_pos_aux();

     with End_of_file ->
       for i = !charpos to Array.length arr - 1 do
         arr.(i) <- (!line, 0);
       done;
       ();
    in
    begin
      full_charpos_to_pos_aux ();
      close_in chan;
      arr
    end

let full_charpos_to_pos2 = file_wrap_changen full_charpos_to_pos_from_changen

let full_charpos_to_pos a =
  profile_code "Common.full_charpos_to_pos" (fun () -> full_charpos_to_pos2 a)

let test_charpos file =
  full_charpos_to_pos file +> Common2.dump +> pr2



let complete_token_location filename table x =
  { x with
    file = filename;
    line   = fst (table.(x.charpos));
    column = snd (table.(x.charpos));
  }



let full_charpos_to_pos_large_from_changen = fun changen ->
  let (chan, chansize, _) = changen () in

  let size = (chansize + 2) in

    (* old: let arr = Array.create size  (0,0) in *)
    let arr1 = Bigarray.Array1.create
      Bigarray.int Bigarray.c_layout size in
    let arr2 = Bigarray.Array1.create
      Bigarray.int Bigarray.c_layout size in
    Bigarray.Array1.fill arr1 0;
    Bigarray.Array1.fill arr2 0;

    let charpos   = ref 0 in
    let line  = ref 0 in

    let full_charpos_to_pos_aux () =
      try
        while true do begin
          let s = (input_line chan) in
          incr line;

          (* '... +1 do'  cos input_line dont return the trailing \n *)
          for i = 0 to (String.length s - 1) + 1 do
            (* old: arr.(!charpos + i) <- (!line, i); *)
            arr1.{!charpos + i} <- (!line);
            arr2.{!charpos + i} <- i;
          done;
          charpos := !charpos + String.length s + 1;
        end done
     with End_of_file ->
       for i = !charpos to (* old: Array.length arr *)
         Bigarray.Array1.dim arr1 - 1 do
         (* old: arr.(i) <- (!line, 0); *)
         arr1.{i} <- !line;
         arr2.{i} <- 0;
       done;
       ();
    in
    begin
      full_charpos_to_pos_aux ();
      close_in chan;
      (fun i -> arr1.{i}, arr2.{i})
    end

let full_charpos_to_pos_large2 =
  file_wrap_changen full_charpos_to_pos_large_from_changen

let full_charpos_to_pos_large a =
  profile_code "Common.full_charpos_to_pos_large"
    (fun () -> full_charpos_to_pos_large2 a)


let complete_token_location_large filename table x =
  { x with
    file = filename;
    line   = fst (table (x.charpos));
    column = snd (table (x.charpos));
  }

(*---------------------------------------------------------------------------*)
(* Decalage is here to handle stuff such as cpp which include file and who
 * can make shift.
 *)
let (error_messagebis: filename -> (string * int) -> int -> string)=
 fun filename (lexeme, lexstart) decalage ->

  let charpos = lexstart      + decalage in
  let tok = lexeme in
  let (line, pos, linecontent) =  info_from_charpos charpos filename in
  spf "File \"%s\", line %d, column %d,  charpos = %d
    around = '%s', whole content = %s"
    filename line pos charpos tok (Common2.chop linecontent)

let error_message = fun filename (lexeme, lexstart) ->
  try error_messagebis filename (lexeme, lexstart) 0
  with
    End_of_file ->
      ("PB in Common.error_message, position " ^ i_to_s lexstart ^
       " given out of file:" ^ filename)

let error_message_token_location = fun info ->
  let filename = info.file in
  let lexeme = info.str in
  let lexstart = info.charpos in
  try error_messagebis filename (lexeme, lexstart) 0
  with
    End_of_file ->
      ("PB in Common.error_message, position " ^ i_to_s lexstart ^
       " given out of file:" ^ filename)

let error_message_info info =
  let pinfo = token_location_of_info info in
  error_message_token_location pinfo


let error_message_short = fun filename (lexeme, lexstart) ->
  try
  let charpos = lexstart in
  let (line, pos, linecontent) =  info_from_charpos charpos filename in
  spf "File \"%s\", line %d"  filename line

  with End_of_file ->
    begin
      ("PB in Common.error_message, position " ^ i_to_s lexstart ^
          " given out of file:" ^ filename);
    end


let print_bad line_error (start_line, end_line) filelines  =
  begin
    pr2 ("badcount: " ^ i_to_s (end_line - start_line));

    for i = start_line to end_line do
      let line = filelines.(i) in

      if i =|= line_error
      then  pr2 ("BAD:!!!!!" ^ " " ^ line)
      else  pr2 ("bad:" ^ " " ^      line)
    done
  end


(*****************************************************************************)
(* Parsing statistics *)
(*****************************************************************************)

let print_parsing_stat_list statxs =
  let total = List.length statxs in
  let perfect =
    statxs
      +> List.filter (function
      | {bad = n; _} when n = 0 -> true
      | _ -> false)
      +> List.length
  in

  pr2 "\n\n\n---------------------------------------------------------------";
  pr2 (
  (spf "NB total files = %d; " total) ^
  (spf "perfect = %d; " perfect) ^
  (spf "=========> %d" ((100 * perfect) / total)) ^ "%"
  );

  let good = statxs +> List.fold_left (fun acc {correct = x; _} -> acc+x) 0 in
  let bad  = statxs +> List.fold_left (fun acc {bad = x; _} -> acc+x) 0  in

  let gf, badf = float_of_int good, float_of_int bad in
  pr2 (
  (spf "nb good = %d,  nb bad = %d " good bad) ^
  (spf "=========> %f"  (100.0 *. (gf /. (gf +. badf))) ^ "%"
   )
  )

