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

(* todo: move code for instance Common.parse_info here
*)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* todo: bad name, vtoken for virtual token ? *)
type vtoken =
    (* Present both in the AST and list of tokens *)
    | OriginTok  of Common.parse_info

    (* Present only in the AST and generated after parsing. Can be used
     * when building some extra AST elements. *)
    | FakeTokStr of string (* to help the generic pretty printer *) *
        (* Sometimes we generate fake tokens close to existing 
         * origin tokens. This can be useful when have to give an error
         * message that involves a fakeToken. The int is a kind of
         * virtual position, an offset. See compare_pos below.
         *)
        (Common.parse_info * int) option

    (* The Ab constructor is (ab)used to call '=' to compare 
     * big AST portions. Indeed as we keep the token information in the AST, 
     * if we have an expression in the code like "1+1" and want to test if
     * it's equal to another code like "1+1" located elsewhere, then
     * the Pervasives.'=' of OCaml will not return true because 
     * when it recursively goes down to compare the leaf of the AST, that is
     * the parse_info, there will be some differences of positions. If instead
     * all leaves use Ab, then there is no position information and we can 
     * use '='. See also the 'al_info' function below.
     * 
     * Ab means AbstractLineTok. I Use a short name to not
     * polluate in debug mode.
     *)
    | Ab

    (* In the case of a XHP file, we could preprocess it and incorporate
     * the tokens of the preprocessed code with the tokens from
     * the original file. We want to mark those "expanded" tokens 
     * with a special tag so that if someone do some transformation on 
     * those expanded tokens they will get a warning (because we may have
     * trouble back-propagating the transformation back to the original file).
     *)
    | ExpandedTok of 
        (* refers to the preprocessed file, e.g. /tmp/pp-xxxx.pphp *)
        Common.parse_info  *
       (* kind of virtual position. This info refers to the last token
        * before a serie of expanded tokens and the int is an offset.
        * The goal is to be able to compare the position of tokens
        * between then, even for expanded tokens. See compare_pos 
        * below.
        *)
        Common.parse_info * int 

   (* with tarzan *)

type info = { 
  (* contains among other things the position of the token through
   * the Common.parse_info embedded inside the pinfo type.
   *)
  mutable pinfo : vtoken; 
  comments: unit; (* TODO *)
  mutable transfo: transformation;
}

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

(*****************************************************************************)
(* Lexer helpers *)
(*****************************************************************************)

let tokinfo_str_pos str pos = 
  { 
    pinfo = OriginTok {
      Common.charpos = pos; 
      Common.str     = str;

      (* info filled in a post-lexing phase, cf Parse_php.tokens *)
      Common.line = -1; 
      Common.column = -1; 
      Common.file = "";
    };
    comments = ();
    transfo = NoTransfo;
  }


let rewrap_str s ii =  
  {ii with pinfo =
    (match ii.pinfo with
    | OriginTok pi -> OriginTok { pi with Common.str = s;}
    | FakeTokStr (s, info) -> FakeTokStr (s, info)
    | Ab -> Ab
    | ExpandedTok _ -> failwith "rewrap_str: ExpandedTok not allowed here"
    )
  }


(*****************************************************************************)
(* vtoken -> ocaml *)
(*****************************************************************************)
let vof_filename v = Ocaml.vof_string v

let vof_parse_info {
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


let vof_vtoken =
  function
  | OriginTok v1 ->
      let v1 = vof_parse_info v1 in Ocaml.VSum (("OriginTok", [ v1 ]))
  | FakeTokStr (v1, opt) ->
      let v1 = Ocaml.vof_string v1 in 
      let opt = Ocaml.vof_option (fun (p1, i) ->
        Ocaml.VTuple [vof_parse_info p1; Ocaml.vof_int i]
      ) opt
      in
      Ocaml.VSum (("FakeTokStr", [ v1; opt ]))
  | Ab -> Ocaml.VSum (("Ab", []))
  | ExpandedTok (v1, v2, v3) ->
      let v1 = vof_parse_info v1 in 
      let v2 = vof_parse_info v2 in 
      let v3 = Ocaml.vof_int v3 in
      Ocaml.VSum (("ExpandedTok", [ v1; v2; v3 ]))



(*****************************************************************************)
(* ocaml -> vtoken *)
(*****************************************************************************)

(* todo: should move this in commons/ *)
let filename_ofv__ =
  let _loc = "Xxx.filename" in fun sexp -> Ocaml.string_ofv sexp
  
let filename_ofv sexp =
  filename_ofv__ sexp
  

let parse_info_ofv__ =
  let _loc = "Xxx.parse_info"
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
  
let parse_info_ofv sexp = parse_info_ofv__ sexp


let pinfo_ofv__ =
  let _loc = "Xxx.pinfo"
  in
    function
    | (Ocaml.VSum (((("OriginTok" as tag)), sexp_args)) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = parse_info_ofv v1 in OriginTok v1
         | _ -> Ocaml.stag_incorrect_n_args _loc tag sexp)

    | (Ocaml.VSum (((("FakeTokStr" as tag)), sexp_args)) as sexp) ->
        (match sexp_args with
         | [ v1; v2 ] ->
             let v1 = Ocaml.string_ofv v1
             and v2 =
               Ocaml.option_ofv
                 (function
                  | Ocaml.VList ([ v1; v2 ]) ->
                      let v1 = parse_info_ofv v1
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
             let v1 = parse_info_ofv v1
             and v2 = parse_info_ofv v2
             and v3 = Ocaml.int_ofv v3
             in ExpandedTok ((v1, v2, v3))
         | _ -> Ocaml.stag_incorrect_n_args _loc tag sexp)
    | sexp -> Ocaml.unexpected_stag _loc sexp
  
let vtoken_ofv sexp = pinfo_ofv__ sexp


(*****************************************************************************)
(* misc *)
(*****************************************************************************)

(*
let v_parse_info x = ()
let v_string (s:string) = ()

let rec v_parse_info = function 
  | OriginTok v1 -> 
      let _v1 = v_parse_info v1 
      in () 
  | Ab -> () 
  | FakeTokStr (s, opt) -> 
      v_string s

  | ExpandedTok (pi_pp, pi_orig, offset) -> 
      (* TODO ? not sure what behavior we want about expanded tokens.
      *)
      ()

*)

(*
let map_pinfo =
  function
  | OriginTok v1 -> let v1 = Common.map_parse_info v1 
    in OriginTok ((v1))
  | FakeTokStr (v1, opt) -> 
      (* TODO? do something with opt ? *)
      let v1 = map_of_string v1 in 
      FakeTokStr ((v1, opt))
  | Ab -> Ab
  | ExpandedTok _ -> 
      failwith "map: ExpandedTok: TODO"
*)
