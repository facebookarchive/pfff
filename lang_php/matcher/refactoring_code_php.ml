(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

open Ast_php
module Ast = Ast_php
module V = Visitor_php
module PI = Parse_info
module R = Refactoring_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let tok_pos_equal_refactor_pos tok refactoring_opt =
  match refactoring_opt with
  | None -> true
  | Some refactoring ->
    PI.line_of_info tok = refactoring.R.line &&
    PI.col_of_info tok = refactoring.R.col
  
let string_of_class_var_modifier modifiers =
  match modifiers with
  | NoModifiers _ -> "var"
  | VModifiers xs -> xs +> List.map (fun (modifier, tok) ->
      PI.str_of_info tok) +> Common.join " "


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let refactor refactorings (ast, tokens) =
  refactorings +> List.iter (fun (kind, pos_opt) ->
    let was_modifed = ref false in
    let visitor =
      match kind with
      | R.AddReturnType str ->
          { V.default_visitor with
            V.kfunc_def = (fun (k, _) def ->
              (match def.f_type with
              | FunctionRegular | MethodRegular | MethodAbstract ->
                  let tok = Ast.info_of_ident def.f_name in
                  if tok_pos_equal_refactor_pos tok pos_opt then begin
                    let tok_close_paren =
                      let (a,b,c) = def.f_params in c
                    in
                    tok_close_paren.PI.transfo <-
                      PI.AddAfter (PI.AddStr (": " ^ str));
                    was_modifed := true;
                  end;
                  k def
              (* lambda f_name are an abstract token and so don't have
               * any line/col position information for now
               *)
              | FunctionLambda ->
                  k def
              )
            );
          }
      | R.AddTypeHintParameter str ->
          { V.default_visitor with
            V.kparameter = (fun (k, _) p ->
              let tok = Ast.info_of_dname p.p_name in
              if tok_pos_equal_refactor_pos tok pos_opt then begin
                tok.PI.transfo <-
                  PI.AddBefore (PI.AddStr (str ^ " "));
                was_modifed := true;
              end;
              k p
            );
          }
      | R.OptionizeTypeParameter ->
          { V.default_visitor with
            V.kparameter = (fun (k, _) p ->
              (match p.p_type with
              | None -> ()
              | Some x ->
                  let tok =
                    match x with
                    | HintArray tok -> tok
                    | Hint (name, _typeargs) -> Ast.info_of_name name
                    | HintQuestion (tok, t) -> tok
                    | HintTuple (t, _, _) -> t
                    | HintCallback (lparen,_,_) -> lparen
                  in
                  if tok_pos_equal_refactor_pos tok pos_opt then begin
                    tok.PI.transfo <-
                      PI.AddBefore (PI.AddStr ("?"));
                    was_modifed := true;
                  end
              );
              k p
            );
          }

      | R.AddTypeMember str ->
          { V.default_visitor with
            V.kclass_stmt = (fun (k, _) x ->
              match x with
              | ClassVariables (_cmodif, _typ_opt, xs, _tok) ->
                  (match xs with
                  | [Left (dname, affect_opt)] ->
                      let tok = Ast.info_of_dname dname in
                      if tok_pos_equal_refactor_pos tok pos_opt then begin
                        tok.PI.transfo <-
                          PI.AddBefore (PI.AddStr (str ^ " "));
                        was_modifed := true;
                      end;
                      k x
                  | xs ->
                      xs +> Ast.uncomma +> List.iter (fun (dname, _) ->
                      let tok = Ast.info_of_dname dname in
                      if tok_pos_equal_refactor_pos tok pos_opt then begin
                        failwith "Do a SPLIT_MEMBERS refactoring first"
                      end;
                      );
                      k x
                  )
              | _ -> k x
            );
          }
      | R.SplitMembers ->
          { V.default_visitor with
            V.kclass_stmt = (fun (k, _) x ->
              match x with
              (* private $x, $y; *)
              | ClassVariables (modifiers, _typ_opt, xs, semicolon) ->
                  (match xs with
                  (* $x *)
                  | Left (dname, affect_opt)::rest ->
                      let tok = Ast.info_of_dname dname in
                      if tok_pos_equal_refactor_pos tok pos_opt then begin

                        let rec aux rest =
                          match rest with
                          (* , $y -> ;\n private $y *)
                          | Right comma::Left (dname, affect_opt)::rest ->
                              (* todo: look at col of modifiers? *)
                              let indent = "  " in
                              let str_modifiers =
                                ";\n" ^ indent ^
                                string_of_class_var_modifier modifiers ^ " "
                              in
                              comma.PI.transfo <-
                                PI.Replace (PI.AddStr str_modifiers);
                              aux rest
                          | [] -> ()
                          | _ -> raise Impossible
                        in
                        aux rest;
                        was_modifed := true;
                      end;
                      k x
                  | _ -> raise Impossible
                  )
              | (UseTrait (_, _, _)|XhpDecl _|Method _
                |ClassConstants (_, _, _)
                ) -> k x
            );
          }

    in
    (V.mk_visitor visitor) (Program ast);
    if not !was_modifed
    then begin
      pr2_gen (kind, pos_opt);
      failwith ("refactoring didn't apply");
    end
  );
  Unparse_php.string_of_program_with_comments_using_transfo (ast, tokens)
