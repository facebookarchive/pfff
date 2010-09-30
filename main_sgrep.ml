(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)

open Common

open Ast_php 

module Ast = Ast_php
module V = Visitor_php

module S = Scope_code

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* 
 * A syntactical grep for PHP.
 * 
 * opti: git grep xxx | xargs sgrep_php
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false


let pattern_file = ref ""
let pattern_string = ref ""

let match_format = ref Lib_parsing_php.Normal

let mvars = ref ([]: Metavars_php.mvar list)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO? could do slicing of function relative to the pattern, so 
 * would see where the parameters come from :)
 *)

let print_match mvars mvar_binding tokens_matched_code = 
  match mvars with
  | [] ->
      Lib_parsing_php.print_match ~format:!match_format tokens_matched_code
  | [x] ->
      (match Common.assoc_option x mvar_binding with
      | Some binded_code ->
          (match binded_code with
          | Metavars_php.Expr e ->
            let ii = Lib_parsing_php.ii_of_expr e in
            Lib_parsing_php.print_match 
              ~format:Lib_parsing_php.OneLine ii
          )
      | None ->
          failwith (spf "the metavariable '%s' was not binded" x)
      )
  | x::y::xs ->
      failwith "multiple metavariables not yet handled. mailto:pad@facebook.com"

let print_simple_match tokens_matched_code =
  print_match [] [] tokens_matched_code
  
(*****************************************************************************)
(* Main action *)
(*****************************************************************************)
let main_action xs =

  (* for now handle only expression *)
  let pattern_expr = 
    match !pattern_file, !pattern_string with
    | "", "" -> failwith "I need a pattern; use -f or -e";
    | file, _ when file <> "" ->
        let (ast2, _stat) = Parse_php.parse file in
        let ast = Parse_php.program_of_program2 ast2 in

        (match ast with
        | [Ast.StmtList [Ast.ExprStmt (e, _tok)];Ast.FinalDef _] -> e
        | _ -> failwith "only expr pattern are supported for now"
        )
    | _, s when s <> ""->
        Parse_php.expr_of_string s
    | _ -> raise Impossible
  in

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  files +> List.iter (fun file ->
    if !verbose then pr2 (spf "processing: %s" file);

    let (ast2, _stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in
    Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

    (* bugfix: if we have an ExprVar, then a pattern such as 
     * $foo->method() will not match expressions such as 
     * $foo->method()->method because the full ExprVar will not
     * match. The pb is that we need not only a match_e_e but also
     * a match_v_v with the same visitor
     *)
    let hook = 

      match Ast.untype pattern_expr with
      | Ast.Lv pattern_var ->
          { V.default_visitor with
            V.klvalue = (fun (k, _) x ->
              let matches_with_env =  
                Matching_php.match_v_v pattern_var x
              in 
              if matches_with_env = []
              then k x
              else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
                let matched_tokens = Lib_parsing_php.ii_of_lvalue x in
                matches_with_env +> List.iter (fun env ->
                  print_match !mvars env matched_tokens
                )
              end
            );
          }

      | _ ->
          { V.default_visitor with
            V.kexpr = (fun (k, _) x ->
              let matches_with_env =  
                Matching_php.match_e_e pattern_expr  x
              in
              if matches_with_env = []
              then k x
              else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
                let matched_tokens = Lib_parsing_php.ii_of_expr x in
                matches_with_env +> List.iter (fun env ->
                  print_match !mvars env matched_tokens
                )
              end
            );
          }
    in
    ast +> List.iter (fun top ->
      (* opti ? dont analyze func if no constant in it ?*)
      (V.mk_visitor hook).V.vtop top
    );
   
  )

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* Finding object passed by references parameter declarations *)
(*---------------------------------------------------------------------------*)

(* TODO should just be a sgrep script in the futur, part of our 
 * batteries of sgrep script for our new lint_php, 
 * but for now I allow only PHP expression patterns in sgrep.
 *)
let test_find_object_ref_parameter xs = 

  let files = Common.files_of_dir_or_files "php" xs in

  Flag_parsing_php.show_parsing_error := false;
  Flag_parsing_php.verbose_lexing := false;

  files +> List.iter (fun file ->
    if !verbose then pr2 (spf "processing: %s" file);

    let (ast2, _stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in
    Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

    let arr = Common.cat_array file in

    let hook = { V.default_visitor with
      V.kparameter = (fun (k, _) param ->
        match param with
        | { Ast_php.p_ref = Some _; p_type = Some (Ast_php.Hint _)} ->
            let info = Ast_php.info_of_dname param.Ast_php.p_name in
            let file = Ast_php.file_of_info info in
            let line = Ast_php.line_of_info info in
            let prefix = spf "%s:%d" file line in
            pr2 (prefix ^ arr.(line));
            pr2 " Passing object by reference makes no sense in PHP";
        | _ -> ()
      );
    }
    in
    (V.mk_visitor hook).V.vprogram ast;
    
    ()
  )

(*---------------------------------------------------------------------------*)
(* bad static array variables *)
(*---------------------------------------------------------------------------*)

(* todo: should be moved in lint_php.ml or facebook/qa_code/ 
 * test with tests/facebook/lint/perf_static_var.php
 *)
let test_find_bad_static_arrays xs = 

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  Flag_parsing_php.show_parsing_error := false;
  Flag_parsing_php.verbose_lexing := false;
  files +> List.iter (fun file ->
    if !verbose then pr2 (spf "processing: %s" file);

    let (ast2, _stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in
    Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

    let process_body body =
      let static_vars = Lib_parsing_php.get_static_vars 
        (fun vout -> vout.V.vbody body) in
      let vars_and_assigns = Lib_parsing_php.get_vars_assignements 
        (fun vout -> vout.V.vbody body) in

      let returns = Lib_parsing_php.get_returns 
        (fun vout -> vout.V.vbody body) in
      let vars_in_return = returns |> Common.map_filter (fun e -> 
        match Ast.untype e with
        | Lv v ->
            (match Ast.untype v with
            | Var (dname, _scope) ->
                let s = Ast.dname dname in
                Some s
            | _ -> None
            )
        | _ -> None
      )
      in
      
      static_vars |> List.iter (fun dname -> 
        let s = Ast.dname dname in
        let info = Ast.info_of_dname dname in

        let assigns = 
          try 
            Common.assoc s vars_and_assigns
          with Not_found -> []
        in
        if assigns |> List.exists (fun e ->
          match Ast.untype e with
          | ConsArray _ -> true
          | _ -> false
        ) && List.mem s vars_in_return
        then
          pr2 (spf "possibly a bad static array: %s" 
                  (Ast.string_of_info info))
        else ()
      );
    in

    let hook = { V.default_visitor with
      V.kfunc_def = (fun (k, _) def ->
        (* we can not do much for regular func for now. Only 
         * static methods can be refactored to put the static var
         * in the class instead of the method
         *)
        (*process_body def.f_body; *)
        k def;
      );
      V.kclass_stmt = (fun (k, _) x ->
        match x with
        | Method def -> 
            (match def.m_body with
            | AbstractMethod _ -> ()
            | MethodBody body -> 
                process_body body
            )
        | ClassConstants _ | ClassVariables _ -> k x
        | XhpDecl _ -> k x
      );
    }
    in
    (V.mk_visitor hook).V.vprogram ast;
    

  )

(*---------------------------------------------------------------------------*)
(* xss attack finder in xhp form, for alok *)
(*---------------------------------------------------------------------------*)
 
(* 
 * Apparently using <form ... method="post"> is now considered dangerous
 * and should be replaced by <ui:form ...>. grepping for 'form'
 * returns too many false positives and grepping for 'form ...method="post"'
 * has false negatives when the form is splitted over multiple lines.
 * 
 * This is an ideal candidate for sgrep_php, but:
 *  - sgrep does not really handle XHP. Instead it first calls the XHP
 *    preprocessor and then analyze the code so the form above 
 *    actually looks like 'new xhp_form(array('action' => ...))'
 *    (just call the 'xhpize' script on any XHP file to see the resulting code)
 *  - In theory we should just do then
 *      sgrep_php -e 'new xhp_foo(array(..., method=> "post"), ...)'
 *    but sgrep does not handle yet '...' in array expressions.
 * 
 * Hence this hardcoded query using directly pfff ASTs visitors.
 *)

let find_bad_xhp_form xs = 
  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  Flag_parsing_php.show_parsing_error := false;
  Flag_parsing_php.verbose_lexing := false;
  files +> List.iter (fun file ->
    if !verbose then pr2 (spf "processing: %s" file);

    let (ast2, _stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in
    Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

    let hook = { V.default_visitor with
      V.kexpr = (fun (k, bigf) x ->
        (* those patterns were mostly generated using pfff/ffi -dump_php_ml 
         *   tests/facebook/xss/xhp1.php
         *)
        match x with
        | (New(i_12,
              ClassNameRefStatic(
                Name(("xhp_form", i_13))),
              Some((i_14,
                   (Left(
                     Arg(
                       (ConsArray(i_15,
                                 (i_16,
                                 xs
                                   ,
                                 i_35)),
                       t_36)))::_rest
                       
                   ),
                     i_48))),
            t_49)
          -> 

            (* found a <form, now look if it's a post form, 
             * or actually anything that is not a get as some
             * form can have method={$method}
             *)
            if xs |> List.exists (fun y ->
              match y with
              | Left(
                  ArrayArrowExpr(
                    (Sc(
                      C(
                        String(
                          ("method",
                          i_29)))),
                    t_30), i_31,
                    (Sc(
                      C(
                        String(
                          ("get", i_32)))),
                    t_33)))
                  -> true
              | _ -> false
            )
            then ()
            else 
              print_simple_match (Lib_parsing_php.ii_of_expr x)
            ;

           (* recurse, the form can contains nested forms *)
            k x;

                
        | _ ->
            k x
      );
    }
    in
    (V.mk_visitor hook).V.vprogram ast;
  )

(*---------------------------------------------------------------------------*)
(* Specific sgrep queries for erling *)
(*---------------------------------------------------------------------------*)

let find_bad_jsprintf xs = 
  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  files +> List.iter (fun file ->
    if !verbose then pr2 (spf "processing: %s" file);

    let (ast2, _stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in
    Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

    let hook = { V.default_visitor with
      V.klvalue = (fun (k, _) x ->
        match Ast.untype x with
        | FunCallSimple (Name ("jsprintf", ii), args) ->
            if !verbose then 
              print_simple_match (Lib_parsing_php.ii_of_lvalue x);

            (* may need to define a helper function for that at some time ...*)
            let args = 
              args +> Ast.unparen +> Ast.uncomma +> List.map Ast.unarg 
            in
            (match args with
            | (Sc(C(String((s, ii_s)))), _t)::xs ->
                let percents = 
                  Common.all_match "\\(%.\\)" s in
                Common.zip percents xs +> List.iter (fun (percent, x) ->
                  if percent = "%C"
                  then 
                    Lib_parsing_php.print_match ~format:Lib_parsing_php.OneLine 
                      (Lib_parsing_php.ii_of_expr x);
                );
            | _ -> failwith ("weird jsprintf: " ^ Ast.string_of_info ii)
            );           
            k x
        | _ -> k x
      );
    }
    in
    (V.mk_visitor hook).V.vprogram ast;
  )

(*---------------------------------------------------------------------------*)
(* Finding run_once patterns *)
(*---------------------------------------------------------------------------*)

(* pre: requires to have run the Check_variables.check_and_annotate_program
 * which "tags" variables with the appropriate scope information.
 *)
let globals_or_statics_lvalues_in = 
 V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.klvalue = (fun (k, _) lvalue ->
      match lvalue with
      | Var (varname, { contents = (S.Global | S.Static)}), _t ->
          Common.push2 lvalue aref
      | VArrayAccess (
          (Var (varname, { contents = (S.Global | S.Static)}), _t2),
          _expr
        ), _t ->
          Common.push2 lvalue aref

      (* todo? all qualified variables are static variables ?
       * maybe should let Check_variables.check_and_annotate_program also
       * annotates those vars with a GlobalClass tag.
       *)
      | VQualifier (qu_, var), _ ->
          Common.push2 lvalue aref

      | _ -> k lvalue
    );
  }
  )
let globals_or_statics_lvalues_in_expr e = 
  globals_or_statics_lvalues_in (fun v -> v.V.vexpr e)

let vars_assigned_in =
  V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.kexpr = (fun (k, _) expr ->
      match Ast.untype expr with
      | Assign (lval, _, expr2) 
      | AssignOp (lval, _, expr2) 
          ->
          Common.push2 (lval, expr2) aref
      | _ -> k expr
    )
  })
let vars_assigned_in_stmt x = 
  vars_assigned_in (fun v -> v.V.vstmt x)


let find_run_once_pattern files_or_dirs =  
  let files = Lib_parsing_php.find_php_files_of_dir_or_files files_or_dirs in

  files +> List.iter (fun file ->
    if !verbose then pr2 (spf "processing: %s" file);

    let (ast2, _stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in
    Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

    (* here we just want to annotate the AST with the Local/Global/Static
     * scope tag; we don't want to check if all the variables
     * are correctly used (e.g. wether a $this->field is valid
     * by looking at the definition of the class of $this) hence
     * the None argument.
     *)
    Check_variables_php.check_and_annotate_program
      ~find_entity:None
      ast;

    (* Step1: find a if.
     * todo? restrict to toplevel ifs in a function/method? 
     *)
    let v = V.mk_visitor { V.default_visitor with
      V.kstmt = (fun (k, _) stmt ->
        match stmt with
        | If (tok_if, (_lp, expr, _rp), then_branch, elseif_list, else_opt) ->

            (* Step2: extract globals or statics lvalues in the
             * condition of the if. Can be a simple global $foo, a
             * specialglobal like $GLOBALS['index'], or a static class
             * variable like self::$foo, or a static variable.
             * 
             * todo? should perhaps restrict to certain patterns like
             * if($foo), if(!$foo), if(isset($foo)), if(!isset($foo)). 
             *)
            let lvalues = globals_or_statics_lvalues_in_expr expr in
            
            lvalues +> List.iter (fun lval ->
              let s = Unparse_php.string_of_lvalue lval in
              if !verbose then pr2 (spf "\tGLOBAL in if: %s" s);
            );

            (* Step3: extract assignements in the body of the if 
             * todo? restrict to simple ifs without a else branch ? *)
            let assigns_lval_expr = 
              vars_assigned_in_stmt then_branch in
            
            (* Step4: check if the global in the if condition matches
             * one of the assignement in the body.
             * 
             * todo? could enforce that there is only one such
             * assignement and that it has a specific form (e.g. '=
             * true' when the condition was on the global being
             * false). *)
            lvalues +> List.iter (fun lval ->
              let lval1 = 
                Lib_parsing_php.abstract_position_info_lvalue lval in
              assigns_lval_expr +> List.iter (fun (lval_assign, expr_assign) ->
                let lval2 =
                  Lib_parsing_php.abstract_position_info_lvalue lval_assign in
                
                if lval1 = lval2
                then begin
                  (* Step5: report the match showing both the condition and
                   * associated assignement.
                   * 
                   * todo: see if the previous naive algorithm reports too
                   * many false positives. *)
                  pr2 ("FOUND a run_once pattern.");
                  pr2 ("IF = ");
                  print_simple_match [tok_if];
                  pr2 ("ASSIGN = ");
                  print_simple_match (Lib_parsing_php.ii_of_expr expr_assign);
                end
              );
            )

        | _ -> k stmt
      );
    }
    in
    v.V.vprogram ast; 
  )

(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let sgrep_extra_actions () = [
  "-find_object_ref", " <files>",
  Common.mk_action_n_arg (test_find_object_ref_parameter);
  "-find_bad_static_arrays", " <files_or_dira>",
  Common.mk_action_n_arg (test_find_bad_static_arrays);

  "-find_bad_xhp_form", " <files_or_dirs>",
  Common.mk_action_n_arg (find_bad_xhp_form);

  "-find_bad_jsprintf", " <files_or_dirs>",
  Common.mk_action_n_arg (find_bad_jsprintf);
  "-find_run_once_pattern", " <files_or_dirs>",
  Common.mk_action_n_arg (find_run_once_pattern);
  
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 sgrep_extra_actions()++
 Test_parsing_php.actions()++
 []

let options () = 
  [
    "-e", Arg.Set_string pattern_string, 
    " <pattern> expression pattern";
    "-f", Arg.Set_string pattern_file, 
    " <file> obtain pattern from file";
    "-emacs", Arg.Unit (fun () -> match_format := Lib_parsing_php.Emacs ),
    " print matches on the same line than the match position";
    "-oneline", Arg.Unit (fun () -> match_format := Lib_parsing_php.OneLine),
    " print matches on one line, in normalized form";

    "-pvar", Arg.String (fun s -> mvars := Common.split "," s),
    " <metavar> print the metavariable, not the matched code";

    "-verbose", Arg.Set verbose, 
    " ";
  ] ++
  Flag_parsing_php.cmdline_flags_pp () ++
  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  Common.cmdline_flags_other () ++
  [
  "-version",   Arg.Unit (fun () -> 
    pr2 (spf "sgrep_php version: %s" Config.version);
    exit 0;
  ), 
    "  guess what";

  (* this can not be factorized in Common *)
  "-date",   Arg.Unit (fun () -> 
    pr2 "version: $Date: 2008/10/26 00:44:57 $";
    raise (Common.UnixExit 0)
    ), 
  "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  let usage_msg = 
    "Usage: " ^ Common.basename Sys.argv.(0) ^ 
      " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 

    (match args with
   
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options()); 
        failwith "too few arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )
