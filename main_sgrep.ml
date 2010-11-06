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

let layer_file = ref (None: filename option)

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* for -gen_layer *)
let _matching_tokens = ref []

(* TODO? could do slicing of function relative to the pattern, so 
 * would see where the parameters come from :)
 *)

let print_match mvars mvar_binding tokens_matched_code = 
  (match mvars with
  | [] ->
      Lib_parsing_php.print_match ~format:!match_format tokens_matched_code

  | [x] ->
      (match Common.assoc_option x mvar_binding with
      | Some binded_code ->
          (match binded_code with
          | Metavars_php.Expr e ->
            let ii = Lib_parsing_php.ii_of_any (Expr e) in
            Lib_parsing_php.print_match 
              ~format:Lib_parsing_php.OneLine ii
          )
      | None ->
          failwith (spf "the metavariable '%s' was not binded" x)
      )
  | x::y::xs ->
      failwith "multiple metavariables not yet handled. mailto:pad@facebook.com"
  );
  tokens_matched_code +> List.iter (fun x -> Common.push2 x _matching_tokens)

let print_simple_match tokens_matched_code =
  print_match [] [] tokens_matched_code


(* a layer need readable path, hence the ~root argument *)
let gen_layer ~root file =
  pr2 ("generating layer in " ^ file);

  let root = Common.relative_to_absolute root in

  let toks = !_matching_tokens in
  let kinds = ["m" (* match *), "red"] in
  
  let files_and_lines = toks +> List.map (fun tok ->
    let file = Ast.file_of_info tok in
    let line = Ast.line_of_info tok in
    let file' = Common.relative_to_absolute file in 
    Common.filename_without_leading_path root file', line
  )
  in
  let group = Common.group_assoc_bykey_eff files_and_lines in
  let layer = { Layer_code.
    kinds = kinds;
    files = group +> List.map (fun (file, lines) ->
      let lines = Common.uniq lines in
      (file, { Layer_code.
               micro_level = (lines +> List.map (fun l -> l, "m"));
               macro_level =  if null lines then [] else ["m", 1.];
      })
    );
  }
  in
  Layer_code.save_layer layer file;
  ()
  

  
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
                let matched_tokens = Lib_parsing_php.ii_of_any (Lvalue x) in
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
                let matched_tokens = Lib_parsing_php.ii_of_any (Expr x) in
                matches_with_env +> List.iter (fun env ->
                  print_match !mvars env matched_tokens
                )
              end
            );
          }
    in
    ast +> List.iter (fun top ->
      (* opti ? dont analyze func if no constant in it ?*)
      (V.mk_visitor hook) (Toplevel top)
    );
   
  );
  !layer_file +> Common.do_option (fun file ->

    let root = Common.common_prefix_of_files_or_dirs xs in
    gen_layer ~root file
  );
  ()

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
    (V.mk_visitor hook) (Program ast);
    
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
        (fun vout -> vout (Body body)) in
      let vars_and_assigns = Lib_parsing_php.get_vars_assignements 
        (fun vout -> vout (Body body)) in

      let returns = Lib_parsing_php.get_returns 
        (fun vout -> vout (Body body)) in
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
    (V.mk_visitor hook) (Program ast);
    

  )


(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let sgrep_extra_actions () = [
  "-find_object_ref", " <files>",
  Common.mk_action_n_arg (test_find_object_ref_parameter);
  "-find_bad_static_arrays", " <files_or_dira>",
  Common.mk_action_n_arg (test_find_bad_static_arrays);
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 sgrep_extra_actions()++
 Test_parsing_php.actions()++
 Test_program_lang.actions()++
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

    "-gen_layer", Arg.String (fun s -> layer_file := Some s),
    " <file> save result in pfff layer file";

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
