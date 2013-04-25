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

module Ast2 = Ast_js
module V2 = Visitor_js

module PI = Parse_info

open Parse_info

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* 
 * A syntactical patch for PHP. https://github.com/facebook/pfff/wiki/Spatch
 * 
 * opti: git grep xxx | xargs spatch_php ...
 * 
 * 
 * Alternatives: 
 *  - you could also just write a sgrep that put a special mark to the 
 *    place where it matched and then do the transformation using an 
 *    emacs macro leveraging those marks.
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

let case_sensitive = ref false

let apply_patch = ref false
(* too experimental for now *)
let pretty_printer = ref false

let spatch_file = ref ""
let sed_string = ref ""

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2 s = 
  if !verbose then Common.pr2 s

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Some helper functions when using the low-level transformation API of pfff.
 * You should use the spatch DSL if you can.
 *)
type transformation = {
  (* Works by side effect on the tokens in the AST, see the transfo field.
   * It returns a boolean indicating whether any transformation was done.
   *)
  trans_func: Ast_php.program -> bool;
  (* optimisation; if a file does not contain certain keywords we don't
   * even have to parse it
   *)
  grep_keywords: string list option;
}

let apply_transfo transfo xs =

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in
  let pbs = ref [] in
  (* xhp and transformation was not mixing well, but now it's better
   * thanks to builtin xhp support
   *)
  Flag_parsing_php.show_parsing_error := false;
  Flag_parsing_php.verbose_lexing := false;

  files +> Common_extra.progress (fun k -> List.iter (fun file ->
    let file = Common2.relative_to_absolute file in
    pr2 (spf "processing: %s" file);
    k();

    let worth_trying = 
      match transfo.grep_keywords with
      | None -> true
      | Some xs -> Common2.contain_any_token_with_egrep xs file
    in
    if not worth_trying then ()
    else
    try (
    let (ast2) = 
      try 
        Parse_php.parse file +> fst
      with Parse_php.Parse_error err ->
        Common.pr2 (spf "warning: parsing problem in %s" file);
        []
    in
    let ast = Parse_php.program_of_program2 ast2 in
    let was_modified = transfo.trans_func ast in

    (* old: 
     * let patch = Patch.generate_patch !edition_cmds 
     * ~filename_in_project:file file in
     * patch +> List.iter pr
     *)

    if was_modified then begin 
      let s = Unparse_php.string_of_program2_using_transfo ast2 in
    
      let tmpfile = Common.new_temp_file "trans" ".php" in
      Common.write_file ~file:tmpfile s;
      
      let diff = Common2.unix_diff file tmpfile in
      diff +> List.iter pr;

      if !apply_patch 
      then Common.write_file ~file:file s;
    end
    ) with exn ->
      Common.push2 (spf "PB with %s, exn = %s" file (Common.exn_to_s exn)) pbs;
  ));
  !pbs +> List.iter Common.pr2
 
(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs =
  let spatch_file = 
    match !spatch_file, !sed_string with
    | "", "" ->
        failwith "I need a semantic patch file; use -f"
    | "", s ->
        (* does not handle nested /, does not handle s# alternate
         * syntax as in perl, does not handle much ...
         *)
        if s =~ "s/\\(.*\\)/\\(.*\\)/"
        then begin
          let (before, after) = Common.matched2 s in
          let tmpfile = Common.new_temp_file "spatch" ".spatch" in
          Common.with_open_outfile tmpfile (fun (pr, _chan) ->
            pr (spf "- %s\n" before);
            if after <> "" 
            then pr (spf "+ %s\n" after);
          );
          tmpfile
        end 
        else failwith ("wrong format, use s/.../.../ not: " ^ s)
    | s, "" ->
        !spatch_file 
    | s1, s2 ->
        failwith "Can't use -f and -e at the same time"
  in

  Logger.log Config_pfff.logger "spatch" (Some (Common.read_file spatch_file));

  (* old: let pattern = dumb_spatch_pattern in *)
  let pattern = 
    Spatch_php.parse spatch_file in

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  files +> Common_extra.progress ~show:!verbose (fun k -> 
   List.iter (fun file->
    k();
    try (
    let resopt = 
      Spatch_php.spatch ~case_sensitive:!case_sensitive pattern file
    in
    resopt +> Common.do_option (fun (s) ->

      pr2 (spf "transforming: %s" file);

      let tmpfile = Common.new_temp_file "trans" ".php" in
      Common.write_file ~file:tmpfile s;

      if !pretty_printer
      then Unparse_pretty_print_mix.pretty_print_when_needit
             ~oldfile:file ~newfile:tmpfile;
      
      let diff = Common2.unix_diff file tmpfile in
      diff +> List.iter pr;
      if !apply_patch 
      then Common.write_file ~file:file (Common.read_file tmpfile);
    )
    ) with Parse_php.Parse_error tok ->
      failwith ("PARSING PB: " ^ Parse_info.error_message_info tok);
  ))

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(* -------------------------------------------------------------------------*)
(* to test *)
(* -------------------------------------------------------------------------*)

(* see also demos/simple_refactoring.ml *)
let simple_transfo xs = 

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  Flag_parsing_php.show_parsing_error := false;
  Flag_parsing_php.verbose_lexing := false;
  files +> List.iter (fun file ->
    pr2 (spf "processing: %s" file);

    let (ast2, _stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in

    let hook = { V.default_visitor with
      V.klvalue = (fun (k, _) x ->
        match x with
        | FunCallSimple((Name ("foo", info_foo)), (lp, args, rp)) ->
            pr2 "found match";
            
            let ii = Lib_parsing_php.ii_of_any (Lvalue x) in
            ii +> List.iter (fun info ->
              info.transfo <- Remove
            );
            info_foo.transfo <- Replace (AddStr "1");
            ()
        | _ -> k x
      );
    }
    in
    (V.mk_visitor hook) (Program ast);

    let s = Unparse_php.string_of_program2_using_transfo ast2 in
    
    let tmpfile = Common.new_temp_file "trans" ".php" in
    Common.write_file ~file:tmpfile s;
    
    let diff = Common2.unix_diff file tmpfile in
    diff +> List.iter pr;
  );
  ()
(* -------------------------------------------------------------------------*)
(* Trailing comma transformation *)
(* -------------------------------------------------------------------------*)
let all_different xs = 
  List.length xs = (xs +> Common.sort +> Common2.uniq +> List.length)

(* todo: 
 * - julien thinks we should transform even if some commas
 *   are on the same line, as long as the last ')' is on a different line.
 * - we should also transform function decl and use() lists.
 *)
let add_trailing_comma_multiline_funcalls ast =
  let was_modified = ref false in
  let visitor = V.mk_visitor { V.default_visitor with
    V.karguments = (fun (k, _) (lp, args, rp) ->
      if List.length args >= 1 then begin
        let (args, commas) = Common.partition_either (fun x -> x) args in
        let lines_commas = 
          commas +> List.map Ast_php.line_of_info
        in
        let line_rp = Ast_php.line_of_info rp in
        let last_expr = Common2.list_last args in
        let ii = Lib_parsing_php.ii_of_any (Argument last_expr) in
        let (_min, max) =
          Parse_info.min_max_ii_by_pos ii in
        let line_last_expr = Ast_php.line_of_info max in

        if List.length args > 2 && 
          all_different (line_last_expr::line_rp::lines_commas) then
          begin
            max.transfo <- AddAfter (AddStr ",");
            was_modified := true;
          end;
      end;
      k (lp, args, rp)
    );
  }
  in
  visitor (Program ast);
  !was_modified

let trailing_comma_transfo = {
  trans_func = add_trailing_comma_multiline_funcalls;
  grep_keywords = None;
}

(* -------------------------------------------------------------------------*)
(* An example of an XHP transformation *)
(* -------------------------------------------------------------------------*)

(*
 * The transformation below can be encoded via this syntactical patch:
 * 
 * <ui:section-header
 * -   border=X
 * ></ui:section-header>
 * 
 * There are nevertheless currenty a few limitations in spatch which
 * will cause this syntactical patch to fail to transform all the
 * relevant code (e.g. because of some XHP isomorphisms not yet
 * handled by spatch). In the mean time the code below uses the low-level
 * transformation API of pfff to express the same transformation;
 * it can be used as a reference for spatch to compare with.
 *)

let remove_border_attribute ast =
  let was_modified = ref false in

  (* $ ./pfff -dump_php_ml tests/php/spatch/border_var.php 
   *
   * [StmtList(
   *  [ExprStmt(
   *    (Assign((Var(DName(("a", i_1)), Ref(NoScope)), tlval_2), i_3,
   *       (XhpHtml(
   *          Xhp((["ui"; "section-header"], i_4),
   *            [(("href", i_5), i_6,
   *              XhpAttrString(i_7, [EncapsString(("foo", i_8))], i_9));
   *             (("border", i_10), i_11,
   *              XhpAttrString(i_12, [EncapsString(("1", i_13))], i_14))],
   *            i_15, [XhpText(("
   *   This is nuts
   *   ", i_16))],
   *            (Some(["ui"; "section-header"]), i_17))),
   *        t_18)),
   *     t_19), i_20)]); FinalDef(i_21)]
   *)
  let visitor = V.mk_visitor { V.default_visitor with
    V.kxhp_html = (fun (k, _) x ->
      match x with
      | Xhp ( (["ui"; "section-header"], _), attributes, _, _, _)
      | XhpSingleton ( (["ui"; "section-header"], _), attributes, _) ->

          attributes +> List.iter (fun attr ->
            match attr with
            | (("border", _tok_border), _tok_equal, _xhp_attr_value) ->
                was_modified := true;
                let tokens = Lib_parsing_php.ii_of_any (XhpAttribute attr) in
                tokens +> List.iter (fun tok ->
                  tok.transfo <- Remove;
                );
            | _ -> ()
          );
          (* recurse *)
          k x
      (* recurse *)
      | _ -> k x
    );
  }
  in
  visitor (Program ast);
  !was_modified

let remove_border_attribute_transfo = {
  trans_func = remove_border_attribute;
  grep_keywords = Some ["border"];
}

(* -------------------------------------------------------------------------*)
(* Add action attribute to <ui:form> *)
(* -------------------------------------------------------------------------*)

(* 
 * Here is the spatch (disjunction are currently not supported in spatch
 * so have to do things manually):
 * 
 *   <ui:form
 * (
 *  action=...
 * |
 *  + action="#"
 * )
 *  >
 * 
 * todo: maybe we could generalize this transformation and add to
 * spatch some command lines like: 
 *  *  -add_xhp_attribute_if_not_there "ui:form" "action='#'"
 *  *  -remove_xhp_attribute "ui:form" "border"
 * a la lex-pass. Or maybe it's better to add support in the DSL directly
 * so one can write the syntactical patch above.
 *)
let add_action_ui_form_transfo_func ast = 
  let was_modified = ref false in
  let visitor = V.mk_visitor { V.default_visitor with
    V.kxhp_html = (fun (k, _) xhp ->
      (* mostly copy paste of: pfff -dump_php tests/.../ui_form.php *)
      match xhp with
      | XhpSingleton((["ui"; "form"], info_tag), attributes, _)
      | Xhp ((["ui"; "form"], info_tag), attributes, _, _, _) 
        ->
          if not (attributes +> 
                  List.exists (fun ((attr_name,_), _tok, attr_val) ->
                    attr_name = "action"
          )) 
          then begin
            was_modified := true;
            info_tag.PI.transfo <- PI.AddAfter (PI.AddStr " action=\"#\"");
          end;
          k xhp
      | _ -> k xhp (* call the continuation *)
    );
  }
  in
  visitor (Program ast);
  !was_modified

let add_action_ui_form_transfo = {
  trans_func = add_action_ui_form_transfo_func;
  grep_keywords = Some ["ui:form"];
}


(*---------------------------------------------------------------------------*)
(* pretty printer testing *)
(*---------------------------------------------------------------------------*)
let test_pp file =
  let tokens = Parse_php.tokens file in
  let ast = Parse_php.parse_program file in

  let ast = Ast_pp_build.program_with_comments tokens ast in

  let buf = Buffer.create 256 in
  let env = Pp2.empty (Buffer.add_string buf) in
  Pretty_print.program_env env ast;
  let s = Buffer.contents buf in

  let tmp_file = Common.new_temp_file "pp" ".php" in
  Common.write_file ~file:tmp_file s;
  let xs = Common2.unix_diff file tmp_file in
  xs +> List.iter pr2

(*---------------------------------------------------------------------------*)
(* juju refactorings *)
(*---------------------------------------------------------------------------*)

let juju_refactoring spec_file =
  let xs = Refactoring_code.load spec_file in

  (* Group by file because we need to do all the transfo on a file
   * before unparsing as the refactoring spec use line/col position
   * that refers to the original file
   *)
  let xxs = 
    xs +> List.map (fun x -> x.Refactoring_code.file, x)
      +> Common.group_assoc_bykey_eff
  in
  xxs +> List.iter (fun (file, refactorings) ->
    let (ast2, _stat) = Parse_php.parse file in
    let s = Refactoring_code_php.refactor refactorings ast2 in

    let tmpfile = Common.new_temp_file "trans" ".php" in
    Common.write_file ~file:tmpfile s;

    if !pretty_printer
    then Unparse_pretty_print_mix.pretty_print_when_needit
      ~oldfile:file ~newfile:tmpfile;
      
    let diff = Common2.unix_diff file tmpfile in
    diff +> List.iter pr;
    if !apply_patch 
    then Common.write_file ~file:file (Common.read_file tmpfile);
  );
  ()

(*---------------------------------------------------------------------------*)
(* regression testing *)
(*---------------------------------------------------------------------------*)
open OUnit
let test () =
  let suite = "spatch" >::: Unit_matcher_php.spatch_unittest in
  OUnit.run_test_tt suite +> ignore;
  ()

(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let spatch_extra_actions () = [
  (* see also demos/simple_refactoring.ml *)
  "-simple_transfo", " <files_or_dirs>",
  Common.mk_action_n_arg (simple_transfo);

  "-remove_border_attribute", " <files_or_dirs>",
  Common.mk_action_n_arg (apply_transfo remove_border_attribute_transfo);
  "-add_action_ui_form", " <files_or_dirs>",
  Common.mk_action_n_arg (apply_transfo add_action_ui_form_transfo);
  "-add_trailing_comma", " <files_or_dirs>",
  Common.mk_action_n_arg (apply_transfo trailing_comma_transfo);

  "-juju_refactoring", " <file>",
  Common.mk_action_1_arg juju_refactoring;

  "-test", " run regression tests",
  Common.mk_action_0_arg test;
  "-pp", "",
  Common.mk_action_1_arg test_pp;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 spatch_extra_actions()++
 []

let options () = 
  [
    "-f", Arg.Set_string spatch_file, 
    " <spatch_file>";
    "-e", Arg.Set_string sed_string, 
    " <s/before/after/>, sed mode";
    "--apply-patch", Arg.Set apply_patch, 
    " ";
    "--pretty-printer", Arg.Set pretty_printer, 
    " reindent the modified code";
    "--case-sensitive", Arg.Set case_sensitive, 
    " match code in a case sensitive manner";
    "--verbose", Arg.Set verbose, 
    " ";
    "-v", Arg.Set verbose, 
    " shortcut for --verbose";
  ] ++
  (* Flag_parsing_php.cmdline_flags_pp () ++ *)
  Common.options_of_actions action (all_actions()) ++
  Common2.cmdline_flags_devel () ++
  [
  "-version",   Arg.Unit (fun () -> 
    Common.pr2 (spf "spatch_php version: %s" Config_pfff.version);
    exit 0;
  ), 
    "  guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  Common_extra.set_link ();

  let usage_msg = 
    spf "Usage: %s [options] <file or dir> \nDoc: %s\nOptions:"
      (Common2.basename Sys.argv.(0))
      "https://github.com/facebook/pfff/wiki/Spatch"
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
