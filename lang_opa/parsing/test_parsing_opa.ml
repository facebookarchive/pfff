open Common

module Ast = Ast_opa
module Flag = Flag_parsing_opa
module TH = Token_helpers_opa

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_opa file = 
  if not (file =~ ".*\\.opa") 
  then pr2 "warning: seems not an OPA file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.debug_lexer := true;

  let toks = Parse_opa.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_opa xs =

  let fullxs = Lib_parsing_opa.find_opa_files_of_dir_or_files xs in

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);

    Common.save_excursion Flag.error_recovery true (fun () ->
    Common.save_excursion Flag.verbose_lexing true (fun () ->
      (* old: let _xs = Parse_opa.parse file in *)

      let (_, toks) = Parse_opa.parse_just_tokens file in
      let toks = Ast_fuzzy_opa.toks_for_ast_fuzzy toks in
      let tree = Token_views_opa.mk_tree toks in
      let _tree = Ast_fuzzy_opa.mk_tree tree in
      ()
    ))
  );
  ()

let test_token_view_opa file =
  let (_, toks) = Parse_opa.parse_just_tokens file in
  let toks = toks +> Common.exclude (fun t ->
    TH.is_comment t
  )
  in
  let tree = Token_views_opa.mk_tree toks in
  let v = Token_views_opa.vof_tree_list tree in
  let s = Ocaml.string_of_v v in
  pr s

let test_fuzzy_opa file =
  let (_, toks) = Parse_opa.parse_just_tokens file in
  let toks = Ast_fuzzy_opa.toks_for_ast_fuzzy toks in
  let tree = Token_views_opa.mk_tree toks in
  let tree = Ast_fuzzy_opa.mk_tree tree in
  let v = Meta_ast_fuzzy_opa.vof_tree_list tree in
  let s = Ocaml.string_of_v v in
  pr s

let translate_opa dir1 dir2 =
  let fullxs = Common.cmd_to_list (spf "find %s -name '*.opa'" dir1) in
  Common.command2(spf "mkdir -p %s" dir2);
  fullxs +> List.iter (fun src ->
    pr2 (spf "processing %s" src);
    let readable = Common.readable ~root:dir1 src in
    let dirname = Filename.dirname readable in
    let dest = spf "%s/%s" dir2 readable in
    Common.command2(spf "mkdir -p %s/%s" dir2 dirname);
    try 
      Common.timeout_function 200 (fun () ->
(* otr script:
#!/bin/sh

rm -rf /tmp/opa-translate

~/local/opalang/_build/opa/syntaxHelper.native \
 --parser classic --printer js-like \
 $1 --build-dir /tmp/opa-translate 2> /tmp/translation-report.txt

#cat /tmp/opa-translate/*
#cat /tmp/translation-report.txt
~/local/opa/scripts/import_comment/import_comment --src $1 --dst /tmp/opa-translate/* --out /tmp/res.opa >>/tmp/translation-report.txt 2>&1 

cat /tmp/res.opa

rm -f error.log
rm -rf _tracks
#~/pfff/pfff -dump_opa /tmp/res.opa
*)
        Common.command2(spf "otr %s > %s" src dest);

        let (_, toks) = Parse_opa.parse_just_tokens dest in
        let toks = Ast_fuzzy_opa.toks_for_ast_fuzzy toks in
        let tree = Token_views_opa.mk_tree toks in
        let _tree = Ast_fuzzy_opa.mk_tree tree in
        ()
      )
    with (Timeout | Failure _) as exn ->
      pr2 (spf "PB %s on %s" (Common.exn_to_s exn) src);
  );
  ()

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_opa", "   <file>", 
  Common.mk_action_1_arg test_tokens_opa;
  "-parse_opa", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_opa;
  "-tview_opa", "   <file>", 
  Common.mk_action_1_arg test_token_view_opa;
  "-dump_opa", "   <file>", 
  Common.mk_action_1_arg test_fuzzy_opa;
  "-translate_opa", "   <src> <tgt>", 
  Common.mk_action_2_arg translate_opa;
]
