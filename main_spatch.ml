
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

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* 
 * A syntactical patch for PHP.
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

let verbose = ref true

let apply_patch = ref false

let spatch_file = ref ""

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

let apply_transfo (f, keywords_grep_opt) xs =

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  let pbs = ref [] in
  (* xhp and transformation was not mixing well, but now it's better
   * thanks to builtin xhp support
   *)

  Flag_parsing_php.show_parsing_error := false;
  Flag_parsing_php.verbose_lexing := false;

  let nbfiles = List.length files in

  Common.execute_and_show_progress ~show_progress:true nbfiles (fun k ->
  files +> List.iter (fun file ->
    let file = Common.relative_to_absolute file in
    pr2 (spf "processing: %s" file);

    k();

    let worth_trying = 
      match keywords_grep_opt with
      | None -> true
      | Some xs -> Common.contain_any_token_with_egrep xs file
    in
    if not worth_trying then ()
    else
    try (
    let (ast2, _stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in
    Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

    let was_modified = f ast in

    (* old: 
     * let patch = Patch.generate_patch !edition_cmds 
     * ~filename_in_project:file file in
     * patch |> List.iter pr
     *)

    if was_modified then begin 
      let s = Unparse_php.string_of_program2_using_tokens ast2 in
    
      let tmpfile = Common.new_temp_file "trans" ".php" in
      Common.write_file ~file:tmpfile s;
      
      let diff = Common.cmd_to_list (spf "diff -u %s %s" file tmpfile) in
      diff |> List.iter pr;

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
(* 
 ./spatch -c tests/php/spatch/foo.spatch tests/php/spatch/foo.php  
*)

(* just to test the backend part of spatch *)
let (dumb_spatch_pattern: Ast_php.expr) = 
  (* ./pfff -dump_php_ml tests/php/spatch/1.php *)
  let i_1 = {
    pinfo =
      PI.OriginTok(
        { PI.str = "1"; charpos = 6; line = 2; column = 0; 
          file = "tests/php/spatch/1.php"; 
        });
     (* the spatch is to replace every 1 by 42 *)
     transfo = Replace (AddStr "42");
    }
  in
  let t_1 = Ast.noType () in
  (Sc(C(Int(("1", i_1)))), t_1)

(*
 * Here is an example of a spatch file:
 * 
 *    foo(2, 
 * -      bar(2)
 * +      foobar(4)
 *       )
 * 
 * This will replace all calls to bar(2) by foobar(4) when
 * the function call is the second argument of a call to
 * foo where its first argument is 2.
 * 
 * Algorithm to parse a spatch file:
 *  - take lines of the file, index the lines
 *  - replace the + lines by an empty line and remember in a line_env
 *    the line and its index
 *  - remove the - in the first column and remember in a line_env
 *    that is was a minus line
 *  - unlines the filtered lines into a new string 
 *  - call the PHP expr parser on this new string
 *  - go through all tokens and adjust its transfo field using the
 *    information in line_env
 *)
type line_kind = 
  | Context
  | Plus of string
  | Minus

let parse_spatch file =

  let xs = Common.cat file +> Common.index_list_1 in

  let hline_env = Hashtbl.create 11 in

  let ys = xs +> List.map (fun (s, lineno) ->
    match s with
    (* ugly: for now I strip the space after the + because.
     * at some point we need to parse this stuff and
     * add the correct amount of indentation when it's processing
     * a token.
     *)
    | _ when s =~ "^\\+[ \t]*\\(.*\\)" -> 
        let rest_line = Common.matched1 s in
        Hashtbl.add hline_env lineno (Plus rest_line);
        ""
    | _ when s =~ "^\\-\\(.*\\)" ->
        let rest_line = Common.matched1 s in
        Hashtbl.add hline_env lineno Minus;
        rest_line
    | _ ->
        Hashtbl.add hline_env lineno Context;
        s
  )
  in
  let spatch_without_patch_annot = Common.unlines ys in
  (* pr2 spatch_without_patch_annot; *)

  let pattern = 
    Common.save_excursion Flag_parsing_php.sgrep_mode true (fun () ->
    (* ugly *)
    if spatch_without_patch_annot =~ "^[ \t]*<"
    then Parse_php.xhp_expr_of_string spatch_without_patch_annot 
    else Parse_php.expr_of_string spatch_without_patch_annot 
    )
  in

  (* need adjust the tokens in it now *)
  let toks = Lib_parsing_php.ii_of_any (Expr pattern) in

  (* adjust with Minus info *)  
  toks +> List.iter (fun tok ->
    let line = Ast.line_of_info tok in

    (* ugly: right now expr_of_string introduce an extra <?php at
     * the beginning which shifts line number by 1 so have
     * to compensate back here.
     *)
    let line = line - 1 in

    let annot = Hashtbl.find hline_env line in
    (match annot with
    | Context -> ()
    | Minus -> tok.transfo <- Remove;
    | Plus _ -> 
        (* normally impossible since we removed the elements in the
         * plus line, except the newline. should assert it's only newline
         *)
        ()
    );
  );
  (* adjust with the Plus info. We need to annotate the last token
   * on the preceding line or next line.
   * e.g. on
   *     foo(2,
   *   +        42,
   *         3)
   * we could either put the + on the ',' of the first line (as an AddAfter)
   * or on the + on the '3' of the thirdt line (as an AddBefore).
   * The preceding and next line could also be a minus line itself.
   * Also it could be possible to have multiple + line in which
   * case we want to concatenate them together.
   * 
   * TODO: for now I just associate it with the previous line ...
   *)

  let grouped_by_lines = 
    toks +> Common.group_by_mapped_key (fun tok -> Ast.line_of_info tok) in
  let rec aux xs = 
    match xs with
    | (line, toks_at_line)::rest ->
        (* ugly *)
        let line = line - 1 in

        (* if the next line was a +, then associate with the last token
         * on this line
         *)
        (match Common.hfind_option (line+1) hline_env with
        | None -> 
            (* probably because was last line *) 
            ()
        | Some (Plus toadd) ->
            (* todo? what if there is no token on this line ? *)
            let last_tok = Common.last toks_at_line in
            (match last_tok.transfo with
            | Remove -> last_tok.transfo <- Replace (AddStr toadd)
            | NoTransfo -> last_tok.transfo <- AddAfter (AddStr toadd)
            | _ -> raise Impossible
            )
        | Some _ -> ()
        );
        aux rest

    | [] -> ()
  in
  aux grouped_by_lines;

  (* both the ast (here pattern) and the tokens share the same
   * reference so by modifying the tokens we actually also modifed
   * the AST.
   *)
  pattern

let spatch pattern file =
  let was_modifed = ref false in
    
  (* quite similar to what we do in main_sgrep.ml *)
  let (ast2, _stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in
  Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

  let visitor = V.mk_visitor { V.default_visitor with
    (* for now handle only expression patching *)
    V.kexpr = (fun (k, _) x ->
      let matches_with_env =  
        Matching_php.match_e_e pattern  x
      in
      if matches_with_env = []
      then k x
      else begin
        was_modifed := true;
        Transforming_php.transform_e_e pattern x
          (* TODO, maybe could get multiple matching env *)
          (List.hd matches_with_env) 
      end
    );
  }
  in
  visitor (Program ast);

  if !was_modifed 
  then Some (Unparse_php.string_of_program2_using_tokens ast2)
  else None

      


    


let main_action xs =

  if Common.null_string !spatch_file
  then failwith "I need a semantic patch file; use -c";

  let spatch_file = !spatch_file in

  (* old: let pattern = dumb_spatch_pattern in *)
  let pattern = parse_spatch spatch_file in

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in
  files +> Common.index_list_and_total +> List.iter (fun (file, i, total) ->
    pr2 (spf "processing: %s (%d/%d)" file i total);
    let resopt = spatch pattern file in
    resopt +> Common.do_option (fun (s) ->

      let tmpfile = Common.new_temp_file "trans" ".php" in
      Common.write_file ~file:tmpfile s;
      
      let diff = Common.cmd_to_list (spf "diff -u %s %s" file tmpfile) in
      diff |> List.iter pr;
      if !apply_patch 
      then Common.write_file ~file:file s;
    )
  )


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
        match Ast.untype x with
        | FunCallSimple((Name ("foo", info_foo)), (lp, args, rp)) ->
            pr2 "found match";
            
            let ii = Lib_parsing_php.ii_of_any (Lvalue x) in
            ii |> List.iter (fun info ->
              info.transfo <- Ast.Remove
            );
            info_foo.transfo <- Ast.Replace (Ast.AddStr "1");
            ()
        | _ -> k x
      );
    }
    in
    (V.mk_visitor hook) (Program ast);

    let s = Unparse_php.string_of_program2_using_tokens ast2 in
    
    let tmpfile = Common.new_temp_file "trans" ".php" in
    Common.write_file ~file:tmpfile s;
    
    let diff = Common.cmd_to_list (spf "diff -u %s %s" file tmpfile) in
    diff |> List.iter pr;
  );
  ()

(*---------------------------------------------------------------------------*)
(* regression testing *)
(*---------------------------------------------------------------------------*)

let unittest_spatch () =
  let testdir = Filename.concat Config.path "tests/php/spatch/" in
  let expfiles = Common.glob (testdir ^ "*.exp") in
  
  expfiles +> List.iter (fun expfile ->
    if expfile =~ "\\(.*\\)\\([0-9]*\\)\\.exp$" then begin
      let (prefix, variant) = Common.matched2 expfile in
      let spatchfile = prefix ^ ".spatch" in
      let phpfile = prefix ^ variant ^ ".php" in
      pr2 (spf "Testing %s on %s expecting %s" 
              (Filename.basename spatchfile)
              (Filename.basename phpfile)
              (Filename.basename expfile));

      let pattern = parse_spatch spatchfile in
      let resopt = spatch pattern phpfile in

      let file_res = 
        match resopt with
        | None -> phpfile
        | Some s ->
            let tmpfile = Common.new_temp_file "spatch_test" ".php" in
            Common.write_file ~file:tmpfile s;
            tmpfile
      in
      let diff = Common.cmd_to_list (spf "diff -u %s %s" file_res expfile) in
      diff |> List.iter pr;
      if List.length diff > 1
      then failwith (spf "PB with %s" expfile);
    end 
    else failwith ("wrong format for expfile: " ^ expfile)
  )


(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let spatch_extra_actions () = [
  (* see also demos/simple_refactoring.ml *)
  "-simple_transfo", "<files_or_dirs>",
  Common.mk_action_n_arg (simple_transfo);

  "-test", "",
  Common.mk_action_0_arg unittest_spatch;

]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 spatch_extra_actions()++
 Test_parsing_php.actions()++
 []

let options () = 
  [
    "-c", Arg.Set_string spatch_file, 
    " <spatch_file>";
    "-apply_patch", Arg.Set apply_patch, 
    " ";
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
    Common.pr2 (spf "sgrep_php version: %s" Config.version);
    exit 0;
  ), 
    "  guess what";

  (* this can not be factorized in Common *)
  "-date",   Arg.Unit (fun () -> 
    Common.pr2 "version: $Date: 2010/04/25 00:44:57 $";
    raise (Common.UnixExit 0)
    ), 
  "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  Common_extra.set_link ();


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
