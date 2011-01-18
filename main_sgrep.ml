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

open Ast_php

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
      | Some any ->
          let ii = Lib_parsing_php.ii_of_any any in
          Lib_parsing_php.print_match  ~format:Lib_parsing_php.OneLine ii
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
  
  (* todo: could now use Layer_code.simple_layer_of_parse_infos *)
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

  let pattern = 
    Common.save_excursion Flag_parsing_php.sgrep_mode true (fun () ->
    match !pattern_file, !pattern_string with
    | "", "" -> 
        failwith "I need a pattern; use -f or -e";
    | file, _ when file <> "" ->
        Parse_php.parse_any file
    | _, s when s <> ""->
        Parse_php.any_of_string s
    | _ -> raise Impossible
    )
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

      match pattern with
      | Expr (Lv pattern_var, _t) ->
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
      | Expr pattern_expr ->
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
      | Stmt2 pattern ->
          { V.default_visitor with
            V.kstmt = (fun (k, _) x ->
              let matches_with_env =  
                Matching_php.match_st_st pattern x
              in
              if matches_with_env = []
              then k x
              else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
                let matched_tokens = Lib_parsing_php.ii_of_any (Stmt2 x) in
                matches_with_env +> List.iter (fun env ->
                  print_match !mvars env matched_tokens
                )
              end
            );
          }

      | _ -> failwith (spf "pattern not yet supported:" ^ 
                       Export_ast_php.ml_pattern_string_of_any pattern)
    in
      (* opti ? dont analyze func if no constant in it ?*)
    (V.mk_visitor hook) (Program ast)
  );
  !layer_file +> Common.do_option (fun file ->

    let root = Common.common_prefix_of_files_or_dirs xs in
    gen_layer ~root file
  );
  ()

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)
let dump_sgrep_pattern file =
  let any = Parse_php.parse_any file in
  let s = Export_ast_php.ml_pattern_string_of_any any in
  pr s


(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let sgrep_extra_actions () = [
  "-dump_pattern", " <file>",
  Common.mk_action_1_arg dump_sgrep_pattern;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 sgrep_extra_actions()++
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
  (* old: Flag_parsing_php.cmdline_flags_pp () ++ *)
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
