
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

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* 
 * A syntactical checker for PHP.
 * 
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* action mode *)
let action = ref ""

let rank = ref true

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* ranking errors, inspired by Engler slides *)
let rank_errors errs = 
  errs |> List.map (fun x ->
    x,
    match x with
    | Error_php.UnusedVariable (_, Scope_php.Local) -> 10
    | _ -> 0
  ) |> Common.sort_by_val_highfirst |> Common.map fst

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2_dbg s = 
  if !verbose then Common.pr2 s
 
(*****************************************************************************)
(* Main action *)
(*****************************************************************************)
let main_action xs =

  (* todo: build info about builtins, so when call to preg_match,
   * know that this function takes things via reference.
   *
   * also make it possible to take a db in parameter so 
   * for other functions, we can also get their prototype.
   *)

  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  Flag_parsing_php.show_parsing_error := false;
  Flag_parsing_php.verbose_lexing := false;
  files +> List.iter (fun file ->
    pr2_dbg (spf "processing: %s" file);

    let (ast2, _stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in
    Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

    Check_variables_php.check_and_annotate_program ast;

    (*
      Check_unused_var_php.check_program ast;
      Checking_php.check_program ast;
      Check_scope_use_php.check_program ast;
      Check_unused_var_php.check_program ast;
    *)
  );

  let errs = !Error_php._errors |> List.rev in
  let errs = 
    if !rank
    then rank_errors errs |> Common.take_safe 20 
    else errs
  in

  errs |> List.iter (fun err ->
    pr (Error_php.string_of_error err);
  );
  pr2 (spf "total errors = %d" (List.length !Error_php._errors));

  (* most recurring probably false positif *)
  let hcount_str = Common.hash_with_default (fun() -> 0) in

  !Error_php._errors |> List.iter (fun err ->
    match err with
    | Error_php.UnusedVariable (dname, scope) ->
        let s = Ast.dname dname in
        hcount_str#update s (fun old -> old+1);
    | _ -> ()
  );
  pr2 "top 10 most recurring unused variable name";
  hcount_str#to_list |> Common.sort_by_val_highfirst |> Common.take_safe 10 
   |> List.iter (fun (s, cnt) -> 
        pr2 (spf " %s -> %d" s cnt)
      );
  ()

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(* the position in the name below correspond to the function at the call site *)
type warning =
  | UndefinedFunction of Ast_php.name
  | UnableToDetermineDef of Ast_php.name
  | WrongKeywordArgument of Ast_php.dname * Ast_php.expr * Ast_php.name *
                     Ast_php.parameter * Ast_php.func_def 
  | TooManyArguments of Ast_php.name * Ast_php.func_def
  | TooFewArguments  of Ast_php.name * Ast_php.func_def

let report_warning = function
  | UndefinedFunction(funcname) ->
      pr2 (spf "Warning: at %s
  function %s is undefined"
          (Ast.string_of_info (Ast.info_of_name funcname))
          (Ast.name funcname)
      )
  | UnableToDetermineDef(funcname) ->
      pr2 (spf "Warning: at %s
  function %s is defined several times; unable to find which one applies"
          (Ast.string_of_info (Ast.info_of_name funcname))
          (Ast.name funcname)
      )
  | WrongKeywordArgument(dn, expr, funcname, param, def) ->
      pr2 (spf "Warning: at %s
  the assignment '$%s=%s' in the argument list of this call to '%s()' looks like a keyword argument, but the corresponding parameter in the definition of '%s' is called '$%s'
  function was declared: %s"
          (Ast.string_of_info (Ast.info_of_dname dn)) 
          (Ast.dname dn) (Unparse_php.string_of_expr expr)
          (Ast.name funcname) (Ast.name funcname)
          (Ast.dname param.p_name) 
          (Ast.string_of_info (Ast.info_of_name def.f_name))
      )
  | TooManyArguments(funcname, def) ->
      pr2 (spf "Warning: at %s
  function call %s has too many arguments
  function was declared: %s"
          (Ast.string_of_info (Ast.info_of_name funcname))
          (Ast.name funcname)
          (Ast.string_of_info (Ast.info_of_name def.f_name))
      )
  | TooFewArguments(funcname, def) ->
      pr2 (spf "Warning: at %s
  function call %s has too few arguments
  function was declared: %s"
          (Ast.string_of_info (Ast.info_of_name funcname))
          (Ast.name funcname)
          (Ast.string_of_info (Ast.info_of_name def.f_name))
      )

let test_jeannin file =
  (* usage for tests/bugs/wrong_keyword_args.php:
   *   ./pfff_db tests/bugs/  # building the database
   *   ./scheck_php -jeannin tests/bugs/wrong_keyword_args.php
   *)
  let dir = "/tmp/pfff_db/" in
    (* TODO: we should probably put the db in a better place *)
  let ast = Parse_php.parse_program file in
  Database_php.with_db ~metapath:dir (fun db ->

    let visitor = 
      V.mk_visitor { V.default_visitor with
        V.klvalue = (fun (k, _) x ->
          match Ast.untype x with
          | FunCallSimple (funcname, args) ->
              (let ids = Database_php.function_ids__of_string 
                       (Ast.name funcname) db in
               match ids with
              | [] -> 
                  report_warning (UndefinedFunction funcname)
              | _ :: _ :: _ ->
                  (* a function with the same name is defined at different places *)
                  (* TODO: deal with functions defined several times *)
                  report_warning (UnableToDetermineDef funcname)
              | [id] -> 
                  let def = match Database_php.ast_of_id id db with
                    | Ast_entity_php.Function(def) -> def
                    | _ -> raise Impossible
                  in
                  let rec aux params args =
                    match (params, args) with
                    | [], [] -> ()
                    | [], y::ys -> 
                        report_warning (TooManyArguments(funcname, def));
                        aux [] ys
                    | x::xs, [] ->
                        (match x.p_default with
                        | None -> 
                            report_warning (TooFewArguments(funcname, def));
                        | Some _ -> ()
                        );
                        aux xs []
                    | x::xs, y::ys ->
                        (match y with
                        | Arg(Assign((Var(dn, _), _),_ , expr), _) ->
                            if not (Ast.dname dn =$= Ast.dname x.p_name)
                            then 
                              report_warning 
                                (WrongKeywordArgument(dn, expr, funcname, 
                                                      x, def))
                        | _ -> ()
                        );
                        aux xs ys
                  in

                  let params = def.f_params in
                  aux (Ast.uncomma (Ast.unparen params))
                      (Ast.uncomma (Ast.unparen args))
              );
              k x
          | _ -> k x
        );
      }
    in
    ast |> visitor.V.vprogram
  )

(*---------------------------------------------------------------------------*)
(* the command line flags *)
(*---------------------------------------------------------------------------*)
let scheck_extra_actions () = [
  "-jeannin", " <file>",
  Common.mk_action_1_arg test_jeannin;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 scheck_extra_actions()++
 Test_parsing_php.actions()++
 []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
    "-strict", Arg.Set Error_php.strict, 
    " ";
    "-no_rank", Arg.Clear rank, 
    " ";
  ] ++
  Flag_parsing_php.cmdline_flags_pp () ++
  Flag_analyze_php.cmdline_flags_verbose () ++
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
    pr2 "version: $Date: 2010/04/25 00:44:57 $";
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
