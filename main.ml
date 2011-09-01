(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)

open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* A "driver" of the different parsers in pfff *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this 
 * program also depends on external files ?
 *)

let verbose = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action xs = 
  raise Todo 

(*****************************************************************************)
(* Extra Actions *)
(*****************************************************************************)
let action1 () = 
  raise Todo

let test_json_pretty_printer file =
  let json = Json_in.load_json file in
  let s = Json_io.string_of_json json in
  pr s

module V = Visitor_ml
module G = Graph
open Ast_ml

(* filename in readable path *)
type ml_graph = Common.filename Graph.graph

let pfff_gephi_dependencies dir output =
  let root = Common.realpath dir in
  let files = 
    Lib_parsing_ml.find_ml_files_of_dir_or_files [root]
    +> Common.exclude (fun file ->
      (* less: could also do a pfff_dependencies that just care about mli
       * like my make doti
       *)
      file =~ ".*.mli$" ||
      file =~ ".*external/.*"
    )
  in
  let tree = Treemap.tree_of_dirs_or_files 
    ~file_hook:(fun f -> ())
    files in
  let tree = tree +> Common.map_tree
    ~fnode:(fun f -> Common.filename_without_leading_path root f)
    ~fleaf:(fun (f, _) -> Common.filename_without_leading_path root f)
  in
  
  let g = G.create () in
  let h_module_to_file = Hashtbl.create 101 in

  files +> List.iter (fun file ->
    let realpath = Common.filename_without_leading_path root file in
    g +> G.add_vertex_if_not_present realpath;
    let m = Module_ml.module_name_of_filename realpath in
    Hashtbl.add h_module_to_file m realpath;
  );
  let _ = tree +> Common.map_tree
    ~fnode:(fun dir -> g +> G.add_vertex_if_not_present dir)
    ~fleaf:(fun f -> f)
  in

  files +> Common.index_list_and_total +> List.iter (fun (file, i, total) ->
    pr2 (spf "processing: %s (%d/%d)" file i total);
    let realpath = Common.filename_without_leading_path root file in
    let ast = 
      Common.save_excursion Flag_parsing_ml.show_parsing_error true (fun ()->
        Parse_ml.parse_program file 
      )
    in

    let add_edge_if_existing_module s =
      if Hashtbl.mem h_module_to_file s
      then 
        (match Hashtbl.find_all h_module_to_file s with
        | [realpath2] ->
            (* todo? do weighted graph? but then if do some pattern matching
             * on 20 constructors, is it more important than
             * 2 functions calls? Need to differentiate those different
             * use of the qualifier
             *)
            g +> G.add_edge realpath realpath2
        | _ -> ()
        )
      else pr2_once (spf "PB: could not find %s" s)
    in

    let visitor = V.mk_visitor { V.default_visitor with
      V.kmodule_expr = (fun (k, _) x ->
        (match x with
        | ModuleName (qu, (Name (s,_))) ->
            add_edge_if_existing_module s
        | _ -> ()
        );
        k x
      );
      V.kitem = (fun (k, _) x ->
        (match x with
        | Open (_tok, (qu, (Name (s,_)))) ->
            add_edge_if_existing_module s
        | _ -> ()
        );
        k x
      );
      V.kqualifier = (fun (k, _) xs ->
        (match xs with 
        | [Name (s, _), _tok] ->
            add_edge_if_existing_module s
        | _ -> ()
        );
        k xs
      );
    }
    in
    visitor (Program ast);
  );
  g +> Gephi.graph_to_gefx 
    ~str_of_node:(fun s -> s)
    ~tree:None
    ~output;
  ()


  
(* ---------------------------------------------------------------------- *)
let pfff_extra_actions () = [
  "-json_pp", " <file>",
  Common.mk_action_1_arg test_json_pretty_printer;
  
  "-layer_stat", " <file>",
  Common.mk_action_1_arg Test_program_lang.layer_stat;

  "-pfff_gephi_dependencies", " <dir> <output>",
  Common.mk_action_2_arg pfff_gephi_dependencies;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
  pfff_extra_actions() ++
  Test_parsing_ml.actions()++
  Test_parsing_php.actions()++
  Test_parsing_js.actions()++
  Test_parsing_cpp.actions()++
  Test_parsing_nw.actions()++
  Test_parsing_lisp.actions()++
  Test_parsing_hs.actions()++
  Test_parsing_python.actions()++
  Test_parsing_csharp.actions()++
  Test_parsing_java.actions()++
  Test_parsing_erlang.actions()++
  Test_mini_php.actions()++
  Test_parsing_text.actions()++
  Test_parsing_html.actions()++
  Test_parsing_css.actions()++
  Test_parsing_web.actions()++

  Test_analyze_cpp.actions () ++
  []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
  ] ++
  Flag_parsing_php.cmdline_flags_verbose () ++
  Flag_parsing_cpp.cmdline_flags_verbose () ++

  Flag_parsing_php.cmdline_flags_debugging () ++
  Flag_parsing_cpp.cmdline_flags_debugging () ++

  Flag_parsing_php.cmdline_flags_pp () ++
  Flag_parsing_cpp.cmdline_flags_macrofile () ++

  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_other () ++

  [
    "-version",   Arg.Unit (fun () -> 
      pr2 (spf "pfff version: %s" Config.version);
      exit 0;
    ), 
    "  guess what";

    (* this can not be factorized in Common *)
    "-date",   Arg.Unit (fun () -> 
      pr2 "version: $Date: 2011/03/26 00:44:57 $";
      raise (Common.UnixExit 0)
    ), 
    "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 

  (* Common_extra.set_link(); 
     let argv = Features.Distribution.mpi_adjust_argv Sys.argv in
  *)

  let usage_msg = 
    "Usage: " ^ basename Sys.argv.(0) ^ 
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
