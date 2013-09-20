open Common
open OUnit

open Ast_ml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let verbose = false

let prolog_query ~files query =

  let tmp_dir = Filename.temp_file (spf "prolog_ml-%d" (Unix.getpid())) "" in
  Unix.unlink tmp_dir;
  (* who cares about race *)
  Unix.mkdir tmp_dir 0o755;
  Common.finalize (fun () ->

    (* generating .cmt files *)
    files +> List.iter (fun (filename, content) ->
      Common.write_file ~file:(Filename.concat tmp_dir filename) content
    );
    (* otherwise will get many lookup failure when build the graph_code *)
    let extra_args = "-nostdlib -nopervasives" in
    Common.command2 (spf "cd %s; ocamlc -c %s -bin-annot %s"
                       tmp_dir
                       extra_args
                       (* dependency order pbs? assume the given list of files
                        * is ordered for ocamlc to work, which means generic
                        * files first and main files at the end.
                        *)
                       (files +> List.map fst +> Common.join " "));
    let skip_list = [] in
    let g = Graph_code_cmt.build ~verbose:verbose tmp_dir skip_list in
    let facts = Graph_code_prolog.build tmp_dir g in
    let facts_pl_file = Filename.concat tmp_dir "facts.pl" in
    Common.with_open_outfile facts_pl_file (fun (pr_no_nl, _chan) ->
      let pr s = pr_no_nl (s ^ "\n") in
      facts +> List.iter (fun x -> pr (Graph_code_prolog.string_of_fact x))
      );
    let predicates_file = 
      Filename.concat Config_pfff.path "h_program-lang/database_code.pl" in
    if verbose 
    then Common.cat facts_pl_file +> List.iter pr2;
    let cmd =
      spf "swipl -s %s -f %s -t halt --quiet -g \"%s ,fail\""
        facts_pl_file predicates_file query
    in
    let xs = Common.cmd_to_list cmd in
    xs
  ) (fun () ->
    Common.command2 (spf "rm -f %s/*" tmp_dir);
    Unix.rmdir tmp_dir
  )

let unittest = 
"analyze_ml" >::: [

(*****************************************************************************)
(* Database building *)
(*****************************************************************************)
  "building light database" >:: (fun () ->
    let data_dir = Config_pfff.path ^ "/tests/ml/db" in
    let _db = Database_light_ml.compute_database ~verbose [data_dir] in
    ()
  );

(*****************************************************************************)
(* Prolog queries *)
(*****************************************************************************)
 "prolog_ml" >::: ([
   
   "kind" >:: (fun () ->
     let files = [
"pervasives.ml", "
type 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = \"%makemutable\"
";
"foo.ml", "
let f x = x
let g = Pervasives.ref 0
";] in
     assert_equal 
       ["function"]  (prolog_query ~files "kind(('Foo','f'), X), writeln(X)");
     assert_equal 
       ["global"]  (prolog_query ~files "kind(('Foo','g'), X), writeln(X)");
   );

   "at" >:: (fun () ->
     let files = [
"foo.ml", " (* line 1 *)
let f x = x (* line 2 *)
let c = 1   (* line 3 *)
";] in
     assert_equal 
       ["3"]  (prolog_query ~files "at(('Foo','c'), _, X), writeln(X)");
   );

 ])

(*****************************************************************************)
(* Postlude *)
(*****************************************************************************)
]
