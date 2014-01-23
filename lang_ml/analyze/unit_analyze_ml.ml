open Common
open OUnit

open Ast_ml
module E = Database_code
module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let verbose = false

let with_graph ~files f =
  Common2.with_tmp_dir (fun tmp_dir ->

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
    f tmp_dir g
  )


let prolog_query ~files query =
  with_graph ~files (fun tmp_dir g ->
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

 ]);

(*****************************************************************************)
(* Codegraph *)
(*****************************************************************************)
   "codegraph_ml" >::: [
     "basic def/uses" >:: (fun () ->
       let file_content = "
let foo () = ()
let bar () = foo ()
"
       in
       with_graph ~files:["foo.ml", file_content] (fun tmp_dir g ->

         let src = ("Foo.foo", E.Function) in
         let pred = G.pred src G.Use g in
         assert_equal
           ~msg:"it should link the use of a function to its def"
           ["Foo.bar", E.Function]
           pred;
       )
     );
   ];

(*****************************************************************************)
(* Coverage *)
(*****************************************************************************)
   "coverage_ml" >::: ([
     
     "basename to readable" >:: (fun () ->
       let dummy_coverage = { Coverage_code.
                              covered_sites = []; all_sites = []
                            }
       in
       let cover = [
         ("unit_analyze_ml.ml", dummy_coverage);
         ("coverage_ml.ml", dummy_coverage);
       ]
       in
       let root = Filename.concat Config_pfff.path "lang_ml" in
       let cover' = 
         Coverage_ml.basename_coverage_to_readable_coverage cover root
         +> List.map fst
       in
       assert_equal
         ~msg:"it should map basename'd files to readable paths"
         ["analyze/unit_analyze_ml.ml";
          "analyze/coverage_ml.ml";
         ]
         cover'
     );
   ]);


(*****************************************************************************)
(* Postlude *)
(*****************************************************************************)
]
