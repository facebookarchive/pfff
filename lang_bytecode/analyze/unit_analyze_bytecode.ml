open Common

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let verbose = false

(* mostly copy paste of Unit_analyze_ml.prolog_query, maybe could
 * factorize stuff and put in h_program-lang/prolog_code.ml at some point
 *)
let prolog_query ~files query =

  let tmp_dir = Filename.temp_file (spf "prolog_class-%d" (Unix.getpid())) "" in
  Unix.unlink tmp_dir;
  (* who cares about race *)
  Unix.mkdir tmp_dir 0o755;
  Common.finalize (fun () ->

    (* generating .class files *)
    files +> List.iter (fun (filename, content) ->
      Common.write_file ~file:(Filename.concat tmp_dir filename) content
    );
    let extra_args = "" in
    Common.command2 (spf "cd %s; javac %s %s"
                       tmp_dir
                       extra_args
                       (* dependency order pbs? assume the given list of files
                        * is ordered for javac to work, which means generic
                        * files first and main files at the end.
                        *)
                       (files +> List.map fst +> Common.join " "));
    let skip_list = [] in
    let graph_code_java = 
      Some (Graph_code_java.build ~verbose:verbose ~only_defs:true 
              tmp_dir skip_list) 
    in
    let g = 
      Graph_code_bytecode.build ~verbose:verbose ~graph_code_java 
        tmp_dir skip_list in
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
"analyze_bytecode" >::: [

(*****************************************************************************)
(* Prolog queries *)
(*****************************************************************************)

 "prolog_bytecode" >::: ([

   "kind" >:: (fun () ->
     let files = [
"Foo.java", "
class Foo {
  static public void f() { }
}
";
"Bar.java", "
class Bar {
  static public void g() { }
}
";] in
     assert_equal 
       ["class"]  (prolog_query ~files "kind('Foo', X), writeln(X)");
     assert_equal 
       ["method"]  (prolog_query ~files "kind(('Bar','g'), X), writeln(X)");
   );

   "at" >:: (fun () ->
     let files = [
"Foo.java", " // line 1
              // line 2
class Foo {   // line 3
  class NestedFoo { // line 4
  }
}
";] in
     assert_equal
       ["3"] (prolog_query ~files "at('Foo', _, X), writeln(X)");
     (* todo: at some point we need to also expose the original java entity
      * name in the bytecode codegraph.
      *)
     assert_equal
       ["4"] (prolog_query ~files "at('Foo\\$NestedFoo', _, X), writeln(X)");
   );
 ])

(*****************************************************************************)
(* Postlude *)
(*****************************************************************************)
]
