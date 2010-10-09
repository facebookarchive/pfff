
(* relevant test files exercising source, with term-frequency of 
 * file in the test *)
type tests_coverage = (Common.filename (* source *), tests_score) Common.assoc
 and tests_score = (Common.filename (* a test *) * float) list

type lines_coverage = (Common.filename, file_lines_coverage) Common.assoc
 and file_lines_coverage = {
   covered_call_sites: int list;
   call_sites: int list;
 }

val threshold_working_tests_percentage : float ref

exception NotEnoughWorkingTests

(* Compute coverage information of a set of phpunit test files. 
 * 
 * Note that because this function internally use Xdebug, 
 * the filenames in the coverage data are in a realpath format.
 * 
 * Note also that this function can leverage MPI (which is why
 * it takes the set of test files inside a closure, see 
 * Distribution.map_reduce_lazy).
 * 
 * This may raise NotEnoughWorkingTests.
 *)

val coverage_tests :
  ?phpunit_parse_trace:(Common.filename -> string list -> Phpunit.test_result)->
  ?skip_call:(Xdebug.call_trace -> bool) ->
  php_cmd_run_test:(php_interpreter:string ->
                     Common.filename -> string) ->
  all_test_files:(unit -> Common.filename list) ->
  unit ->

  tests_coverage * 
  (Common.filename * string (* error *)) list (* tests with pbs *)


val lines_coverage_from_tests:  
  ?skip_call:(Xdebug.call_trace -> bool) ->
  ?is_directive_to_filter:(string -> bool) ->
  php_cmd_run_test:(php_interpreter:string -> Common.filename -> string) ->
  all_test_files:Common.filename list ->
  all_files:Common.filename list ->
  unit ->
  lines_coverage


(* for percentage statistics per file *)
val get_all_calls: 
  ?is_directive_to_filter:(string -> bool) ->
  (Visitor_php.visitor_out -> unit) -> (string option * Ast_php.tok) list

val get_all_call_lines_with_sanity_check:
  ?is_directive_to_filter:(string -> bool) ->
  Common.filename -> int list (* covered line according to xdebug *) ->
  int list


val actions: unit -> Common.cmdline_actions
val unittest: OUnit.test

(* input/output *)
val json_of_tests_coverage: tests_coverage -> Json_type.json_type
val json_of_lines_coverage: lines_coverage -> Json_type.json_type

val tests_coverage_of_json: Json_type.json_type -> tests_coverage
val lines_coverage_of_json: Json_type.json_type -> lines_coverage

(* shortcuts *)
val save_tests_coverage: tests_coverage -> Common.filename -> unit
val load_tests_coverage: Common.filename -> tests_coverage

val save_lines_coverage: lines_coverage -> Common.filename -> unit
val load_lines_coverage: Common.filename -> lines_coverage
