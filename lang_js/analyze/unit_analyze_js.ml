open Common

module Ast = Ast_js
module Db = Database_code

open OUnit

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* Annotations *)
(*---------------------------------------------------------------------------*)
let annotation_unittest =
  "annotation_js" >::: [

    "commonjs annotations" >:: (fun () ->
      let file_content = "
/**
 * @providesModule my-module
 */
function foo() {}
/**
 * @providesLegacy other-module
 */
function bar() {}
" in
      Common2.with_tmp_file ~str:file_content ~ext:"js" (fun tmpfile ->
        let (ast_and_tokens, _stat) = Parse_js.parse tmpfile in
        let annots = 
          Annotation_js.annotations_of_program_with_comments ast_and_tokens
          +> List.map fst
        in
        assert_equal 
          ~msg:"it should extract @providesModule annotations"
          [Annotation_js.ProvidesModule "my-module";
           Annotation_js.ProvidesLegacy "other-module";
          ]
          annots
      )
    );
  ]

(*---------------------------------------------------------------------------*)
(* Tags *)
(*---------------------------------------------------------------------------*)

let tags_unittest =
 "tags_js" >::: [
   "commonjs tags support" >:: (fun () ->
     let file_content = "
/**
 * @providesModule my-module
 */
function foo() {}
" in
     Common2.with_tmp_file ~str:file_content ~ext:"js" (fun tmpfile ->
       let tags = Tags_js.tags_of_files_or_dirs ~verbose:false [tmpfile] in
       let tags = tags +> List.map snd +> List.flatten in
       match tags with
       | [{ Tags_file.tagname = "my-module"; kind = Db.Module; _}] -> ()
       | _ -> assert_failure "it should extract module names from comments"
     ));
 ]

(*---------------------------------------------------------------------------*)
(* Final suite *)
(*---------------------------------------------------------------------------*)

let unittest =
  "analyze_js" >::: [
    annotation_unittest;
    tags_unittest;
  ]
