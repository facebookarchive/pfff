open OUnit

module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let unittest = 
"foundation_java" >::: [

(*****************************************************************************)
(* Tags *)
(*****************************************************************************)
(*
 "tags_java" >::: [

   "basic tags" >:: (fun () ->
     let file_content = "
       package foo;
       class Bar { }
     " in
     let tmpfile = Parse_java.tmp_file_from_string file_content in
     let tags = Tags_java.defs_of_dir_or_file ~verbose:false tmpfile [] in
        (match tags with
        | [file, tags_in_file] ->
            assert_equal tmpfile file;
            let xs = tags_in_file +> List.map (fun x ->
              x.Tags_file.tagname, x.Tags_file.kind
            ) in
            assert_equal ~msg:"it should contain the right 6 entries" [
              "foo.Bar", E.Class E.RegularClass;
            ]
            xs
        | _ ->
            assert_failure "The tags should contain only one entry for one file"
        )
   );
 ]
*)

(*---------------------------------------------------------------------------*)
(* Final suite *)
(*---------------------------------------------------------------------------*)
]
