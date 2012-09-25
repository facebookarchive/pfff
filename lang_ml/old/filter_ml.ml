(* Adjust the set of files: exclude noisy modules (e.g. tests in external/)
 * or modules that would introduce false positive when we do the
 * modulename->file lookup (e.g. applications of external packages)
 *)
let filter_ml_files files =
  files +> Common.exclude (fun file ->
    let (d,b,e) = Common.dbe_of_filename file in
    let xs = Common.split "/" d in
    let ml_file = Common.filename_of_dbe (d,b,"ml") in

    (* pfff specific ... *)
    let is_test_in_external =
      List.mem "external" xs &&
        xs +> List.exists (fun s ->
          match s with
          | "examples" | "tests" |  "test" 
          (* ocamlgraph and ocamlgtk specific *)
          | "dgraph" | "editor" | "view_graph" | "applications"
              -> true
          | _ -> false
        )
    in
    let _is_test =
      xs +> List.exists (fun s ->
        match s with
        | "examples" | "tests" |  "test" -> true
        | _ -> false
      )
    in
    (* pad specific *)
    let is_old = 
      List.mem "old" xs in
    let is_todo = 
      List.mem "todo" xs in
    let is_unparsable_on_purpose =
      List.mem "parsing_errors" xs in

    let is_build_variant = 
      (* mmm *)
      List.mem "safe" xs 
      (* ocaml: arch *)
    in

    let is_generated = 
      List.mem "_build" xs
    in
    let is_garbage =
      b = "myocamlbuild"
    in
    (* some files like in pfff/external/core/ do not have a .ml
     * so at least index the mli. otherwise skip the mli
     *)
    let _is_mli_with_a_ml =
      e = "mli" && Sys.file_exists ml_file
    in

    is_test_in_external || (*is_test || *)
    is_unparsable_on_purpose ||
    (* is_mli_with_a_ml ||  *)
    is_old || is_todo ||
    is_generated || is_garbage ||
    is_build_variant ||
    false
  )
