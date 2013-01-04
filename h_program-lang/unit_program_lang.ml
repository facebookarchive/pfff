open Common

open OUnit

open Dependencies_matrix_code
module E = Database_code
module G = Graph_code
module DM = Dependencies_matrix_code

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Data *)
(*****************************************************************************)

let build_g_and_dm () =
  let g = G.create () in
  g +> G.add_node (".", E.Dir);
  g +> G.add_node ("foo.ml", E.File);
  g +> G.add_node ("a", E.Dir);
  g +> G.add_node ("a/x.ml", E.File);
  g +> G.add_node ("a/y.ml", E.File);
  g +> G.add_node ("bar.ml", E.File);
  g +> G.add_edge ((".", E.Dir), ("foo.ml", E.File)) G.Has;
  g +> G.add_edge ((".", E.Dir), ("bar.ml", E.File)) G.Has;
  g +> G.add_edge ((".", E.Dir), ("a", E.Dir)) G.Has;
  g +> G.add_edge (("a", E.Dir), ("a/x.ml", E.File)) G.Has;
  g +> G.add_edge (("a", E.Dir), ("a/y.ml", E.File)) G.Has;

  g +> G.add_edge (("a/x.ml", E.File), ("foo.ml", E.File)) G.Use;
  g +> G.add_edge (("a/y.ml", E.File), ("foo.ml", E.File)) G.Use;
  g +> G.add_edge (("bar.ml", E.File), ("foo.ml", E.File)) G.Use;
  g +> G.add_edge (("a/y.ml", E.File), ("a/x.ml", E.File)) G.Use;
  g +> G.add_edge (("bar.ml", E.File), ("a/y.ml", E.File)) G.Use;

  let dm = {
    matrix = [|
      [| 0; 0; 0; 0|];
      [| 1; 0; 0; 0|];
      [| 1; 2; 0; 0|];
      [| 1; 0; 3; 0|];
    |];
    name_to_i = Common.hash_of_list [
      ("foo.ml", E.File), 0;
      ("a/x.ml", E.File), 1;
      ("a/y.ml", E.File), 2;
      ("bar.ml", E.File), 3;
    ];
    i_to_name = [|
      ("foo.ml", E.File);
      ("a/x.ml", E.File);
      ("a/y.ml", E.File);
      ("bar.ml", E.File);
    |];
    config = 
      Node ((".", E.Dir), [
        Node (("foo.ml", E.File), []);
        Node (("a", E.Dir), [
          Node (("a/x.ml", E.File), []);
          Node (("a/y.ml", E.File), []);
        ]);
        Node (("bar.ml", E.File), []);
      ]);
  } in
  g, dm
  
(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "program_lang" >::: [

    "dm" >::: [

      "dead columns" >:: (fun () ->
        let (_, dm) = build_g_and_dm () in
        assert_equal false (DM.is_dead_column 0 dm);
        assert_equal true (DM.is_dead_column 3 dm);
        ()
      );
      "internal helpers" >:: (fun () ->
        let (_, dm) = build_g_and_dm () in
        let arr = DM.parents_of_indexes dm in
        assert_equal arr
          [| [(".", E.Dir)];
             [(".", E.Dir); ("a", E.Dir); ];
             [(".", E.Dir); ("a", E.Dir); ];
             [(".", E.Dir)];
          |];
        assert_equal
          ~msg:"It should not find distance between foo.ml and a/x.ml"
          (DM.distance_entity 0 1 arr) 0;
        assert_equal
          ~msg:"It should find distance between a/x.ml and foo.ml"
          (DM.distance_entity 1 0 arr) 1;
        assert_equal
          ~msg:"It should not find distance between a/x.ml a/y.ml"
          (DM.distance_entity 1 2 arr) 0;

        assert_equal 
          false (DM.is_internal_helper 0 dm);
        assert_equal 
          true (DM.is_internal_helper 1 dm);
        assert_equal 
          false (DM.is_internal_helper 2 dm);
      );
      "explain cell" >:: (fun () ->
        let (g, dm) = build_g_and_dm () in
        let gopti = Graph_code_opti.convert g in
        let xs = DM.explain_cell_list_use_edges (2, 1) dm gopti in
        assert_equal xs [
          ("a/y.ml", E.File), ("a/x.ml", E.File);
        ];
      );
    ]
  ]
