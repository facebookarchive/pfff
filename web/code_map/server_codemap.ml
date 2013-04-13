open Common

module Flag = Flag_web

(*
module FT = File_type
module Parsing = Parsing2
*)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let filter = ref Treemap_pl.ex_filter_file

(*****************************************************************************)
(* Macro view server helpers *)
(*****************************************************************************)

let treemap_generator paths = 
  let treemap = Treemap_pl.code_treemap ~filter_file:!filter paths in
  let algo = Treemap.Ordered Treemap.PivotByMiddle in
  let rects = Treemap.render_treemap_algo 
    ~algo ~big_borders:!Flag.boost_label_size
    treemap 
  in
  pr2 (spf "%d rectangles to draw" (List.length rects));
  rects

(*****************************************************************************)
(* Micro view server helpers *)
(*****************************************************************************)

(*
let is_big_file_with_few_lines ~nblines fullpath = 
  nblines < 20. && 
  Common2.filesize_eff fullpath > 4000
*)
