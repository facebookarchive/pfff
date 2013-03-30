open Common

module Flag = Flag_web

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let filter = ref Treemap_pl.ex_filter_file

(*****************************************************************************)
(* Model helpers *)
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
