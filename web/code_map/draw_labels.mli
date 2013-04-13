
(* TODO: merge ctx and ctx2 *)
val draw_treemap_rectangle_label_maybe :
  ctx:Model_codemap.context -> 
  ctx2:Model_codemap.world_client -> 
  ?color:Simple_color.emacs_color option -> 
  Treemap.treemap_rectangle -> 
  unit
