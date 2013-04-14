
(* will render (maybe) the filecontent of treemap_rectangle.tr_label *)
val draw_treemap_rectangle_content_maybe :
  Dom_html.canvasRenderingContext2D Js.t ->
  Model_codemap.world_client ->
  (* clipping:Figures.rectangle -> *)
  (* context:Model2.context -> *)
  Model_codemap.fileinfo_client ->
  Treemap.treemap_rectangle -> 
  unit
