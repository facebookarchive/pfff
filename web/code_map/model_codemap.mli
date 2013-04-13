type world_client = {
  rects: Treemap.treemap_rendering;

  (* viewport, device coordinates *)
  width:  int;
  height: int;

  orig_coord_width: float;
  orig_coord_height: float;
  width_text_etalon_normalized_coord: float;
}

type context = Dom_html.canvasRenderingContext2D Js.t

type fileinfo_client = {
  lines: string list;
}
