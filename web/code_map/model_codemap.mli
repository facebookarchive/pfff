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
  nblines: float; (* more convenient than int *)

  style: file_rendering_style;
}

  and file_rendering_style =
    | Regular of string list (* lines *)
    | Fancy of (lines * Highlight_code.category option * Common2.filepos) list
    | Nothing
  and lines = 
   (string, unit) Common2.either list
