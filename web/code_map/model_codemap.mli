type world_client = {
  project: string;
  (* readable path *)
  path: string;
  size: string;

  rects: Treemap.treemap_rendering;

  root: Common.dirname;

  (* viewport, device coordinates *)
  width:  int;
  height: int;
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

val find_rectangle_at_user_point:
  world_client ->
  Figures.point ->
  (Treemap.treemap_rectangle * 
   Treemap.treemap_rectangle list * 
   Treemap.treemap_rectangle)
  option
