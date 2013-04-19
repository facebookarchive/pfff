
type world_client = {
  project: string;
  path: string list; 
  size: string;

  m: Dependencies_matrix_code.dm;

  width:  int;
  height: int;

  mutable interactive_regions: (region * Figures.rectangle) list;
}

and region =
    | Cell of int * int (* i, j *)
    | Row of int (* i *)
    | Column of int (* j *)

val xy_ratio: float

(* this assumes a xy_ratio of 1.71 *)
type layout = {
  x_start_matrix_left: float;
  x_end_matrix_right: float;
  y_start_matrix_up: float;
  y_end_matrix_down: float;

  width_vertical_label: float;

  nb_elts: int;
  width_cell: float;
  height_cell: float;
}

val layout_of_w: world_client -> layout

val find_region_at_user_point: 
  world_client -> x:float -> y:float -> 
  region option
