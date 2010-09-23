(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)
(* ../src/lablgtk2 -localdir custom_tree.ml *)


let debug = false
let () = 
  if debug then begin 
  Gc.set { (Gc.get()) with Gc.verbose = 0x00d; space_overhead = 0 };
  ignore (Gc.create_alarm (fun () -> 
  let s = Gc.stat () in
  Format.printf "blocks=%d words=%d@."
  s.Gc.live_blocks
  s.Gc.live_words))
  end

type finfo = { fname : string; mutable fchecked:bool }

type file = { finfo: finfo; mutable globals: global array; fidx: int }
and global = { gname: string; parent: file; gidx: int }
type custom_tree =
  | File of file
  | Global of global

let get_nb ct = match ct with
| File{fidx=i}|Global{gidx=i} -> i


(** The columns in our custom model *)
let column_list = new GTree.column_list ;;
let col_file = column_list#add Gobject.Data.caml;;
let col_bool = column_list#add Gobject.Data.boolean;;
let col_int = column_list#add Gobject.Data.int;;

(** The custom model itself *)
class custom_tree_class column_list =
object (self)
  inherit 
    [custom_tree,custom_tree,unit,unit] GTree.custom_tree_model column_list

  method custom_encode_iter cr = cr, (), ()
  method custom_decode_iter cr () () = cr

  val mutable num_files : int = 0
  val mutable rows : file array = [||]

  method custom_flags : GtkEnums.tree_model_flags list = [`ITERS_PERSIST]

  method custom_get_iter (path:Gtk.tree_path) : custom_tree option =
    let indices  = GTree.Path.get_indices path in
    match indices with
    | [| file |] ->
	if file >= num_files || file < 0 then None else Some (File rows.(file))
    | [| file; global |] -> 
	if file >= num_files|| file < 0 then None 
	else 
	  let globals = rows.(file).globals in
	  if global >= Array.length globals || global < 0 then None 
	  else Some (Global globals.(global))
    | _ -> None

  method custom_get_path (row:custom_tree) : Gtk.tree_path =
    match row with
    | File file -> GTree.Path.create [ file.fidx ]
    | Global global -> GTree.Path.create [ global.parent.fidx; global.gidx ]

  method custom_value (t:Gobject.g_type) (row:custom_tree) ~column =
    if column = 0 then `CAML (Obj.repr row)
    else if column = 1 then
      `BOOL (match row with File {finfo={fchecked=b}} -> b
             | _ -> false )
    else if column = 2 then
      `INT (5+(get_nb row))
    else assert false

  method custom_iter_next (row:custom_tree) : custom_tree option =
    match row with
    | File file ->
	if file.fidx < Array.length rows - 1 then 
	  Some (File (rows.(succ file.fidx)))
	else 
	  None
    | Global global -> 
	let parent = global.parent in
	if global.gidx < Array.length parent.globals - 1 then 
	  Some (Global (parent.globals.(succ global.gidx)))
	else 
	  None

  method custom_iter_children (rowopt:custom_tree option) : custom_tree option =
    match rowopt with
    | None | Some (File { globals = [||] }) | Some (Global _) -> None
    | Some (File { globals = globals }) -> Some (Global globals.(0))

  method custom_iter_has_child (row:custom_tree) : bool =
    match row with 
    | File { globals = g } when Array.length g > 0 -> true
    | _ -> false

  method custom_iter_n_children (rowopt:custom_tree option) : int =
    match rowopt with
    | None -> Array.length rows
    | Some (Global _) -> 0
    | Some (File { globals = g }) -> Array.length g

  method custom_iter_nth_child (rowopt:custom_tree option) (n:int) : custom_tree option =
    match rowopt with
    | None when Array.length rows > 0 -> Some (File rows.(0))
    | Some (File { globals = g }) when n < Array.length g -> 
	Some (Global g.(n))
    | _ -> 
	None

  method custom_iter_parent (row:custom_tree) : custom_tree option =
    match row with
    | File _ -> None
    | Global g -> Some (File g.parent)

  method append_file name global_names =
    let pos = num_files in
    let f = { finfo = name; globals = [||]; fidx = pos } in
    let globals = 
      Array.mapi 
	(fun i g -> { gname = g; parent = f; gidx = i })
	global_names;
    in
    f.globals <- globals;
    num_files <- num_files + 1;
    rows <-
      Array.init num_files (fun n -> if n = num_files - 1 then f else rows.(n))

end

let fill_model t =
  for i = 0 to 100 do
    let g = Array.init 100  (fun i -> "Son "^string_of_int i) in
    t#append_file {fname = ("Parent "^string_of_int i); fchecked = false} g
  done

let create_view_and_model () : GTree.view =
  let custom_tree = new custom_tree_class column_list in
  fill_model custom_tree;
  let view = GTree.view ~model:custom_tree () in
  let renderer = GTree.cell_renderer_text [] in
  let col_name = GTree.view_column ~title:"Name" ~renderer:(renderer,["height",col_int]) () in
  col_name#set_cell_data_func 
    renderer
    (fun model row -> 
       try
	 let data = model#get ~row ~column:col_file in
	 match data with 
	 | File { finfo={fname = s} } | Global { gname = s } -> 
	   renderer#set_properties [ `TEXT s ];
       with exn -> 
	 let s = GtkTree.TreePath.to_string (model#get_path row) in
	 Format.printf "Accessing %s, got '%s' @." s (Printexc.to_string exn));
  ignore (view#append_column col_name);
  
  let renderer = GTree.cell_renderer_toggle [] in
  let col_tog = GTree.view_column ~title:"Check" 
    ~renderer:(renderer,["active", col_bool])
    ()
  in
  renderer#connect#toggled (fun path -> 
                              let row = custom_tree#custom_get_iter path in
                              match row with 
                              | Some (File {finfo=f}) -> f.fchecked <- not f.fchecked
                              | Some (Global _ ) -> 
                                  Format.printf "Clearing %s@." (GtkTree.TreePath.to_string path);
                                  Format.printf "Global@."
                              | _ -> ());
  ignore (view#append_column col_tog);
  
  view

let _ =
  ignore (GtkMain.Main.init ());
  let window = GWindow.window ~width:200 ~height:400 () in
  ignore 
    (window#event#connect#delete 
       ~callback:(fun _ -> exit 0));
  let scrollwin = GBin.scrolled_window ~packing:window#add () in
  let view = create_view_and_model () in
  scrollwin#add view#coerce;
  window#show ();
  GtkMain.Main.main ()
