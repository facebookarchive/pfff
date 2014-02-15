(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* ../src/lablgtk2 -localdir custom_list_generic.ml *)

(*
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
*)


module MAKE(A:sig type t 
        val custom_value: Gobject.g_type -> t -> column:int -> Gobject.basic
        val column_list:GTree.column_list
end) = 
struct
  type custom_list =
      {finfo: A.t; 
       fidx: int (* invariant: root.(fidx)==myself *) }
        
  module H = Hashtbl

  let inbound i a = i>=0 && i<Array.length a
    
  (** The custom model itself *)
  class custom_list_class column_list =
  object (self)
    inherit 
      [custom_list,custom_list,unit,unit] GTree.custom_tree_model column_list

    method custom_encode_iter cr = cr, (), ()
    method custom_decode_iter cr () () = cr

    val mutable last_idx = 0
    val mutable roots : (int,custom_list) H.t = H.create 19
    method private find_opt i = 
      try Some (H.find roots i) with Not_found -> None
    method! custom_flags = [`LIST_ONLY]
    method custom_get_iter (path:Gtk.tree_path) : custom_list option =
      let indices: int array  = GTree.Path.get_indices path in
      match indices with
      | [||] ->      
          None
      | [|i|] -> self#find_opt i
      | _ -> failwith "Invalid Path of depth > 1 in a list"

    method custom_get_path (row:custom_list) : Gtk.tree_path =
      GTree.Path.create [row.fidx]

    method custom_value (t:Gobject.g_type) (row:custom_list) ~column =
      A.custom_value t row.finfo ~column

    method custom_iter_next (row:custom_list) : custom_list option =
      let nidx = succ row.fidx in
	self#find_opt nidx
	  
    method custom_iter_children (rowopt:custom_list option) :custom_list option =
      match rowopt with
      | None -> self#find_opt 0
      | Some _ -> None

    method custom_iter_has_child (_row:custom_list) : bool = false

    method custom_iter_n_children (rowopt:custom_list option) : int =
      match rowopt with
      | None -> H.length roots
      | Some _ -> assert false

    method custom_iter_nth_child (rowopt:custom_list option) (n:int) 
      : custom_list option =
      match rowopt with
      | None -> self#find_opt n
      | _ -> None 

    method custom_iter_parent (_row:custom_list) : custom_list option = None

    method insert (t:A.t) =
      let e = {finfo=t; fidx= last_idx } in
      self#custom_row_inserted (GTree.Path.create [last_idx]) e;
      H.add roots last_idx e;
      last_idx <- last_idx+1;

  end

  let custom_list () = 
    new custom_list_class A.column_list
end

(*
module L=struct
  type t = {mutable checked: bool; mutable lname: string; }

  (** The columns in our custom model *)
  let column_list = new GTree.column_list ;;
  let col_full = (column_list#add Gobject.Data.caml: t GTree.column);;
  let col_bool = column_list#add Gobject.Data.boolean;;
  let col_int = column_list#add Gobject.Data.int;;
 

  let custom_value _ t ~column = 
    match column with
    | 0 -> (* col_full *) `CAML (Obj.repr t)
    | 1 -> (* col_bool *) `BOOL false
    | 2 -> (* col_int *) `INT 0
    | _ -> assert false

end

module MODEL=MAKE(L)

let fill_model t =
  for i= 0 to 100000 do
    t#insert {L.lname = "Elt "^string_of_int i; checked=i mod 2 = 0}
  done

let create_view_and_model () : GTree.view =
  let custom_list = MODEL.custom_list () in
  fill_model custom_list;
  let view = GTree.view ~model:custom_list () in
  let renderer = GTree.cell_renderer_text [] in
  let col_name = GTree.view_column ~title:"Name" ~renderer:(renderer,[]) () in
  col_name#set_cell_data_func 
    renderer
    (fun model row -> 
       try
	 let data = model#get ~row ~column:L.col_full in
	 match data with 
	 | {L.lname = s} -> 
	     renderer#set_properties [ `TEXT s ];
       with exn -> 
	 let s = GtkTree.TreePath.to_string (model#get_path row) in
	 Format.printf "Accessing %s, got '%s' @." s (Printexc.to_string exn));
  ignore (view#append_column col_name);
  
  let renderer = GTree.cell_renderer_toggle [] in
  let col_tog = GTree.view_column ~title:"Check me" 
    ~renderer:(renderer,[])
    ()
  in
  col_tog#set_cell_data_func 
    renderer
    (fun model row -> 
       try
	 let {L.checked = b} = model#get ~row ~column:L.col_full in
         renderer#set_properties [ `ACTIVE b ]
       with exn -> 
	 let s = GtkTree.TreePath.to_string (model#get_path row) in
	 Format.printf "Accessing %s, got '%s' @." s (Printexc.to_string exn));
  
  ignore(renderer#connect#toggled 
           (fun path -> 
              let row = custom_list#custom_get_iter path in
              match row with 
              | Some {MODEL.finfo=l} -> 
                  l.L.checked <- not l.L.checked
              | _ -> ()));
  ignore (view#append_column col_tog);
  Glib.Timeout.add ~ms:10000 ~callback:(fun () -> 
					 fill_model custom_list; false);
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

*)
