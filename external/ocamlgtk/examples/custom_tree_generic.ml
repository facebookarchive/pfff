(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* ../src/lablgtk2 -localdir custom_tree_generic.ml *)


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

module MAKE(TREE:sig type t 
		     val sons: t -> t array 
                     val custom_value: Gobject.g_type -> t -> column:int -> Gobject.basic
                     val column_list:GTree.column_list
	    end) = 
struct
  type custom_tree = 
      {finfo: TREE.t; 
       mutable sons: custom_tree array;
       mutable parent: custom_tree option;
       fidx: int (* invariant: parent.(fidx)==myself *) }
        
  let inbound i a = i>=0 && i<Array.length a
    
  (** The custom model itself *)
  class custom_tree_class column_list =
  object (self)
    inherit 
      [custom_tree,custom_tree,unit,unit] GTree.custom_tree_model column_list

    method custom_encode_iter cr = cr, (), ()
    method custom_decode_iter cr () () = cr

    val mutable num_roots : int = 0
    val mutable roots :  custom_tree array = [||]

    method custom_get_iter (path:Gtk.tree_path) : custom_tree option =
      let indices: int array  = GTree.Path.get_indices path in
      match indices with
      | [||] ->      
          None
      | _ -> 
          if inbound indices.(0) roots then
            let result = ref (roots.(indices.(0))) in
            try
              for depth=1 to Array.length indices - 1 do 
                let index = indices.(depth) in
                if inbound index !result.sons then       
                  result:=!result.sons.(index)
                else raise Not_found
              done;
              Some !result
            with Not_found -> 
              None
          else None

    method custom_get_path (row:custom_tree) : Gtk.tree_path =
      let current_row = ref row in
      let path = ref [] in
      while !current_row.parent <> None do
        path := !current_row.fidx::!path;
        current_row := match !current_row.parent with Some p -> p 
        | None -> assert false
      done;
      GTree.Path.create ((!current_row.fidx)::!path)

    method custom_value (t:Gobject.g_type) (row:custom_tree) ~column =
      TREE.custom_value t row.finfo ~column

    method custom_iter_next (row:custom_tree) : custom_tree option =
      let nidx = succ row.fidx in
      match row.parent with
      | None -> if inbound nidx roots then Some roots.(nidx)
        else None
      | Some parent ->
          if inbound nidx parent.sons then
            Some parent.sons.(nidx)
          else None

    method custom_iter_children (rowopt:custom_tree option) :custom_tree option =
      match rowopt with
      | None -> if inbound 0 roots then Some roots.(0) else None
      | Some row -> if inbound 0 row.sons then Some row.sons.(0) else None

    method custom_iter_has_child (row:custom_tree) : bool =
      Array.length row.sons  > 0 

    method custom_iter_n_children (rowopt:custom_tree option) : int =
      match rowopt with
      | None -> Array.length roots
      | Some row -> Array.length row.sons

    method custom_iter_nth_child (rowopt:custom_tree option) (n:int) 
      : custom_tree option =
      match rowopt with
      | None when inbound n roots -> Some roots.(n)
      | Some row when inbound n row.sons -> Some (row.sons.(n))
      | _ -> None 

    method custom_iter_parent (row:custom_tree) : custom_tree option =
      row.parent

    method append_tree (t:TREE.t) =
      let rec make_forest root sons = 
        Array.mapi 
          (fun i t -> let result = {finfo=t; fidx=i; parent = Some root; 
                                    sons = [||] }
           in 
           let sons = make_forest result (TREE.sons t) in
           result.sons<-sons;
           result)
          sons
      in
      let pos = num_roots in
      num_roots <- num_roots+1;
      let root = { finfo = t; sons = [||];
                   parent = None;
                   fidx = pos } 
      in
      
      let sons = make_forest root (TREE.sons t)
      in
      root.sons <- sons;
      roots <-
        Array.init num_roots (fun n -> if n = num_roots - 1 then root 
                              else roots.(n))

  end

  let custom_tree () = 
    new custom_tree_class TREE.column_list
end


module T=struct
  type leaf = {mutable checked: bool; mutable lname: string; }
  type t = Leaf of leaf |  Node of string* t list

  let sons t = match t with
  | Leaf _ -> [||]
  | Node (_,s)-> Array.of_list s

  (** The columns in our custom model *)
  let column_list = new GTree.column_list ;;
  let col_file = (column_list#add Gobject.Data.caml: t GTree.column);;
  let col_bool = column_list#add Gobject.Data.boolean;;
  let col_int = column_list#add Gobject.Data.int;;
  let col_is_leaf = column_list#add Gobject.Data.boolean;;
  

  let custom_value _ t ~column = 
    match column with
    | 0 -> (* col_file *) `CAML (Obj.repr t)
    | 1 -> (* col_bool *) `BOOL false
    | 2 -> (* col_int *) `INT 0
    | 3 -> (* col_is_leaf*) `BOOL (match t with Leaf _ -> true | _ -> false)
    | _ -> assert false

end

module MODEL=MAKE(T)

let nb = ref 0

let make_tree n p = 
  let rec aux p0 = 
    if p=p0 then 
      begin
        incr nb;
        T.Leaf {T.lname = "Leaf "^string_of_int !nb; checked = false}
      end
    else begin
      incr nb;
      let name = "Node "^string_of_int !nb in
      T.Node (name,aux_list n (succ p0))
    end
  and aux_list n p = 
    if n = 0 then []
    else aux p::aux_list (n-1) p
  in
  aux 0
    
let fill_model t =
  for i = 0 to 10000 do
    t#append_tree (make_tree 1 1)
  done



let create_view_and_model () : GTree.view =
  let custom_tree = MODEL.custom_tree () in
  fill_model custom_tree;
  let view = GTree.view ~fixed_height_mode:true ~model:custom_tree () in
  let renderer = GTree.cell_renderer_text [] in
  let col_name = GTree.view_column ~title:"Name" ~renderer:(renderer,[]) () in
  col_name#set_sizing `FIXED;
  col_name#set_fixed_width 150;
  col_name#set_cell_data_func 
    renderer
    (fun model row -> 
       try
	 let data = model#get ~row ~column:T.col_file in
	 match data with 
	 | T.Leaf {T.lname = s} | T.Node (s,_) -> 
	     renderer#set_properties [ `TEXT s ];
       with exn -> 
	 let s = GtkTree.TreePath.to_string (model#get_path row) in
	 Format.printf "Accessing %s, got '%s' @." s (Printexc.to_string exn));
  ignore (view#append_column col_name);
  
  let renderer = GTree.cell_renderer_toggle [] in
  let col_tog = GTree.view_column ~title:"Check" 
    ~renderer:(renderer,["visible", T.col_is_leaf])
    ()
  in
  col_tog#set_sizing `FIXED;
  col_tog#set_fixed_width 10;
  col_tog#set_cell_data_func 
    renderer
    (fun model row -> 
       try
	 let data = model#get ~row ~column:T.col_file in
	 match data with 
	 | T.Leaf {T.checked = b}  -> renderer#set_properties [ `ACTIVE b ]
         | _ -> ()
       with exn -> 
	 let s = GtkTree.TreePath.to_string (model#get_path row) in
	 Format.printf "Accessing %s, got '%s' @." s (Printexc.to_string exn));
  
  ignore(renderer#connect#toggled 
           (fun path -> 
              let row = custom_tree#custom_get_iter path in
              match row with 
              | Some {MODEL.finfo=T.Leaf l} -> 
                  l.T.checked <- not l.T.checked
              | _ -> ()));
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
