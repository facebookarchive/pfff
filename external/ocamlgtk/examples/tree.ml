(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: tree.ml 1353 2007-07-17 13:27:23Z ben_99_9 $ *)

open StdLabels
open Gobject.Data

let cols = new GTree.column_list
let title = cols#add string
let author = cols#add string
let checked = cols#add boolean

let create_model () =
  let store = GTree.tree_store cols in
  let row = store#append () in
  store#set ~row ~column:title "The Art of Computer Programming";
  store#set ~row ~column:author "Donald E. Knuth";
  store#set ~row ~column:checked false;
  store#set ~row:(store#append ~parent:row ())
    ~column:title "Volume 1: Fundamental Algorithms";
  store#set ~row:(store#append ~parent:row ())
    ~column:title "Volume 2: Seminumerical Algorithms";
  store#set ~row:(store#append ~parent:row ())
    ~column:title "Volume 3: Sorting and Searching Algorithms";
  store

let main () =
  let model = create_model () in
  let window = GWindow.window () in
  window#connect#destroy ~callback:GMain.quit;
  let view = GTree.view ~model ~packing:window#add () in
  let col = GTree.view_column ~title:"Title" ()
      ~renderer:(GTree.cell_renderer_text[], ["text",title]) in
  view#append_column col;
  let col = GTree.view_column ~title:"Author" ()
      ~renderer:(GTree.cell_renderer_text[], ["text",author]) in
  view#append_column col;
  let col = GTree.view_column ~title:"Checked-out" ()
      ~renderer:(GTree.cell_renderer_text[], ["text",checked]) in
  view#append_column col;
  view#selection#connect#after#changed ~callback:
    begin fun () ->
      prerr_endline "selection changed";
      List.iter view#selection#get_selected_rows ~f:
        (fun p -> prerr_endline (GtkTree.TreePath.to_string p));
    end;
  view#connect#after#row_activated ~callback:
    (fun path vcol ->
       prerr_endline "Row activated";
       let it = model#get_iter path in
       assert (model#iter_is_valid it);
       model#clear ();
    );

  (* Seems to be inverted *)
  let allow_expand = ref true in
  view#connect#test_expand_row ~callback:
    (fun _ _ -> 
       if !allow_expand then (Format.printf "Expansion allowed@."; 
                              allow_expand := false; 
                              true)
       else (Format.printf "Expansion NOT allowed@."; 
                              allow_expand := true; 
                              false));

  let allow_collapse = ref true in
  view#connect#test_collapse_row ~callback:
    (fun _ _ -> 
       if !allow_collapse then (Format.printf "Collapse allowed@."; 
                              allow_collapse := false; 
                              true)
       else (Format.printf "Collapse NOT allowed@."; 
                              allow_collapse := true; 
                              false));
  window#show ();
  GMain.main ()

let () = main ()
