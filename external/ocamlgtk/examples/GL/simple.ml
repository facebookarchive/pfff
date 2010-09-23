(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: simple.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GMain

let main () =
  let w = GWindow.window ~title:"LablGL/Gtk" () in
  w#connect#destroy ~callback:Main.quit;
  let area =
    GlGtk.area [`RGBA;`DEPTH_SIZE 1;`DOUBLEBUFFER]
      ~width:500 ~height:500 ~packing:w#add () in
  area#connect#realize ~callback:
    begin fun () ->
      GlMat.mode `projection;
      GlMat.load_identity ();
      GlMat.ortho ~x:(-1.0,1.0) ~y:(-1.0,1.0) ~z:(-1.0,1.0);
    end;
  area#connect#display ~callback:
    begin fun () ->
      GlClear.color (0.0, 0.0, 0.0);
      GlClear.clear [`color];
      GlDraw.color (1.0, 1.0, 1.0);
      GlDraw.begins `polygon;
      GlDraw.vertex ~x:(-0.5) ~y:(-0.5) ();
      GlDraw.vertex ~x:(-0.5) ~y:(0.5) ();
      GlDraw.vertex ~x:(0.5) ~y:(0.5) ();
      GlDraw.vertex ~x:(0.5) ~y:(-0.5) ();
      GlDraw.ends ();
      Gl.flush ();
      area#swap_buffers ()
    end;
  Timeout.add ~ms:10000 ~callback:(fun () -> w#destroy ();false);
  w#show ();
  Main.main ()

let _ = main ()
