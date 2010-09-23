(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: slide_show.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

(*
let get_pixbuf ~file =
  try GdkPixbuf.from_file file
  with GdkPixbuf.GdkPixbufError(_,msg) as exn ->
    let d = GWindow.message_dialog ~message:msg ~message_type:`ERROR
        ~buttons:GWindow.Buttons.close ~show:true () in
    d#run ();
    raise exn
*)

class directory ~path = object (self)
  val d = Unix.opendir path
  method read = path ^"/"^ Unix.readdir d
  method rewind = Unix.rewinddir d
  method close = Unix.closedir d
  method read_file =
    let f = self#read in
    if (Unix.stat f).Unix.st_kind = Unix.S_REG then f
    else self#read_file
  method next_file =
    try self#read_file with End_of_file -> self#rewind; self#read_file
  method read_pix =
    let f = self#read_file in
    try GdkPixbuf.from_file f
    with GdkPixbuf.GdkPixbufError _ -> self#read_pix
end

let () =
  let w = GWindow.window () in
  let da = GMisc.drawing_area ~packing:w#add () in
  da#misc#realize ();
  let dw = new GDraw.drawable da#misc#window in
  let dir = new directory "." in
  let pm = ref None in
  let set_pm pxm =
    Gaux.may (fun pm -> Gdk.Pixmap.destroy pm) !pm;
    pm := Some pxm;
    dw#put_pixmap ~x:0 ~y:0 pxm
  in
  let set_pix pix =
    let pxm, _ = GdkPixbuf.create_pixmap pix
    and width = GdkPixbuf.get_width pix
    and height = GdkPixbuf.get_height pix in
    w#set_default_size ~width ~height;
    set_pm pxm
  in
  let pix = dir#read_pix in set_pix pix;
  da#event#connect#expose ~callback:
    (fun _ -> Gaux.may (dw#put_pixmap ~x:0 ~y:0) !pm; true);
  GMain.Timeout.add ~ms:2000 ~callback:
    (fun () -> try
      let pix =
        try dir#read_pix with End_of_file -> dir#rewind; dir#read_pix in
      set_pix pix;
      true
    with _ -> false);
  w#connect#destroy GMain.quit;
  w#show ();
  GMain.main ()
