(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)


let zoom  = ref None
let dpi   = ref None
let fname = ref ""

let _ = 
  let cli_args = [
    ( "-zoom", Arg.Float (fun v -> zoom := Some v), "zoom factor" ) ;
    ( "-dpi" , Arg.Float (fun v -> dpi := Some v),  "") ] in
  let usg_msg = 
    Printf.sprintf "usage: %s [options] <file>\n"
      (Filename.basename Sys.executable_name) in
  Arg.parse cli_args ((:=) fname) usg_msg ;
  if not (Sys.file_exists !fname)
  then begin
    Arg.usage cli_args usg_msg ; 
    exit 2 
  end

let pb =
  let gz =
    Filename.check_suffix !fname ".svgz" ||
    Filename.check_suffix !fname ".svg.gz" in
  let size_cb = match !zoom with
  | None -> None
  | Some z -> Some (Rsvg.at_zoom z z) in
  Rsvg.render_from_file ~gz ?dpi:!dpi ?size_cb !fname

let w = GWindow.window ~allow_grow:false ~title:!fname ()
let i = GMisc.image ~packing:w#add ()

let () = 
  i#set_pixbuf pb ;
  w#connect#destroy GMain.quit;
  w#show ();
  GMain.main ()
