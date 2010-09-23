(**************************************************************************)
(*     Lablgtk - Applications                                             *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*      Benjamin Monate  <Benjamin.Monate@free.fr>                        *)
(*      Olivier Andrieu  <oandrieu@nerim.net>                             *)
(*      Jun Furuse       <Jun.Furuse@inria.fr>                            *)
(*      Hubert Fauque    <hubert.fauque@wanadoo.fr>                       *)
(*      Koji Kagawa      <kagawa@eng.kagawa-u.ac.jp>                      *)
(*                                                                        *)
(**************************************************************************)

(* $Id: shell.ml 1352 2007-07-12 08:56:18Z zoggy $ *)

open StdLabels
module Unix = UnixLabels
open GdkKeysyms
open Printf

(* Nice history class. May reuse *)

class ['a] history () = object
  val mutable history = ([] : 'a list)
  val mutable count = 0
  method empty = history = []
  method add s = count <- 0; history <- s :: history
  method previous =
    let s = List.nth history count in
    count <- (count + 1) mod List.length history;
    s
  method next =
    let l = List.length history in
    count <- (l + count - 1) mod l;
    List.nth history ((l + count - 1) mod l)
end

(* The shell class. Now encapsulated *)

let protect f x = try f x with _ -> ()

class shell ~prog ~args ~env ?packing ?show () =
  let (in2,out1) = Unix.pipe ()
  and (in1,out2) = Unix.pipe ()
  and (err1,err2) = Unix.pipe () in
  let _ = List.iter ~f:Unix.set_nonblock [out1;in1;err1] in
  let view = GText.view ?packing ?show () in
  let buffer = view#buffer in
object (self)
  inherit GObj.widget view#as_widget
  val pid = Unix.create_process_env
      ~prog ~args ~env ~stdin:in2 ~stdout:out2 ~stderr:err2
  val out = Unix.out_channel_of_descr out1
  val h = new history ()
  val mutable alive = true
  val mutable reading = false
  val input_start =
    `MARK (buffer#create_mark ~left_gravity:true buffer#start_iter)
  method private position = buffer#get_iter `INSERT
  method private input_start = buffer#get_iter (input_start :> GText.position)
  method private set_input_start () =
    buffer#move_mark input_start self#position
  method textview = view
  method alive = alive
  method kill () =
    view#set_editable false;
    if alive then begin
      alive <- false;
      protect close_out out;
      List.iter ~f:(protect Unix.close) [in1; err1; in2; out2; err2];
      try
	Unix.kill ~pid ~signal:Sys.sigkill;
	Unix.waitpid pid ~mode:[]; ()
      with _ -> ()
    end
  method interrupt () =
    if alive then try
      reading <- false;
      Unix.kill ~pid ~signal:Sys.sigint
    with Unix.Unix_error _ -> ()
  method send s =
    if alive then try
      output_string out s;
      flush out
    with Sys_error _ -> ()
  method private read ~fd ~len =
    try
      let buf = String.create len in
      let len = Unix.read fd ~buf ~pos:0 ~len in
      if len > 0 then begin
	buffer#place_cursor buffer#end_iter;
	self#insert (String.sub buf ~pos:0 ~len);
	self#set_input_start ();
      end;
      len
    with Unix.Unix_error _ -> 0
  method history (dir : [`next|`previous]) =
    if not h#empty then begin
      if reading then begin
	buffer#delete ~start:(self#input_start) ~stop:(self#position);
      end else begin
	reading <- true;
	self#set_input_start ();
      end;
      self#insert (if dir = `previous then h#previous else h#next);
    end
  method private lex ~start ~stop =
    if start#compare stop < 0 then Lexical.tag buffer ~start ~stop
  method insert text =
    buffer#insert text
  method private keypress c =
    if not reading & c > " " then begin
      reading <- true;
      self#set_input_start ();
    end
  method private return () =
    if reading then reading <- false else begin
      let rec search (it : GText.iter) =
        match it#backward_search "# " with None -> it
        | Some (it1, it2) ->
            if it1#starts_line then it2
            else search it1
      in
      buffer#move_mark input_start (search self#position)
    end;
    let stop = self#position#forward_to_line_end in
    buffer#place_cursor stop;
    let s = buffer#get_text ~start:(self#input_start) ~stop () in
    buffer#place_cursor buffer#end_iter;
    h#add s;
    self#send s;
    self#send "\n"
  method private paste () =
    if not reading then begin
      reading <- true;
      self#set_input_start ();
    end
  initializer
    Lexical.init_tags buffer;
    view#misc#modify_font_by_name "monospace";
    view#misc#set_size_chars ~width:80 ~height:25 ~lang:"C" ();
    view#event#connect#key_press ~callback:
      begin fun ev ->
	if GdkEvent.Key.keyval ev = _Return && GdkEvent.Key.state ev = []
	then self#return ()
	else self#keypress (GdkEvent.Key.string ev);
        false
      end;
    buffer#connect#after#insert_text ~callback:
      begin fun it s ->
        let start = it#backward_chars (String.length s) in
        self#lex ~start:(start#set_line_index 0) ~stop:it#forward_to_line_end;
        view#scroll_mark_onscreen `INSERT
      end;
    buffer#connect#after#delete_range ~callback:
      begin fun ~start ~stop ->
        let start = start#set_line_index 0
        and stop = start#forward_to_line_end in
        self#lex ~start ~stop
      end;
    view#event#connect#button_press ~callback:
      begin fun ev ->
	if GdkEvent.Button.button ev = 2 then self#paste ();
	false
      end;
    view#connect#destroy ~callback:self#kill;
    GMain.Timeout.add ~ms:100 ~callback:
      begin fun () ->
	if alive then begin
	  List.iter [err1;in1]
	    ~f:(fun fd -> while self#read ~fd ~len:1024 = 1024 do () done);
	  true
	end else false
      end;
    ()
end

(* Specific use of shell, for LablBrowser *)

let shells : (string * shell) list ref = ref []

(* Called before exiting *)
let kill_all () =
  List.iter !shells ~f:(fun (_,sh) -> if sh#alive then sh#kill ());
  shells := []
let _ = at_exit kill_all

let get_all () =
  let all = List.filter !shells ~f:(fun (_,sh) -> sh#alive) in
  shells := all;
  all

let may_exec prog =
  try Unix.access prog ~perm:[Unix.X_OK]; true
  with Unix.Unix_error _ -> false

let f ~prog ~title =
  let progargs =
    List.filter ~f:((<>) "") (Str.split (Str.regexp " ") prog) in
  if progargs = [] then () else
  let prog = List.hd progargs in
  let path = try Sys.getenv "PATH" with Not_found -> "/bin:/usr/bin" in
  let exec_path = Str.split (Str.regexp":") path in
  let prog =
    if not (Filename.is_implicit prog) then
      if may_exec prog then prog else ""
    else
      List.fold_left exec_path ~init:"" ~f:
	begin fun acc dir ->
	  if acc <> "" then acc else
	  let prog = Filename.concat dir prog in
	  if may_exec prog then prog else acc
	end
  in
  if prog = "" then () else
  let reg = Str.regexp "TERM=" in
  let env = Array.map (Unix.environment ()) ~f:
      begin fun s ->
 	if Str.string_match reg s 0 then "TERM=dumb" else s
      end in
  let load_path =
    List.flatten (List.map !Config.load_path ~f:(fun dir -> ["-I"; dir])) in
  let args = Array.of_list (progargs @ load_path) in
  let current_dir = ref (Unix.getcwd ()) in

  let tl = GWindow.window ~title () in
  let vbox = GPack.vbox ~packing:tl#add () in
  let menus = GMenu.menu_bar ~packing:vbox#pack () in
  let f = new GMenu.factory menus in
  let accel_group = f#accel_group in
  let file_menu = f#add_submenu "File"
  and history_menu = f#add_submenu "History"
  and signal_menu = f#add_submenu "Signal" in

  let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~packing:vbox#add () in
  let sh = new shell ~prog ~env ~args ~packing:sw#add () in

  let f = new GMenu.factory file_menu ~accel_group in
  f#add_item "Use..." ~callback:
    begin fun () ->
      File.dialog ~title:"Use File" ~filename:(!current_dir ^ "/") () ~callback:
	begin fun name ->
	  current_dir := Filename.dirname name;
	  if Filename.check_suffix name ".ml" then
	    let cmd = "#use \"" ^ name ^ "\";;\n" in
	    sh#insert cmd;
	    sh#send cmd
	end
    end;
  f#add_item "Load..." ~callback:
    begin fun () ->
      File.dialog ~title:"Load File" ~filename:(!current_dir ^ "/") () ~callback:
	begin fun name ->
	  current_dir := Filename.dirname name;
	  if Filename.check_suffix name ".cmo" or
	    Filename.check_suffix name ".cma"
	  then
	    let cmd = Printf.sprintf "#load \"%s\";;\n" name in
	    sh#insert cmd;
	    sh#send cmd
	end
    end;
  f#add_item "Import path" ~callback:
    begin fun () ->
      List.iter (List.rev !Config.load_path)
	~f:(fun dir -> sh#send (sprintf "#directory \"%s\";;\n" dir))
    end;
  f#add_item "Close" ~key:_W ~callback:tl#destroy;

  let h = new GMenu.factory history_menu ~accel_group ~accel_modi:[`MOD1] in
  h#add_item "Previous" ~key:_P ~callback:(fun () -> sh#history `previous);
  h#add_item "Next" ~key:_N ~callback:(fun () -> sh#history `next);
  let s = new GMenu.factory signal_menu ~accel_group in
  s#add_item "Interrupt" ~key:_G ~callback:sh#interrupt;
  s#add_item "Kill" ~callback:sh#kill;
  shells := (title, sh) :: !shells;
  tl#add_accel_group accel_group;
  tl#show ()
