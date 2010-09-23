(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: dcalendar.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

(* A small calendar *)
(* Needs Unix module, so use with lablgtk_t *)

open StdLabels
module Unix = UnixLabels
open Printf

type date = { mutable year: int; mutable mon: int; mutable mday: int }

    (* Load the schedule data *)
let calendar_file = Filename.concat (Sys.getenv "HOME") ".camlendar"

let schedule =
  try
    let ichan = open_in_bin calendar_file in
    let (s : (int * int * int, string) Hashtbl.t) =
      Marshal.from_channel ichan in
    close_in ichan;
    s
  with Sys_error msg ->
    prerr_endline msg; flush stderr;
    Hashtbl.create 13;;

    (* Saves the schedule data when the application terminates *)
at_exit (fun () ->
  let ochan = open_out_bin calendar_file in
  Marshal.to_channel ochan schedule [];
  close_out ochan);;

    (* date: Current date initialized to "today" *)
let date =
  let tm = Unix.localtime (Unix.time ()) in
  { year = 1900 + tm.Unix.tm_year; mon = tm.Unix.tm_mon; mday = 1 }


    (* previous_month, next_month: change application status *)
let previous_month () =
  date.mday <- 1;
  if date.mon = 0 then
    (date.year <- date.year - 1; date.mon <- 11)
  else date.mon <- date.mon - 1

let next_month () =
  date.mday <- 1;
  if date.mon = 11 then (date.year <- date.year + 1; date.mon <- 0)
  else date.mon <- date.mon + 1

    (* leap, mon_name, wday_name: Calendar related function and data *)
let leap year =
  (year mod 400 = 0) or
  (year mod 4 = 0) & (year mod 100 <> 0)

let mdays_in_month = [|31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]

let mon_name =
  [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
    "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]

let wday_name =
  [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|]

let s_normal = 0 and s_focused = 1 and s_planned = 2
let styles =
  let default = (Obj.magic () : GObj.style) in
  [| default; default; default |]

   (* class date_button: one button for each day in the month *)
class date_button i (calendar : GPack.table) =
  let mday = i + 1 in

  object (self)
    val widget = GButton.button ~label: (string_of_int mday) ~show: false ()
    val mday = mday
    val mutable show = false
    val mutable have_plan = false

    method widget = widget
    method focus_on =
      date.mday <- mday;
      widget#misc#set_style styles.(s_focused)
    method focus_off =
      widget#misc#set_style styles.(if have_plan then s_planned else s_normal)
    method set_plan =
      have_plan <- true;
      widget#misc#set_style styles.(s_planned)
    method unset_plan =
      have_plan <- false;
      widget#misc#set_style styles.(s_normal)
	
    method show wday0 =
      if not show then
      	let top = (mday + wday0) / 7 + 1
      	and left = (mday + wday0) mod 7 in
      	calendar#attach ~left ~top ~expand:`BOTH widget#coerce;
      	widget#misc#show ();
	show <- true
	    
    method hide =
      if show then
      	(widget#misc#hide ();
	 calendar#remove widget#coerce;
	 show <- false)
  end

let update_calendar (calendar : GPack.table) (buttons : date_button array) =
  let now = Unix.localtime (Unix.gettimeofday ()) in
  let _, first = Unix.mktime { now with 
			       Unix.tm_mday = 1;
			       Unix.tm_mon = date.mon;
			       Unix.tm_year = date.year - 1900 } in

  (* wday0: day of the week of the zero'th day in the month *)
  let wday0 = (first.Unix.tm_wday - 1 + 7) mod 7 in
  
  let ndays =
    if date.mon = 1 & leap date.year then mdays_in_month.(date.mon) + 1
    else mdays_in_month.(date.mon) in

  Array.iter ~f: (fun button -> button#hide)
    buttons;

  for i = 0 to ndays - 1 do buttons.(i)#show wday0 done

let create_GUI () =
  
  (* views part *)

  let win =
    GWindow.window ~title: "Camlendar" ~show: true
      ~allow_shrink: false ~allow_grow: false () in
  win#event#connect#delete
    ~callback: (fun _ -> GMain.Main.quit (); false);

  let style = win#misc#style#copy in
  styles.(s_normal) <- style;
  
  let style = style#copy in
  style#set_bg [`NORMAL, `NAME "light green";
		`PRELIGHT, `NAME "light green"];
  styles.(s_focused) <- style;

  styles.(s_focused) <- style;
  let style = style#copy in
  style#set_bg [`NORMAL, `NAME "sky blue";
		`PRELIGHT, `NAME "sky blue"];
  styles.(s_planned) <- style;

  let vbox = GPack.vbox ~packing: win#add () in
  let packing = vbox#add in
  let toolbar = GButton.toolbar ~style: `TEXT ~packing () in

  let prev =
    toolbar#insert_button ~text: "Prev" ~tooltip: "Show previous month" () in
  let next =
    toolbar#insert_button ~text: "Next" ~tooltip: "Show next month" () in
  
  let calendar =
    GPack.table ~homogeneous: true ~rows: 7 ~columns: 7
      ~border_width: 10 ~row_spacings: 2 ~col_spacings: 2 ~packing () in

  Array.iteri
    ~f: (fun i wday ->
      ignore (GButton.button ~label: wday
	      	~packing:(calendar#attach ~top: 0 ~left: i ~expand:`BOTH) ()))
    wday_name;

  let buttons =
    Array.init 31 ~f: (fun i -> new date_button i calendar) in

  let date_view = GMisc.label ~justify: `CENTER ~packing () in

  let text = GText.view ~editable:true ~width:70 ~height:50 ~packing () in

  (* Controls part *)

  let save_text () =
    let data = text#buffer#get_text () in
    let key = (date.year, date.mon, date.mday) in
    Hashtbl.remove schedule key;
    if data <> "" then
      (Hashtbl.add schedule key data;
       buttons.(date.mday - 1)#set_plan)
    else buttons.(date.mday - 1)#unset_plan in

  let restore_text () =
    try
      text#buffer#set_text
 	(Hashtbl.find schedule (date.year, date.mon, date.mday));
      ()
    with Not_found -> 
      let start,stop = text#buffer#bounds in
      text#buffer#delete ~start ~stop
  in

  let update_date_view () =
    date_view#set_text (sprintf "%d %s, %d\n"
			  date.mday mon_name.(date.mon) date.year) in
  
  let update_view () =
    update_calendar calendar buttons;
    update_date_view ();
    Array.iteri ~f: (fun i button ->
      (try
 	Hashtbl.find schedule (date.year, date.mon, i + 1);
	button#set_plan
      with Not_found -> button#unset_plan);
      button#focus_off) buttons;
    win#set_title (sprintf "Camlendar: %s, %d"
		     mon_name.(date.mon) date.year) in

  prev#connect#clicked
    ~callback: (fun () ->
      save_text ();
      previous_month ();
      
      update_view ();
      restore_text ();
      buttons.(0)#focus_on);
  
  next#connect#clicked
    ~callback: (fun () ->
      save_text ();
      next_month ();
      
      update_view ();
      restore_text ();
      buttons.(0)#focus_on);
  
  Array.iteri
    ~f: (fun i button ->
      button#widget#connect#clicked
      	~callback: (fun () ->
	  save_text ();
	  buttons.(date.mday - 1)#focus_off;

	  button#focus_on;
	  restore_text ();
	  update_date_view ());
      ())
    buttons;

  update_view ();
  buttons.(0)#focus_on;;

GMain.Main.init ();
print_endline (Glib.Main.setlocale `ALL None);
flush stdout;
create_GUI ();
GMain.Main.main ()
