(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: gWindow.ml 1515 2010-06-08 08:50:23Z garrigue $ *)

open Gaux
open Gtk
open GtkBase
open GtkWindow
open GtkMisc
open GObj
open OgtkBaseProps
open GContainer

let set = Gobject.Property.set
let get = Gobject.Property.get

(** Window **)

module P = Window.P

class window_skel obj = object (self)
  inherit ['b] bin_impl obj
  inherit window_props
  method event = new GObj.event_ops obj
  method as_window = (obj :> Gtk.window obj)
  method activate_focus () = Window.activate_focus obj
  method activate_default () = Window.activate_default obj
  method add_accel_group = Window.add_accel_group obj
  method set_default_size ~width ~height =
    set obj P.default_width width;
    set obj P.default_height height
  method move = Window.move obj
  method parse_geometry = Window.parse_geometry obj
  method resize = Window.resize obj
  method set_geometry_hints ?min_size ?max_size ?base_size ?aspect
      ?resize_inc ?win_gravity ?pos ?user_pos ?user_size w =
    Window.set_geometry_hints obj ?min_size ?max_size ?base_size ?aspect
      ?resize_inc ?win_gravity ?pos ?user_pos ?user_size (as_widget w)
  method set_transient_for w =
    set obj P.transient_for (Some w)
  method set_wm_name name = Window.set_wmclass obj ~name
  method set_wm_class cls = Window.set_wmclass obj ~clas:cls
  method show () = Widget.show obj
  method present () = Window.present obj
  method iconify () = Window.iconify obj
  method deiconify () = Window.deiconify obj
end

class window obj = object
  inherit window_skel (obj : [> Gtk.window] obj)
  method connect = new container_signals_impl obj
  method maximize () = Window.maximize obj
  method unmaximize () = Window.unmaximize obj
  method fullscreen () = Window.fullscreen obj
  method unfullscreen () = Window.unfullscreen obj
  method stick () = Window.stick obj
  method unstick () = Window.unstick obj
end

let make_window ~create =
  Window.make_params ~cont:(fun pl ?wm_name ?wm_class ->
    Container.make_params pl ~cont:(fun pl ?(show=false) () ->
      let (w : #window_skel) = create pl in
      may w#set_wm_name wm_name;
      may w#set_wm_class wm_class;
      if show then w#show ();
      w))

let window ?kind =
  make_window [] ~create:(fun pl -> new window (Window.create ?kind pl))

let cast_window (w : #widget) =
  new window (Window.cast w#as_widget)

let toplevel (w : #widget) =
  try Some (cast_window w#misc#toplevel) with Gobject.Cannot_cast _ -> None


(** Dialog **)

class ['a] dialog_signals (obj : [>Gtk.dialog] obj) ~decode = object (self)
  inherit container_signals_impl obj
  method response ~(callback : 'a -> unit) = 
    self#connect Dialog.S.response
      ~callback:(fun i -> callback (decode i))
  method close = self#connect Dialog.S.close
end

let rec list_rassoc k = function
  | (a, b) :: _ when b = k -> a
  | _ :: l -> list_rassoc k l
  | [] -> raise Not_found

let resp = Dialog.std_response

let rnone = resp `NONE
and rreject = resp `REJECT
and raccept = resp `ACCEPT
and rdelete = resp `DELETE_EVENT
and rok = resp `OK
and rcancel = resp `CANCEL
and rclose = resp `CLOSE
and ryes = resp `YES
and rno = resp `NO
and rapply = resp `APPLY
and rhelp = resp `HELP

class virtual ['a] dialog_base obj = object (self)
  inherit window_skel obj
  inherit dialog_props
  method action_area = new GPack.button_box (Dialog.action_area obj)
  method vbox = new GPack.box (Dialog.vbox obj)
  method private virtual encode : 'a -> int
  method private virtual decode : int -> 'a
  method response v = Dialog.response obj (self#encode v)
  method set_response_sensitive v s =
    Dialog.set_response_sensitive obj (self#encode v) s
  method set_default_response v = 
    Dialog.set_default_response obj (self#encode v)
  method run () = 
    let resp = Dialog.run obj in
    if resp = rnone
    then failwith "dialog destroyed"
    else self#decode resp
end

class ['a] dialog_skel obj = object
  inherit ['a] dialog_base obj
  val mutable tbl = [rdelete, `DELETE_EVENT]
  val mutable id = 0
  method private encode (v : 'a) = list_rassoc v tbl
  method private decode r = 
    try 
      List.assoc r tbl 
    with Not_found -> 
      Format.eprintf 
        "Warning: unknown response id:%d in dialog. \
                  Please report to lablgtk dev team.@." 
        r;
      `DELETE_EVENT
end

class ['a] dialog_ext obj = object (self)
  inherit ['a] dialog_skel obj
  method add_button text (v : 'a) =
    tbl <- (id, v) :: tbl ;
    Dialog.add_button obj text id ;
    id <- succ id
  method add_button_stock s_id v =
    self#add_button (GtkStock.convert_id s_id) v
end

class ['a] dialog obj = object (self)
  inherit ['a] dialog_ext (obj :> Gtk.dialog obj)
  method connect : 'a dialog_signals = new dialog_signals obj (self#decode)
end

let make_dialog pl ?parent ?destroy_with_parent ~create =
  make_window ~create:(fun pl ->
    let d = create pl in
    may (fun p -> d#set_transient_for p#as_window) parent ;
    may d#set_destroy_with_parent destroy_with_parent ;
    d) pl

let dialog ?(no_separator=false) =
  make_dialog [] ~create:(fun pl ->
    let pl = 
      if no_separator 
      then (Gobject.param Dialog.P.has_separator false) :: pl
      else pl in
    new dialog (Dialog.create pl))

type any_response = [GtkEnums.response | `OTHER of int]

class dialog_any obj = object (self)
  inherit [any_response] dialog_base (obj :> Gtk.dialog obj)
  method private encode = function
      `OTHER n -> n
    | #GtkEnums.response as v -> Dialog.std_response v
  method private decode r =      
    try (Dialog.decode_response r : GtkEnums.response :> [>GtkEnums.response])
    with Invalid_argument _ -> `OTHER r
  method connect : any_response dialog_signals =
    new dialog_signals obj self#decode
  method add_button text v =
    Dialog.add_button obj text (self#encode v)
  method add_button_stock s_id v =
    self#add_button (GtkStock.convert_id s_id) v
end

(** MessageDialog **)

type 'a buttons = Gtk.Tags.buttons * (int * 'a) list
module Buttons = struct
  let ok = `OK, [ rok, `OK ]
  let close = `CLOSE, [ rclose, `CLOSE ]
  let yes_no = `YES_NO, [ ryes, `YES ; rno, `NO ]
  let ok_cancel = `OK_CANCEL, [ rok, `OK; rcancel, `CANCEL ]
  type color_selection = [`OK | `CANCEL | `HELP | `DELETE_EVENT]
  type file_selection = [`OK | `CANCEL | `HELP | `DELETE_EVENT]
  type font_selection = [`OK | `CANCEL | `APPLY | `DELETE_EVENT]
  type about = [`CANCEL | `CLOSE | `DELETE_EVENT]
end

class ['a] message_dialog obj ~(buttons : 'a buttons) = object (self)
  inherit ['a] dialog_skel obj
  inherit message_dialog_props
  method connect : 'a dialog_signals = new dialog_signals obj self#decode
  method set_markup = MessageDialog.set_markup obj
  initializer
    tbl <- snd buttons @ tbl
end

let message_dialog ?(message="") ?(use_markup=false) ~message_type ~buttons =
  make_dialog [] ~create:(fun pl ->
    let w = 
      let message = if use_markup then "" else message in
      MessageDialog.create ~message_type ~buttons:(fst buttons) ~message () in 
    Gobject.set_params w pl;
    if use_markup then MessageDialog.set_markup w message ;
    new message_dialog ~buttons w)


(** AboutDialog *)

let namep =
  if GtkMain.Main.version >= (2,12,0)
  then GtkBaseProps.AboutDialog.P.program_name
  else GtkBaseProps.Widget.P.name

class about_dialog obj =
  object (self)
    inherit [Buttons.about] dialog_skel obj
    inherit about_dialog_props as props
    method name = Gobject.get namep obj
    method set_name = Gobject.set namep obj
    method connect : Buttons.about dialog_signals =
      new dialog_signals obj self#decode
    method set_artists = AboutDialog.set_artists obj
    method artists = AboutDialog.get_artists obj
    method set_authors = AboutDialog.set_authors obj
    method authors = AboutDialog.get_authors obj
    method set_documenters = AboutDialog.set_documenters obj
    method documenters = AboutDialog.get_documenters obj
    initializer
      tbl <- [ rcancel, `CANCEL ; rclose, `CLOSE ] @ tbl
  end

let about_dialog ?name ?authors =
  let pl = Gobject.Property.may_cons namep name [] in
  AboutDialog.make_params pl ~cont:(fun pl ->
    make_dialog pl ~create:(fun pl ->
      let d = AboutDialog.create () in
      Gobject.set_params d pl ;
      may (AboutDialog.set_authors d) authors ;
      new about_dialog d))

(** ColorSelectionDialog **)

class color_selection_dialog obj = object (self)
  inherit [Buttons.color_selection] dialog_skel (obj : Gtk.color_selection_dialog obj)
  method connect : 'a dialog_signals = new dialog_signals obj self#decode
  method ok_button =
    new GButton.button (ColorSelectionDialog.ok_button obj)
  method cancel_button =
    new GButton.button (ColorSelectionDialog.cancel_button obj)
  method help_button =
    new GButton.button (ColorSelectionDialog.help_button obj)
  method colorsel =
    new GMisc.color_selection (ColorSelectionDialog.colorsel obj)
  initializer
    tbl <- [ rok, `OK ; rcancel, `CANCEL ; rhelp, `HELP ] @ tbl
end

let color_selection_dialog ?(title="Pick a color") =
  make_dialog [] ~title ~resizable:false ~create:(fun pl ->
    new color_selection_dialog (ColorSelectionDialog.create pl))


(** FileSelection **)
class file_selection obj = object (self)
  inherit [Buttons.file_selection] dialog_skel (obj : Gtk.file_selection obj)
  inherit file_selection_props
  method connect : 'a dialog_signals = new dialog_signals obj self#decode
  method complete = FileSelection.complete obj
  method get_selections = FileSelection.get_selections obj
  method ok_button = new GButton.button (FileSelection.get_ok_button obj)
  method cancel_button =
    new GButton.button (FileSelection.get_cancel_button obj)
  method help_button = new GButton.button (FileSelection.get_help_button obj)
  method file_list : string GList.clist =
    new GList.clist (FileSelection.get_file_list obj)
  method dir_list : string GList.clist =
    new GList.clist (FileSelection.get_dir_list obj)
  initializer
    tbl <- [ rok, `OK ; rcancel, `CANCEL ; rhelp, `HELP ] @ tbl
end

let file_selection ?(title="Choose a file") ?(show_fileops=false) =
  FileSelection.make_params [] ~show_fileops ~cont:(
  make_dialog ?title:None ~create:(fun pl ->
    let w = FileSelection.create title in
    Gobject.set_params w pl;
    new file_selection w))


(** FontSelectionDialog **)

class font_selection_dialog obj = object (self)
  inherit [Buttons.font_selection] dialog_skel (obj : Gtk.font_selection_dialog obj)
  method connect : 'a dialog_signals = new dialog_signals obj self#decode
  method selection =
    new GMisc.font_selection (FontSelectionDialog.font_selection obj)
  method ok_button =  new GButton.button (FontSelectionDialog.ok_button obj)
  method apply_button =
    new GButton.button (FontSelectionDialog.apply_button obj)
  method cancel_button =
    new GButton.button (FontSelectionDialog.cancel_button obj)
  initializer
    tbl <- [ rok, `OK ; rcancel, `CANCEL ; rapply, `APPLY ] @ tbl
end

let font_selection_dialog ?title =
  make_dialog [] ?title ~create:(fun pl ->
    new font_selection_dialog (FontSelectionDialog.create pl))


(** Plug **)

class plug_signals obj = object
  inherit container_signals_impl (obj : [> plug] obj)
  inherit plug_sigs
end

class plug (obj : Gtk.plug obj) = object
  inherit window_skel obj
  method connect = new plug_signals obj
end

let plug ~window:xid =
  Container.make_params [] ~cont:(fun pl ?(show=false) () ->
    let w = Plug.create xid in
    Gobject.set_params w pl;
    if show then Widget.show w;
    new plug w)

(** Socket **)

class socket_signals obj = object
  inherit container_signals_impl (obj : [> socket] obj)
  inherit socket_sigs
end

class socket obj = object (self)
  inherit container (obj : Gtk.socket obj)
  method connect = new socket_signals obj
  method steal = Socket.steal obj
  method xwindow =
    self#misc#realize ();
    Gdk.Window.get_xwindow self#misc#window
end

let socket =
  pack_container [] ~create:(fun pl -> new socket (Socket.create pl))

(** FileChooser *)
class ['a] file_chooser_dialog_signals obj ~decode = object
  inherit ['a] dialog_signals obj ~decode
  inherit OgtkFileProps.file_chooser_sigs
end

class ['a] file_chooser_dialog obj = object (self)
  inherit ['a] dialog_ext obj
  inherit GFile.chooser_impl
  method connect : 'a file_chooser_dialog_signals = 
    new file_chooser_dialog_signals obj self#decode
  method add_select_button text v =
    tbl <- (raccept, v) :: tbl ;
    Dialog.add_button obj text raccept
  method add_select_button_stock s_id v =
    self#add_select_button (GtkStock.convert_id s_id) v
end

let file_chooser_dialog ~action ?backend =
  make_dialog 
    (Gobject.Property.may_cons 
       GtkFile.FileChooser.P.file_system_backend backend
       [ Gobject.param GtkFile.FileChooser.P.action action ])
    ~create:(fun pl ->
      let w = GtkFile.FileChooser.dialog_create pl in
      new file_chooser_dialog w)
