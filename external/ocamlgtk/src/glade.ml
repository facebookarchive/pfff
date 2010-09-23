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

(* $Id: glade.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels
open Gtk

(* GladeXML widget *)

type glade_xml = [`data|`glade_xml]

external init : unit -> unit = "ml_glade_init"
(* external gnome_init : unit -> unit = "ml_glade_gnome_init" *)
external create :
    ?file:string -> ?data:string ->
    ?root:string -> ?domain:string -> unit -> glade_xml obj
    = "ml_glade_xml_new"

external _signal_autoconnect :
    [> `glade_xml] obj ->
    (string * unit obj * string * unit obj option * bool -> unit) -> unit
    = "ml_glade_xml_signal_autoconnect_full"

let signal_autoconnect self ~f =
  _signal_autoconnect self
    (fun (handler, obj, signal, target, after) ->
      f ~handler ~signal ~after ?target obj)

external _signal_connect :
    [> `glade_xml] obj -> string ->
    (string * unit obj * string * unit obj option * bool -> unit) -> unit
    = "ml_glade_xml_signal_connect_full"

let signal_connect self ~handler ~f =
  _signal_connect self handler
    (fun (handler, obj, signal, target, after) ->
      f ~signal ~after ?target obj)

external get_widget : [> `glade_xml] obj -> name:string -> widget obj
    = "ml_glade_xml_get_widget"
external get_widget_name : [> `widget] obj -> string
    = "ml_glade_get_widget_name"
external get_widget_tree : [> `widget] obj -> glade_xml obj
    = "ml_glade_get_widget_tree"

let get_widget_msg ~name ?info xml =
  try get_widget ~name xml
  with Gpointer.Null ->
    let name = match info with None -> name | Some s -> s^":"^name in
    failwith ("Glade error: " ^ name ^ " is not accessible.")

(* Signal handlers *)
open Gobject

type handler =
  [ `Simple of (unit -> unit)
  | `Object of string * (unit obj -> unit)
  | `Custom of (Closure.argv -> data_get list -> unit) ]

let ($) f g x = g (f x)
let gtk_bool b argv _ = Closure.set_result argv (`BOOL b)
let known_handlers : (string, handler) Hashtbl.t = Hashtbl.create 11
let add_handler ~name handler =
  Hashtbl.add known_handlers name handler
open GtkBase
let _ = List.iter ~f:(fun (name,h) -> add_handler ~name h)
    [ "gtk_widget_destroy",`Object ("GtkObject", Object.cast $ Object.destroy);
      "gtk_main_quit", `Simple GtkMain.Main.quit;
      "gtk_widget_show", `Object ("GtkWidget", Widget.cast $ Widget.show);
      "gtk_widget_hide", `Object ("GtkWidget", Widget.cast $ Widget.hide);
      "gtk_widget_grab_focus",
      `Object ("GtkWidget",
               Widget.cast $ fun w -> set Widget.P.has_focus w true);
      "gtk_window_activate_default",
      `Object ("GtkWindow", fun w -> ignore (GtkWindow.Window.activate_default
                                               (GtkWindow.Window.cast w)));
      "gtk_true", `Custom (gtk_bool true);
      "gtk_false", `Custom (gtk_bool false);
    ]

open Printf

let check_handler ?target ?(name="<unknown>") handler =
  match handler with
    `Simple f ->
      fun _ -> f ()
  | `Object (cls, f) ->
      begin match target with
        None ->
          eprintf "Glade-warning: %s requires an object argument.\n" name;
          raise Not_found
      | Some obj ->
          if Gobject.is_a obj cls then
            fun _ -> f obj
          else begin
            eprintf "Glade-warning: %s expects a %s argument.\n" name cls;
            raise Not_found
          end
      end
  | `Custom f ->
      if target <> None then
        eprintf "Glade-warning: %s does not take an object argument.\n" name;
      fun argv -> f argv (Closure.get_args argv)

let bind_handlers ?(extra=[]) ?(warn=false) xml =
  signal_autoconnect xml ~f:
    begin fun ~handler:name ~signal ~after ?target obj ->
      try
        let handler =
          try List.assoc name extra
          with Not_found -> Hashtbl.find known_handlers name
        in
        let callback = check_handler ?target ~name handler in
        ignore (GtkSignal.connect_by_name obj ~name:signal ~after
		  ~callback:(Closure.create callback))
      with Not_found ->
        if warn then eprintf "Glade.bind_handlers: no handler for %s\n" name
    end;
  flush stderr

let bind_handler ~name ~handler ?(warn=true) xml =
  let warn = ref warn in
  signal_connect xml ~handler:name ~f:
    begin fun ~signal ~after ?target obj ->
      warn := false;
      let callback = check_handler ?target ~name handler in
      ignore (GtkSignal.connect_by_name obj ~name:signal ~after
		~callback:(Closure.create callback))
    end;
  if !warn then begin
    eprintf "Glade-warning: handler %s is not used\n" name;
    flush stderr
  end

(* To list bindings *)
let ($) f g x = g (f x)
let show_option sh = function None -> "None" | Some x -> "Some " ^ sh x
let print_binding oc ~handler ~signal ~after ?target obj =
  Printf.fprintf oc "object=%s, signal=%s, handler=%s, target=%s\n"
   (get_widget_name (GtkBase.Widget.cast obj)) signal handler
   (show_option (GtkBase.Widget.cast $ get_widget_name) target)
let print_bindings oc xml =
  signal_autoconnect xml ~f:(print_binding oc); flush oc

let trace_handlers oc xml =
  signal_autoconnect xml ~f:
    begin fun ~handler ~signal ~after ?target obj ->
      let callback _ =
        if signal = "" then
          Printf.fprintf oc "Glade-debug: handler %s called\n" handler
        else
          Printf.fprintf oc
            "Glade-debug: %s called by signal %s on widget %s\n"
            handler signal (get_widget_name (GtkBase.Widget.cast obj));
        flush oc
      in
      ignore (GtkSignal.connect_by_name obj ~name:signal ~after
		~callback:(Closure.create callback))
    end
      
(* class skeleton, for use in generated wrappers *)

let create ?file ?data ?root ?domain () =
  init (); create ?file ?data ?root ?domain ()

class xml ?trace ?(autoconnect = true) (xmldata : glade_xml Gtk.obj) =
  let () = match trace with Some oc -> trace_handlers oc xmldata | None -> () in
  let () = if autoconnect then bind_handlers xmldata in
  object (self)
    val xml = xmldata
    method xml = xmldata
    method bind ~name ~callback =
      bind_handler ~name ~handler:(`Simple callback) ~warn:true xmldata
  end
