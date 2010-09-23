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

(* $Id: gtkMain.ml 1402 2008-03-25 08:55:03Z garrigue $ *)

open StdLabels
open Gtk

let () = Callback.register_exception "gtkerror" (Error"")
let () = Gc.set {(Gc.get()) with Gc.max_overhead = 1000000}

module Main = struct
  external init : string array -> string array = "ml_gtk_init"
  (* external set_locale : unit -> string = "ml_gtk_set_locale" *)
  external disable_setlocale : unit -> unit = "ml_gtk_disable_setlocale"
  (* external main : unit -> unit = "ml_gtk_main" *)
  let init ?(setlocale=true) () =
    let setlocale =
      try Sys.getenv "GTK_SETLOCALE" <> "0" with Not_found -> setlocale in
    if not setlocale then disable_setlocale ();
    let argv =
      try
	init Sys.argv
      with Error err ->
        raise (Error ("GtkMain.init: initialization failed\n" ^ err))
    in
    if setlocale then ignore (Glib.Main.setlocale `NUMERIC (Some "C"));
    Array.blit ~src:argv ~dst:Sys.argv ~len:(Array.length argv)
      ~src_pos:0 ~dst_pos:0;
    Obj.truncate (Obj.repr Sys.argv) (Array.length argv);
    if setlocale then Glib.Main.setlocale `ALL None else ""
  open Glib
  let loops = ref []
  let default_main () =
    let loop = (Main.create true) in
    loops := loop :: !loops;
    while Main.is_running loop do Main.iteration true done;
    if !loops <> [] then loops := List.tl !loops
  let main_func = ref default_main
  let main () = !main_func ()
  let quit () = if !loops <> [] then Main.quit (List.hd !loops)
  external get_version : unit -> int * int * int = "ml_gtk_get_version"
  let version = get_version ()
  external get_current_event_time : unit -> int32
    = "ml_gtk_get_current_event_time"
end

module Grab = struct
  external add : [>`widget] obj -> unit = "ml_gtk_grab_add"
  external remove : [>`widget] obj -> unit = "ml_gtk_grab_remove"
  external get_current : unit -> widget obj= "ml_gtk_grab_get_current"
end

module Event = struct
  (* May return GDK_CURRENT_TIME *)
  external get_current_time : unit -> int32
    = "ml_gtk_get_current_event_time"
  (* May raise Gpointer.Null *)
  external get_current : unit -> GdkEvent.any
    = "ml_gtk_get_current_event"
  (* May raise Gpointer.Null *)
  external get_widget : 'a Gdk.event -> widget obj
    = "ml_gtk_get_event_widget"
  external propagate : [> `widget] obj -> 'a Gdk.event -> unit
    = "ml_gtk_propagate_event"
end

module Rc = struct
  external add_default_file : string -> unit = "ml_gtk_rc_add_default_file"
  external parse : file:string -> unit = "ml_gtk_rc_parse"
  external parse_string : string -> unit = "ml_gtk_rc_parse_string"
end
