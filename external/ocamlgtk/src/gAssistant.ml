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

(* $Id: gWindow.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gtk
open GtkBase
open GtkAssistant
open GtkMisc
open GObj
open OgtkAssistantProps
open GContainer

(** Assistants **)
class assistant_signals obj = object
  inherit container_signals_impl (obj : [> assistant] obj)
  inherit assistant_sigs
end

class assistant obj = object (self)
  inherit GWindow.window_skel obj
  method connect = new assistant_signals obj
  method current_page = Assistant.get_current_page obj 
  method set_current_page = Assistant.set_current_page obj 
  method n_pages = Assistant.get_n_pages obj
  method nth_page = Assistant.get_nth_page obj 
  method insert_page ?page_type ?title ?header_image ?side_image ?complete 
    ~position w
    = 
    let n = Assistant.insert_page obj w position in
    may (self#set_page_type w) page_type;
    may (self#set_page_title w) title;
    may (self#set_page_header_image w) header_image;
    may (self#set_page_side_image w) side_image;
    may (self#set_page_complete w) complete;
    n
  method append_page ?page_type ?title ?header_image ?side_image ?complete w = 
    self#insert_page ?page_type ?title ?header_image ?side_image ?complete ~position:(-1) w
  method prepend_page ?page_type ?title ?header_image ?side_image ?complete w = 
    self#insert_page ?page_type ?title ?header_image ?side_image ?complete ~position:0 w
  method set_page_type = Assistant.set_page_type obj
  method page_type = Assistant.get_page_type obj
  method set_page_title = Assistant.set_page_title obj
  method page_title = Assistant.get_page_title obj
  method set_page_header_image = Assistant.set_page_header_image obj
  method page_header_image = Assistant.get_page_header_image obj
  method set_page_side_image = Assistant.set_page_side_image obj
  method page_side_image = Assistant.get_page_side_image obj
  method set_page_complete = Assistant.set_page_complete obj
  method page_complete = Assistant.get_page_complete obj
  method add_action_widget = Assistant.add_action_widget obj
  method remove_action_widget = Assistant.remove_action_widget obj
  method update_buttons_state = Assistant.update_buttons_state obj
end

(*let assistant () =
  new assistant (Assistant.create [])
*)

let make_assistant ~create =
  GtkWindow.Window.make_params ~cont:(fun pl ?wm_name ?wm_class ->
    Container.make_params pl ~cont:(fun pl ?(show=false) () ->
      let (w : #GWindow.window_skel) = create pl in
      may w#set_wm_name wm_name;
      may w#set_wm_class wm_class;
      if show then w#show ();
      w))

let assistant =
  make_assistant [] ~create:(fun pl -> new assistant (Assistant.create []))
