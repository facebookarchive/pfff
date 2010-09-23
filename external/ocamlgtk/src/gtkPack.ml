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

(* $Id: gtkPack.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gobject
open Gtk
open Tags
open GtkPackProps
open GtkBase

external _gtkpack_init : unit -> unit = "ml_gtkpack_init"
let () = _gtkpack_init ()

module Box = struct
  include Box
  let pack box ?from:( dir = (`START : pack_type))
      ?(expand=false) ?(fill=true) ?(padding=0) child =
    (match dir with `START -> pack_start | `END -> pack_end)
      box child ~expand ~fill ~padding
end

module BBox = struct
  include ButtonBox
  (* Omitted defaults setting *)
  type bbox_style = [ `DEFAULT_STYLE|`SPREAD|`EDGE|`START|`END ]
  external get_child_width : [>`buttonbox] obj -> int
      = "ml_gtk_button_box_get_child_min_width"
  external get_child_height : [>`buttonbox] obj -> int
      = "ml_gtk_button_box_get_child_min_height"
  external get_child_ipadx : [>`buttonbox] obj -> int
      = "ml_gtk_button_box_get_child_ipad_x"
  external get_child_ipady : [>`buttonbox] obj -> int
      = "ml_gtk_button_box_get_child_ipad_y"
  external set_child_size :
      [>`buttonbox] obj -> width:int -> height:int -> unit
      = "ml_gtk_button_box_set_child_size"
  external set_child_ipadding : [>`buttonbox] obj -> x:int -> y:int -> unit
      = "ml_gtk_button_box_set_child_ipadding"
  let set_child_size w ?width ?height () =
    set_child_size w ~width:(may_default get_child_width w ~opt:width)
      ~height:(may_default get_child_height w ~opt:height)
  let set_child_ipadding w ?x ?y () =
    set_child_ipadding w
      ~x:(may_default get_child_ipadx w ~opt:x)
      ~y:(may_default get_child_ipady w ~opt:y)
  let set ?child_width ?child_height ?child_ipadx
      ?child_ipady ?layout w =
    if child_width <> None || child_height <> None then
      set_child_size w ?width:child_width ?height:child_height ();
    if child_ipadx <> None || child_ipady <> None then
      set_child_ipadding w ?x:child_ipadx ?y:child_ipady ();
    may layout ~f:(set P.layout_style w)
end

module Fixed = Fixed

module Layout = Layout

module Paned = Paned

module Table = struct
  include Table
  let has_x : expand_type -> bool =
    function `X|`BOTH -> true | `Y|`NONE -> false
  let has_y : expand_type -> bool =
    function `Y|`BOTH -> true | `X|`NONE -> false
  let attach t ~left ~top ?(right=left+1) ?(bottom=top+1)
      ?(expand=`NONE) ?(fill=`BOTH) ?(shrink=`NONE)
      ?(xpadding=0) ?(ypadding=0) w =
    let xoptions = if has_x shrink then [`SHRINK] else [] in
    let xoptions = if has_x fill then `FILL::xoptions else xoptions in
    let xoptions = if has_x expand then `EXPAND::xoptions else xoptions in
    let yoptions = if has_y shrink then [`SHRINK] else [] in
    let yoptions = if has_y fill then `FILL::yoptions else yoptions in
    let yoptions = if has_y expand then `EXPAND::yoptions else yoptions in
    attach t w ~left ~top ~right ~bottom ~xoptions ~yoptions
      ~xpadding ~ypadding
end

module Notebook = Notebook

module SizeGroup = SizeGroup
