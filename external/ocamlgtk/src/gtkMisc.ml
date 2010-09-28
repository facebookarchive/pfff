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

(* $Id: gtkMisc.ml 1525 2010-09-09 06:49:49Z garrigue $ *)

open Gaux
open Gobject
open Gtk
open Tags
open GtkMiscProps
open GtkBase

external _gtkmisc_init : unit -> unit = "ml_gtkmisc_init"
let () = _gtkmisc_init ()

module GammaCurve = GammaCurve

module ColorSelection = ColorSelection

module Statusbar = Statusbar

module StatusIcon = GtkStatusIcon

module Calendar = Calendar

module DrawingArea = DrawingArea

module Curve = Curve

module Misc = struct
  include Misc
  let all_params ~cont =
    make_params ~cont:(Widget.size_params ~cont)
end

module Arrow = Arrow

module Image = Image

module Label = Label

module TipsQuery = TipsQuery

module Separator = Separator

module FontSelection = FontSelection
