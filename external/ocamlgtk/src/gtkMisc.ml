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

(* $Id: gtkMisc.ml 1419 2008-09-22 13:37:06Z zoggy $ *)

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

(* Does not seem very useful ...
module Curve = struct
  type t = [widget drawing curve] obj
  let cast w : t = Object.try_cast w "GtkCurve"
  external create : unit -> t = "ml_gtk_curve_new"
  external reset : [>`curve] obj -> unit = "ml_gtk_curve_reset"
  external set_gamma : [>`curve] obj -> float -> unit
      = "ml_gtk_curve_set_gamma"
  external set_range :
      [>`curve] obj -> min_x:float -> max_x:float ->
      min_y:float -> max_y:float -> unit
      = "ml_gtk_curve_set_gamma"
end
*)

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
