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

(* $Id: gtkList.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels
open Gaux
open Gtk
open Tags
open GtkListProps
open GtkBase

external _gtklist_init : unit -> unit = "ml_gtklist_init"
let () = _gtklist_init ()

module ListItem = struct
  include ListItem
  external create_with_label : string -> list_item obj
      = "ml_gtk_list_item_new_with_label"
  let create ?label () =
    match label with None -> create []
    | Some label -> create_with_label label
end

module Liste = struct
  include Liste
  external insert_item :
      [>`list] obj -> [>`listitem] obj -> pos:int -> unit
      = "ml_gtk_list_insert_item"
  let insert_items l wl ~pos =
    let wl = if pos < 0 then wl else List.rev wl in
    List.iter wl ~f:(insert_item l ~pos)
  let append_items l = insert_items l ~pos:(-1)
  let prepend_items l = insert_items l ~pos:0
  external clear_items : [>`list] obj -> start:int -> stop:int -> unit =
    "ml_gtk_list_clear_items"
  external select_item : [>`list] obj -> pos:int -> unit
      = "ml_gtk_list_select_item"
  external unselect_item : [>`list] obj -> pos:int -> unit
      = "ml_gtk_list_unselect_item"
  external select_child : [>`list] obj -> [>`listitem] obj -> unit
      = "ml_gtk_list_select_child"
  external unselect_child : [>`list] obj -> [>`listitem] obj -> unit
      = "ml_gtk_list_unselect_child"
  external child_position : [>`list] obj -> [>`listitem] obj -> int
      = "ml_gtk_list_child_position"
end

module CList = struct
  include Clist
  external create : cols:int -> clist obj = "ml_gtk_clist_new"
  external create_with_titles : string array -> clist obj
      = "ml_gtk_clist_new_with_titles"
  external get_rows : [>`clist] obj -> int = "ml_gtk_clist_get_rows"
  external get_columns : [>`clist] obj -> int = "ml_gtk_clist_get_columns"
  external get_focus_row : [>`clist] obj -> int
      = "ml_gtk_clist_get_focus_row"
  external set_hadjustment : [>`clist] obj -> [>`adjustment] obj -> unit
      = "ml_gtk_clist_set_hadjustment"
  external set_vadjustment : [>`clist] obj -> [>`adjustment] obj -> unit
      = "ml_gtk_clist_set_vadjustment"
  external get_hadjustment : [>`clist] obj -> adjustment obj
      = "ml_gtk_clist_get_hadjustment"
  external get_vadjustment : [>`clist] obj -> adjustment obj
      = "ml_gtk_clist_get_vadjustment"
  external set_shadow_type : [>`clist] obj -> shadow_type -> unit
      = "ml_gtk_clist_set_shadow_type"
  external set_selection_mode : [>`clist] obj -> selection_mode -> unit
      = "ml_gtk_clist_set_selection_mode"
  external set_reorderable : [>`clist] obj -> bool -> unit
      = "ml_gtk_clist_set_reorderable"
  external set_use_drag_icons : [>`clist] obj -> bool -> unit
      = "ml_gtk_clist_set_use_drag_icons"
  external set_button_actions :
      [>`clist] obj -> int -> button_action list -> unit
      = "ml_gtk_clist_set_button_actions"
  external freeze : [>`clist] obj -> unit = "ml_gtk_clist_freeze"
  external thaw : [>`clist] obj -> unit = "ml_gtk_clist_thaw"
  external column_titles_show : [>`clist] obj -> unit
      = "ml_gtk_clist_column_titles_show"
  external column_titles_hide : [>`clist] obj -> unit
      = "ml_gtk_clist_column_titles_hide"
  external column_title_active : [>`clist] obj -> int -> unit
      = "ml_gtk_clist_column_title_active"
  external column_title_passive : [>`clist] obj -> int -> unit
      = "ml_gtk_clist_column_title_passive"
  external column_titles_active : [>`clist] obj -> unit
      = "ml_gtk_clist_column_titles_active"
  external column_titles_passive : [>`clist] obj -> unit
      = "ml_gtk_clist_column_titles_passive"
  external set_column_title : [>`clist] obj -> int -> string -> unit
      = "ml_gtk_clist_set_column_title"
  external get_column_title : [>`clist] obj -> int -> string
      = "ml_gtk_clist_get_column_title"
  external set_column_widget : [>`clist] obj -> int -> [>`widget] obj -> unit
      = "ml_gtk_clist_set_column_widget"
  external get_column_widget : [>`clist] obj -> int -> widget obj
      = "ml_gtk_clist_get_column_widget"
  external set_column_justification :
      [>`clist] obj -> int -> justification -> unit
      = "ml_gtk_clist_set_column_justification"
  external set_column_visibility : [>`clist] obj -> int -> bool -> unit
      = "ml_gtk_clist_set_column_visibility"
  external set_column_resizeable : [>`clist] obj -> int -> bool -> unit
      = "ml_gtk_clist_set_column_resizeable"
  external set_column_auto_resize : [>`clist] obj -> int -> bool -> unit
      = "ml_gtk_clist_set_column_auto_resize"
  external columns_autosize : [>`clist] obj -> unit
      = "ml_gtk_clist_columns_autosize"
  external optimal_column_width : [>`clist] obj -> int -> int
      = "ml_gtk_clist_optimal_column_width"
  external set_column_width : [>`clist] obj -> int -> int -> unit
      = "ml_gtk_clist_set_column_width"
  external set_column_min_width : [>`clist] obj -> int -> int -> unit
      = "ml_gtk_clist_set_column_min_width"
  external set_column_max_width : [>`clist] obj -> int -> int -> unit
      = "ml_gtk_clist_set_column_max_width"
  external set_row_height : [>`clist] obj -> int -> unit
      = "ml_gtk_clist_set_row_height"
  external moveto :
      [>`clist] obj ->
      int -> int -> row_align:clampf -> col_align:clampf -> unit
      = "ml_gtk_clist_moveto"
  external row_is_visible : [>`clist] obj -> int -> visibility
      = "ml_gtk_clist_row_is_visible"
  external get_cell_type : [>`clist] obj -> int -> int -> cell_type
      = "ml_gtk_clist_get_cell_type"
  external set_text : [>`clist] obj -> int -> int -> string -> unit
      = "ml_gtk_clist_set_text"
  external get_text : [>`clist] obj -> int -> int -> string
      = "ml_gtk_clist_get_text"
  external set_pixmap :
      [>`clist] obj ->
      int -> int -> Gdk.pixmap -> Gdk.bitmap Gpointer.optboxed -> unit
      = "ml_gtk_clist_set_pixmap"
  external get_pixmap :
      [>`clist] obj -> int -> int -> Gdk.pixmap option * Gdk.bitmap option
      = "ml_gtk_clist_get_pixmap"
  external set_pixtext :
      [>`clist] obj -> int -> int ->
      string -> int -> Gdk.pixmap -> Gdk.bitmap Gpointer.optboxed -> unit
      = "ml_gtk_clist_set_pixtext_bc" "ml_gtk_clist_set_pixtext"
  external set_foreground :
      [>`clist] obj -> row:int -> Gdk.color Gpointer.optboxed -> unit
      = "ml_gtk_clist_set_foreground"
  external set_background :
      [>`clist] obj -> row:int -> Gdk.color Gpointer.optboxed -> unit
      = "ml_gtk_clist_set_background"
  external get_cell_style : [>`clist] obj -> int -> int -> Gtk.style
      = "ml_gtk_clist_get_cell_style"
  external set_cell_style : [>`clist] obj -> int -> int -> Gtk.style -> unit
      = "ml_gtk_clist_set_cell_style"
  external get_row_style : [>`clist] obj -> row:int -> Gtk.style
      = "ml_gtk_clist_get_row_style"
  external set_row_style : [>`clist] obj -> row:int -> Gtk.style -> unit
      = "ml_gtk_clist_set_row_style"
  external set_selectable : [>`clist] obj -> row:int -> bool -> unit
      = "ml_gtk_clist_set_selectable"
  external get_selectable : [>`clist] obj -> row:int -> bool
      = "ml_gtk_clist_get_selectable"
  external set_shift :
      [>`clist] obj -> int -> int -> vertical:int -> horizontal:int -> unit
      = "ml_gtk_clist_set_shift"
  external insert : [>`clist] obj -> row:int -> Gpointer.optstring array -> int
      = "ml_gtk_clist_insert"
  let insert w ~row texts =
    let len = get_columns w in
    if List.length texts > len then invalid_arg "CList.insert";
    let arr = Array.create (get_columns w) None in
    List.fold_left texts ~init:0
      ~f:(fun pos text -> arr.(pos) <- text; pos+1);
    let r = insert w ~row (Array.map ~f:Gpointer.optstring arr) in
    if r = -1 then invalid_arg "GtkCList::insert";
    r
  external remove : [>`clist] obj -> row:int -> unit
      = "ml_gtk_clist_remove"
  external set_row_data : [>`clist] obj -> row:int -> Obj.t -> unit
      = "ml_gtk_clist_set_row_data"
  external get_row_data : [>`clist] obj -> row:int -> Obj.t
      = "ml_gtk_clist_get_row_data"
  external select : [>`clist] obj -> int -> int -> unit
      = "ml_gtk_clist_select_row"
  external unselect : [>`clist] obj -> int -> int -> unit
      = "ml_gtk_clist_unselect_row"
  external clear : [>`clist] obj -> unit = "ml_gtk_clist_clear"
  external get_row_column : [>`clist] obj -> x:int -> y:int -> int * int
      = "ml_gtk_clist_get_selection_info"
  external select_all : [>`clist] obj -> unit = "ml_gtk_clist_select_all"
  external unselect_all : [>`clist] obj -> unit = "ml_gtk_clist_unselect_all"
  external swap_rows : [>`clist] obj -> int -> int -> unit
      = "ml_gtk_clist_swap_rows"
  external row_move : [>`clist] obj -> int -> dst:int -> unit
      = "ml_gtk_clist_row_move"
  external set_sort_column : [>`clist] obj -> int -> unit
      = "ml_gtk_clist_set_sort_column"
  external set_sort_type : [>`clist] obj -> sort_type -> unit
      = "ml_gtk_clist_set_sort_type"
  external sort : [>`clist] obj -> unit
      = "ml_gtk_clist_sort"
  external set_auto_sort : [>`clist] obj -> bool -> unit
      = "ml_gtk_clist_set_auto_sort"
  let set_titles_show w = function
      true -> column_titles_show w
    | false -> column_titles_hide w
  let set_titles_active w = function
      true -> column_titles_active w
    | false -> column_titles_passive w
  let setter ~cont ?hadjustment ?vadjustment ?(button_actions=[])
      ?titles_show =
    cont (fun w ->
      let may_set f param = may param ~f:(f w) in
      may_set set_hadjustment hadjustment;
      may_set set_vadjustment vadjustment;
      List.iter button_actions ~f:(fun (n,act) -> set_button_actions w n act);
      may_set set_titles_show titles_show)
  let set_sort w ?auto ?column ?dir:sort_type () =
    may auto ~f:(set_auto_sort w);
    may column ~f:(set_sort_column w);
    may sort_type ~f:(set_sort_type w)
  let set_cell w ?text ?pixmap ?mask ?(spacing=0) ?style row col =
    begin match text, pixmap with
    | Some text, None ->
        set_text w row col text
    | None, Some pm ->
        set_pixmap w row col pm (Gpointer.optboxed mask)
    | Some text, Some pm ->
        set_pixtext w row col text spacing pm (Gpointer.optboxed mask)
    | _ -> ()
    end;
    may style ~f:(set_cell_style w row col)
  let set_column w ?widget ?title ?title_active ?justification
      ?visibility ?resizeable ?auto_resize ?width ?min_width ?max_width
      col =
    let may_set f param = may param ~f:(f w col) in
    may_set set_column_widget widget;
    may_set set_column_title title;
    may title_active
      ~f:(fun active -> if active then column_title_active w col
                                   else column_title_passive w col);
    may_set set_column_justification justification;
    may_set set_column_visibility visibility;
    may_set set_column_resizeable resizeable;
    may_set set_column_auto_resize auto_resize;
    may_set set_column_width width;
    may_set set_column_min_width min_width;
    may_set set_column_max_width max_width
  let set_row w ?foreground ?background ?selectable ?style row =
    let may_set f = may ~f:(f w ~row) in
    may_set set_foreground foreground;
    may_set set_background  background;
    may_set set_selectable  selectable;
    may_set set_row_style style

  external get_row_state :
      [>`clist] obj -> int -> Gtk.Tags.state_type
	  = "ml_gtk_clist_get_row_state"

  let emit_scroll =
    GtkSignal.emit ~conv:ignore ~emitter:
      (fun ~cont t ~(pos:clampf) ->
        cont [|`INT(Gpointer.encode_variant GtkEnums.scroll_type t);
               `FLOAT pos|])
end
