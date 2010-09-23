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

(* $Id: gToolbox.mli 1347 2007-06-20 07:40:34Z guesdon $ *)

(** Useful functions for LablGTK. *)

(** {2 Menus} *)

(** Tree description of a menu *)
type menu_entry =
  [ `I of string * (unit -> unit)
  | `C of string * bool * (bool -> unit)
  | `R of (string * bool * (bool -> unit)) list
  | `M of string * menu_entry list
  | `S ]

(** Build a menu from a tree description *)
val build_menu : GMenu.menu -> entries: menu_entry list -> unit

(** Popup a menu created from the given list of 
   labels and functions. *)
val popup_menu : entries: menu_entry list -> button: int -> time: int32 -> unit

(** {2 Parametrized dialog windows} *)

(**This function is used to display a question in a dialog box,
   with a parametrized list of buttons. The function returns the number
   of the clicked button (starting at 1), or 0 if the window is 
   savagedly destroyed.
   @param title the title of the dialog
   @param buttons the list of button labels.
   @param default the index of the default answer
   @param icon a widget (usually a pixmap) which can be displayed on the left
     of the window.
   @param message the text to display
*)
val question_box :
    title:string ->
    buttons:string list ->
    ?default:int -> ?icon:#GObj.widget -> string -> int

(**This function is used to display a message in a dialog box with just an Ok button.
   We use [question_box] with just an ok button.
   @param title the title of the dialog
   @param icon a widget (usually a pixmap) which can be displayed on the left
     of the window.
   @param ok the text for the ok button (default is "Ok")
   @param message the text to display
*)
val message_box :
    title:string -> ?icon:#GObj.widget -> ?ok:string -> string -> unit

(** Make the user type in a string. 
   @return [None] if the user clicked on cancel, or [Some s] if the user
   clicked on the ok button.
   @param title the title of the dialog
   @param ok the text for the confirmation button (default is "Ok")
   @param cancel the text for the cancel button (default is "Cancel")
   @param text the default text displayed in the entry widget
   @param message the text to display
*)
val input_string :
    title:string ->
    ?ok:string -> ?cancel:string -> ?text:string -> string -> string option

(** Make the user type in a text.
   @return [None] if the user clicked on cancel, or [Some s] if the user
   clicked on the ok button.
   @param title the title of the dialog
   @param ok the text for the confirmation button (default is "Ok")
   @param cancel the text for the cancel button (default is "Cancel")
   @param text the default text displayed in the entry widget (utf8)
   @param message the text to display
*)
val input_text :
    title:string ->
      ?ok:string -> ?cancel:string -> ?text:string -> string -> string option


(**This function allows the user to select a file and returns the
   selected file name.
   A VOIR : multi-selection ?
*)
val select_file :
    title:string ->
    ?dir:string ref -> ?filename:string -> unit -> string option

(** A tree. *)
type 'a tree = [ `L of 'a | `N of 'a * 'a tree list]

(** A class to make the user select a node in a tree.
   @param tree is the tree to display.
   @param label gives a label from the data of a node.
   @param info gives a (Utf8) string from the data of a node,
          to give more information to the user when he selects
          a node.
   @param width is the width of the tree widget
   @param height is the height of the tree widget
*)
class ['a] tree_selection :
  tree:'a tree ->
  label:('a -> string) ->
  info:('a -> string) ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit ->
  object
    inherit GObj.widget
    val obj : Gtk.widget Gtk.obj
    val mutable selection : 'a option
    method clear_selection : unit -> unit
    method selection : 'a option
    method wview : GText.view
    method wtree : GBroken.tree
  end

(** A function to make the user select a node in a tree.
   @param tree the to build a tree selection widget
   @param ok the text for the confirmation button (default is "Ok")
   @param cancel the text for the cancel button (default is "Cancel")
   @param title is the title of the window.
   @return The data associated to the selected node, or None
   if the user canceled the selection.
*)
val tree_selection_dialog :
  tree:'a tree ->
  label:('a -> string) ->
  info:('a -> string) ->
  title:string ->
  ?ok:string -> ?cancel:string ->
  ?width:int -> ?height:int ->
  ?show:bool -> unit -> 'a option

(** {2 Miscellaneous functions} *)

(** Resize the columns of a clist according to the length of the 
   content and the title of each column.*)
val autosize_clist : 'a GList.clist -> unit
