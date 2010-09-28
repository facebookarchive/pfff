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

(* $Id: gtk.ml 1525 2010-09-09 06:49:49Z garrigue $ *)

open Gobject

exception Error of string

type 'a optobj = 'a obj Gpointer.optboxed
type clampf = float

module Tags = struct
  type anchor_type = [ `CENTER|`NORTH|`NW|`NE|`SOUTH|`SW|`SE|`WEST|`EAST ]
  type arrow_type = [ `UP|`DOWN|`LEFT|`RIGHT ]
  type attach_options = [ `EXPAND|`SHRINK|`FILL ]
  type button_box_style = [ `DEFAULT_STYLE|`SPREAD|`EDGE|`START|`END ]
  type curve_type = [ `LINEAR|`SPLINE|`FREE ]
  type delete_type =
    [ `CHARS|`WORD_ENDS|`WORDS|`DISPLAY_LINES|`DISPLAY_LINE_ENDS
    | `PARAGRAPH_ENDS|`PARAGRAPHS|`WHITESPACE ]
  type direction_type = [ `TAB_FORWARD|`TAB_BACKWARD|`UP|`DOWN|`LEFT|`RIGHT ]
  type expander_style =
    [ `COLLAPSED|`SEMI_COLLAPSED|`SEMI_EXPANDED|`EXPANDED ]
  type icon_size =
    [ `INVALID|`MENU|`SMALL_TOOLBAR|`LARGE_TOOLBAR|`BUTTON|`DND|`DIALOG ]
  type side_type = [ `TOP|`BOTTOM|`LEFT|`RIGHT ]
  type text_direction = [ `NONE|`LTR|`RTL ]
  type justification = [ `LEFT|`RIGHT|`CENTER|`FILL ]
  type match_type = [ `ALL|`ALL_TAIL|`HEAD|`TAIL|`EXACT|`LAST ]
  type menu_direction = [ `PARENT|`CHILD|`NEXT|`PREV ]
  type metric_type = [ `PIXELS|`INCHES|`CENTIMETERS ]
  type movement_step =
    [ `LOGICAL_POSITIONS|`VISUAL_POSITIONS|`WORDS|`DISPLAY_LINES
    | `DISPLAY_LINE_ENDS|`PARAGRAPH_ENDS|`PARAGRAPHS|`PAGES|`BUFFER_ENDS ]
  type orientation = [ `HORIZONTAL|`VERTICAL ]
  type corner_type = [ `TOP_LEFT|`BOTTOM_LEFT|`TOP_RIGHT|`BOTTOM_RIGHT ]
  type pack_type = [ `START|`END ]
  type path_priority = [ `LOWEST|`GTK|`APPLICATION|`THEME|`RC|`HIGHEST ]
  type path_type = [ `WIDGET|`WIDGET_CLASS|`CLASS ]
  type policy_type = [ `ALWAYS|`AUTOMATIC|`NEVER ]
  type position = [ `LEFT|`RIGHT|`TOP|`BOTTOM ]
  type relief_style = [ `NORMAL|`HALF|`NONE ]
  type resize_mode = [ `PARENT|`QUEUE|`IMMEDIATE ]
  type signal_run_type = [ `FIRST|`LAST|`BOTH|`NO_RECURSE|`ACTION|`NO_HOOKS ]
  type scroll_type =
    [ `NONE|`JUMP|`STEP_FORWARD|`STEP_BACKWARD|`PAGE_BACKWARD|`PAGE_FORWARD
    | `STEP_UP|`STEP_DOWN|`PAGE_UP|`PAGE_DOWN|`STEP_LEFT|`STEP_RIGHT
    | `PAGE_LEFT|`PAGE_RIGHT|`START|`END ]
  type selection_mode = [ `NONE|`SINGLE|`BROWSE|`MULTIPLE ]
  type shadow_type = [ `NONE|`IN|`OUT|`ETCHED_IN|`ETCHED_OUT ]
  type state_type = [ `NORMAL|`ACTIVE|`PRELIGHT|`SELECTED|`INSENSITIVE ]
  type submenu_direction = [ `LEFT|`RIGHT ]
  type submenu_placement = [ `TOP_BOTTOM|`LEFT_RIGHT ]
  type toolbar_style = [ `ICONS|`TEXT|`BOTH|`BOTH_HORIZ ]
  type update_type = [ `CONTINUOUS|`DISCONTINUOUS|`DELAYED ]
  type visibility = [ `NONE|`PARTIAL|`FULL ]
  type window_position =
    [ `NONE|`CENTER|`MOUSE|`CENTER_ALWAYS|`CENTER_ON_PARENT ]
  type window_type = [ `TOPLEVEL|`POPUP ]
  type wrap_mode = [ `NONE|`CHAR|`WORD ]
  type sort_type = [ `ASCENDING|`DESCENDING ]

  type expand_type = [ `X|`Y|`BOTH|`NONE ]
  type update_policy = [ `ALWAYS|`IF_VALID|`SNAP_TO_TICKS ]
  type cell_type = [ `EMPTY|`TEXT|`PIXMAP|`PIXTEXT|`WIDGET ]
  type toolbar_child =
      [ `SPACE | `BUTTON | `TOGGLEBUTTON | `RADIOBUTTON | `WIDGET ]
  type toolbar_space_style = [ `EMPTY | `LINE ]
  type spin_type =
    [ `STEP_FORWARD | `STEP_BACKWARD | `PAGE_FORWARD | `PAGE_BACKWARD
    | `HOME | `END | `USER_DEFINED of float ]
  type accel_flag = [ `VISIBLE|`LOCKED ]
  type button_action = [ `SELECTS|`DRAGS|`EXPANDS ]
  type calendar_display_options =
    [ `SHOW_HEADING|`SHOW_DAY_NAMES|`NO_MONTH_CHANGE|`SHOW_WEEK_NUMBERS
    | `WEEK_START_MONDAY ]
  type spin_button_update_policy = [ `ALWAYS|`IF_VALID ]
  type progress_bar_style = [ `CONTINUOUS|`DISCRETE ]
  type progress_bar_orientation =
    [ `LEFT_TO_RIGHT|`RIGHT_TO_LEFT|`BOTTOM_TO_TOP|`TOP_TO_BOTTOM ]
  type dest_defaults = [ `MOTION|`HIGHLIGHT|`DROP|`ALL ]
  type target_flags = [ `SAME_APP|`SAME_WIDGET ]
  type text_window_type = [ `PRIVATE | `WIDGET | `TEXT | `LEFT
			  | `RIGHT | `TOP | `BOTTOM]
  type text_search_flag = [ `VISIBLE_ONLY | `TEXT_ONLY ]
  type tree_view_column_sizing = [ `GROW_ONLY | `AUTOSIZE | `FIXED ]
  type cell_renderer_mode = [ `INERT | `ACTIVATABLE | `EDITABLE ]
  type message_type = [ `INFO | `WARNING | `QUESTION | `ERROR ]
  type buttons = [ `NONE | `OK | `CLOSE | `CANCEL | `YES_NO | `OK_CANCEL ]
  type response =
    [ `NONE | `REJECT | `ACCEPT | `DELETE_EVENT
    | `OK | `CANCEL | `CLOSE  | `YES | `NO | `APPLY | `HELP ]
  type gtkobj_flags =
    [ `IN_DESTRUCTION | `FLOATING ]
  type widget_flags =
    [ gtkobj_flags | `TOPLEVEL | `NO_WINDOW | `REALIZED | `MAPPED | `VISIBLE
    | `SENSITIVE | `PARENT_SENSITIVE | `CAN_FOCUS | `HAS_FOCUS
    | `CAN_DEFAULT | `HAS_DEFAULT | `HAS_GRAB | `RC_STYLE | `COMPOSITE_CHILD
    | `NO_REPARENT | `APP_PAINTABLE | `RECEIVES_DEFAULT | `DOUBLE_BUFFERED ]
  type size_group_mode =
    [ `NONE | `HORIZONTAL | `VERTICAL | `BOTH ]
end
open Tags

type gtk_class

type accel_group
type clipboard

type style = [`style] obj
type 'a group = 'a obj option

type statusbar_message
type statusbar_context
type selection_data

type rectangle  = { x: int; y: int; width: int; height: int }
type target_entry = { target: string; flags: target_flags list; info: int }
type box_packing =
    { expand: bool; fill: bool; padding: int; pack_type: pack_type }

type adjustment = [`gtk|`adjustment]
type tooltips = [`gtk|`tooltips]
type widget = [`gtk|`widget]
type container = [widget|`container]
type bin = [container|`bin]
type alignment = [bin|`alignment]
type button = [bin|`button]
type toggle_button = [button|`togglebutton]
type radio_button = [button|`togglebutton|`radiobutton]
type color_button = [button|`colorbutton]
type font_button = [button|`fontbutton]
type link_button = [button|`linkbutton]
type scale_button = [button|`scalebutton]
type option_menu = [button|`optionmenu]
type event_box = [bin|`eventbox]
type frame = [bin|`frame]
type aspect_frame = [bin|`frame|`aspectframe]
type handle_box = [bin|`handlebox]
type invisible = [bin|`invisible]
type item = [bin|`item]
type list_item = [item|`listitem]
type menu_item = [item|`menuitem]
type image_menu_item = [menu_item| `imagemenuitem]
type check_menu_item = [item|`menuitem|`checkmenuitem]
type radio_menu_item = [item|`menuitem|`checkmenuitem|`radiomenuitem]
type tree_item = [item|`treeitem]
type scrolled_window = [bin|`scrolledwindow]
type viewport = [bin|`viewport]
type window = [bin|`window]
type assistant = [window|`assistant]
type dialog = [window|`dialog]
type message_dialog = [dialog|`messagedialog]
type color_selection_dialog = [dialog|`colorselectiondialog]
type input_dialog = [dialog|`inputdialog]
type file_selection = [dialog|`fileselection]
type font_selection_dialog = [dialog|`fontselectiondialog]
type plug = [window|`plug]
type box = [container|`box]
type button_box = [container|`box|`buttonbox]
type gamma_curve = [container|`buttonbox|`gamma]
type color_selection = [container|`box|`colorselection]
type font_selection = [container|`box|`fontselection]
type combo = [container|`box|`combo]
type statusbar = [container|`box|`statusbar]
type status_icon = [`gtkstatusicon]
type gtk_status_icon = status_icon obj
type clist = [container|`clist]
type fixed = [container|`fixed]
type layout = [container|`layout]
type liste = [container|`list]
type menu_shell = [container|`menushell]
type menu = [container|`menushell|`menu]
type menu_bar = [container|`menushell|`menubar]
type notebook = [container|`notebook]
type packer = [container|`packer]
type paned = [container|`paned]
type socket = [container|`socket]
type table = [container|`table]
type toolbar = [container|`toolbar]
type tool_item = [bin|`toolitem]
type separator_tool_item = [tool_item|`separatortoolitem]
type tool_button = [tool_item|`toolbutton]
type toggle_tool_button = [tool_button|`toggletoolbutton]
type radio_tool_button = [toggle_tool_button|`radiotoolbutton]
type menu_tool_button = [tool_button|`menutoolbutton]
type tree = [container|`tree]
type calendar = [widget|`calendar]
type drawing_area = [widget|`drawingarea]
type curve = [drawing_area|`curve]
type editable = [widget|`editable]
type entry = [editable|`entry]
type spin_button = [editable|`entry|`spinbutton]
type old_editable = [editable|`oldeditable]
type text = [old_editable|`text]
type misc = [widget|`misc]
type arrow = [misc|`arrow]
type image = [misc|`image]
type label = [misc|`label]
type tips_query = [misc|`label|`tipsquery]
type pixmap = [misc|`pixmap]
type progress = [widget|`progress]
type progress_bar = [widget|`progress|`progressbar]
type range = [widget|`range]
type scale = [widget|`range|`scale]
type scrollbar = [widget|`range|`scrollbar]
type ruler = [widget|`ruler]
type separator = [widget|`separator]

type text_view = [container|`textview]
type text_buffer = [`textbuffer] obj
type text_tag_table = [`texttagtable] obj
type text_tag = [`texttag] obj
type text_mark = [`textmark] obj
type text_child_anchor = [`textchildanchor] obj
type text_iter

type tree_view = [container|`treeview]
type tree_view_column = [`gtk|`celllayout|`treeviewcolumn]
type tree_selection = [`treeselection] obj
type tree_model = [`treemodel] obj
type tree_model_custom = [`custommodel|`treemodel] obj
type tree_sortable = [`treemodel|`tree_sortable] obj
type tree_model_sort = [`treemodelsort|`treesortable|`treemodel] obj
type tree_model_filter = [`treemodelfilter|`treemodel] obj
type tree_store = [`treestore|`treesortable|`treemodel] obj
type list_store = [`liststore|`treesortable|`treemodel] obj
type tree_iter
type tree_path
type row_reference
type cell_renderer = [`gtk|`cellrenderer]
type cell_renderer_pixbuf = [cell_renderer|`cellrendererpixbuf]
type cell_renderer_text = [cell_renderer|`cellrenderertext]
type cell_renderer_toggle = [cell_renderer|`cellrenderertoggle]
type cell_renderer_progress = [cell_renderer|`cellrendererprogress]
type cell_renderer_combo = [cell_renderer_text|`cellrenderercombo]
type cell_renderer_accel = [cell_renderer_text|`cellrendereraccel]

type icon_source
type icon_set
type icon_factory = [`iconfactory] obj

type size_group = [`sizegroup] obj

(* New widgets in 2.4 *)
type cell_layout = [`celllayout]
type combo_box = [bin|`combobox|cell_layout]
type combo_box_entry = [combo_box|`comboboxentry]
type expander = [bin|`expander]
type file_filter = [`gtk|`filefilter]
type file_chooser = [widget|`filechooser]
type entry_completion = [`entrycompletion|cell_layout] obj

type action = [`action]
type toggle_action = [action|`toggleaction]
type radio_action = [toggle_action|`radioaction]
type action_group = [`actiongroup]
type ui_manager = [`uimanager]

(* New widgets in 2.6 *)
type icon_view = [container|`iconview]
type about_dialog = [dialog|`aboutdialog]
type file_chooser_button = [box|`filechooserbutton|`filechooser]

(* New widgets in 2.12 *)
type tooltip = [`tooltip] obj

(* re-export Gobject.obj *)
type 'a obj = 'a Gobject.obj
  (* constraint 'a = [> `gtk] *)
  (* *Props modules break this *)

