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

open Gaux
open GtkSourceView
open Gobject
open Gtk
open GtkBase
open GtkSourceView_types
open OgtkSourceViewProps
open GObj

let get_bool = function `BOOL x -> x | _ -> assert false
let bool x = `BOOL x
let get_uint = function `INT x -> x | _ -> assert false
let uint x = `INT x
let get_int = function `INT x -> x | _ -> assert false
let int x = `INT x
let get_gobject = function `OBJECT x -> x | _ -> assert false
let gobject x = `OBJECT (Some x)

let map_opt f = function
    None -> None
  | Some x -> Some (f x)

(** {2 GtkSourceTag} *)

type source_tag_property = [
  | `BACKGROUND of Gdk.color
  | `BOLD of bool
  | `FOREGROUND of Gdk.color
  | `ITALIC of bool
  | `STRIKETHROUGH of bool
  | `UNDERLINE of bool
]

let text_tag_property_of_source_tag_property = function
  | `BACKGROUND p -> `BACKGROUND_GDK p
  | `BOLD p -> `WEIGHT (if p then `BOLD else `NORMAL)
  | `FOREGROUND p -> `FOREGROUND_GDK p
  | `ITALIC p -> `STYLE (if p then `ITALIC else `NORMAL)
  | `STRIKETHROUGH p -> `STRIKETHROUGH p
  | `UNDERLINE p -> `UNDERLINE (if p then `SINGLE else `NONE)

let color_of_string s =
  Gdk.Color.alloc ~colormap: (Gdk.Color.get_system_colormap())
    (`NAME s)

class source_tag_style (obj: GtkSourceView_types.source_tag_style obj) =
  object (self)
    method as_source_tag_style = obj
    method copy = new source_tag_style (SourceTagStyle.copy obj)

    method background = SourceTagStyle.get_background obj
    method bold = SourceTagStyle.get_bold obj
    method foreground = SourceTagStyle.get_foreground obj
    method italic = SourceTagStyle.get_italic obj
    method strikethrough = SourceTagStyle.get_strikethrough obj
    method underline = SourceTagStyle.get_underline obj
    method use_background = SourceTagStyle.get_use_background obj
    method use_foreground = SourceTagStyle.get_use_foreground obj

    method set_background = SourceTagStyle.set_background obj
    method set_background_by_name s =
      self#set_background (color_of_string s)
    method set_bold = SourceTagStyle.set_bold obj
    method set_foreground = SourceTagStyle.set_foreground obj
    method set_foreground_by_name s =
      self#set_foreground (color_of_string s)
    method set_italic = SourceTagStyle.set_italic obj
    method set_strikethrough = SourceTagStyle.set_strikethrough obj
    method set_underline = SourceTagStyle.set_underline obj
    method set_use_background = SourceTagStyle.set_use_background obj
    method set_use_foreground = SourceTagStyle.set_use_foreground obj
  end

let source_tag_style
    ?background ?background_by_name ?bold
    ?foreground ?foreground_by_name ?italic
    ?strikethrough ?underline () =
  let st = new source_tag_style (SourceTagStyle.new_ ()) in
  (match background with None -> () | Some p -> st#set_background p);
  (match background_by_name with None -> () | Some p -> st#set_background_by_name p);
  (match bold with None -> () | Some p -> st#set_bold p);
  (match foreground with None -> () | Some p -> st#set_foreground p);
  (match foreground_by_name with None -> () | Some p -> st#set_foreground_by_name p);
  (match italic with None -> () | Some p -> st#set_italic p);
  (match strikethrough with None -> () | Some p -> st#set_strikethrough p);
  (match underline with None -> () | Some p -> st#set_underline p);
  (match background, background_by_name with
    None, None -> () | _ -> st#set_use_background true);
  (match foreground, foreground_by_name with
    None, None -> () | _ -> st#set_use_foreground true);
  st

type source_tag_id = string

class source_tag (obj: GtkSourceView_types.source_tag obj) =
  object (self)
    inherit GText.tag (obj :> [`texttag] obj)
    method as_source_tag = obj
    method id = Gobject.Property.get obj SourceTag.P.id

    method style : source_tag_style =
      let st =
	match SourceTag.get_style obj with
	  None ->
	    let st = SourceTagStyle.new_ () in
	    SourceTag.set_style obj st;
	    st
	| Some st -> st
      in
      new source_tag_style st
    method set_style (s:source_tag_style) =
      SourceTag.set_style obj s#as_source_tag_style

    method set_source_property (p:source_tag_property) =
      self#set_property (text_tag_property_of_source_tag_property p)
    method set_source_properties (l:source_tag_property list) =
      self#set_properties (List.map text_tag_property_of_source_tag_property l)

  end

let syntax_tag ~id ~name ~pat_start ~pat_end =
  new source_tag (SourceTag.syntax_tag ~id ~name ~pat_start ~pat_end)

let pattern_tag ~id ~name ~pat =
  new source_tag (SourceTag.pattern_tag ~id ~name ~pat)

let keyword_list_tag ~id ~name ~keywords
    ?(case_sensitive=true)
    ?(match_empty_string_at_beginning=false)
    ?(match_empty_string_at_end=false)
    ?beginning_regex ?end_regex () =
  new source_tag
    (SourceTag.keyword_list_tag
       ~id ~name ~keywords
       ~case_sensitive
       ~match_empty_string_at_beginning
       ~match_empty_string_at_end
       ~beginning_regex ~end_regex)


let block_comment_tag = syntax_tag

let line_comment_tag ~id ~name ~pat_start =
  new source_tag (SourceTag.line_comment_tag ~id ~name ~pat_start)

let string_tag ~id ~name ~pat_start ~pat_end ~end_at_line_end =
  new source_tag
    (SourceTag.string_tag
       ~id ~name ~pat_start ~pat_end ~end_at_line_end)

(** {2 GtkSourceTagTable} *)

class source_tag_table_signals obj' =
object
  inherit (['a] gobject_signals (obj' : [> GtkSourceView_types.source_tag_table] obj))
  inherit OgtkTextProps.text_tag_table_sigs
  inherit source_tag_table_sigs
end

class source_tag_table (obj: GtkSourceView_types.source_tag_table obj) =
  object (self)
    inherit GText.tag_table_skel (obj :> [`texttagtable] obj)
    method as_source_tag_table =
      (Gobject.try_cast obj "GtkSourceTagTable" :> [`sourcetagtable] obj)
    method connect = new source_tag_table_signals obj
    method misc = new gobject_ops obj
    method remove_source_tags () = SourceTagTable.remove_source_tags obj
    method add_tags (l:source_tag list) =
      SourceTagTable.add_tags obj (List.map (fun t -> t#as_source_tag) l)
  end

let source_tag_table () =
  new source_tag_table (SourceTagTable.new_())

(** {2 GtkSourceStyleScheme} *)

class source_style_scheme (obj: GtkSourceView_types.source_style_scheme obj) =
object(self)
  method as_source_style_scheme = obj
  method get_name = SourceStyleScheme.get_name obj
  method get_tag_style s =
    match SourceStyleScheme.get_tag_style obj s with
      None -> raise Not_found
    | Some o -> new source_tag_style o
end

let default_style_scheme () =
  new source_style_scheme (SourceStyleScheme.get_default ())

(** {2 GtkSourceLanguage} *)

class source_language_signals obj' =
object (self)
  inherit ['a] gobject_signals (obj' : [> GtkSourceView_types.source_language] obj)
  inherit source_language_sigs
end

class source_language (obj: GtkSourceView_types.source_language obj) =
object (self)
  method as_source_language = obj
  method connect = new source_language_signals obj
  method misc = new gobject_ops obj
  method get_name = SourceLanguage.get_name obj
  method get_section = SourceLanguage.get_section obj
  method get_escape_char = SourceLanguage.get_escape_char obj
  method get_style_scheme =
    new source_style_scheme (SourceLanguage.get_style_scheme obj)
  method set_style_scheme (s:source_style_scheme) =
    SourceLanguage.set_style_scheme obj s#as_source_style_scheme
  method get_tags =
    List.map (fun o -> new source_tag o) (SourceLanguage.get_tags obj)
  method get_tag_style id =
    new source_tag_style (SourceLanguage.get_tag_style obj id)
  method set_tag_style id (s:source_tag_style) =
    SourceLanguage.set_tag_style obj id s#as_source_tag_style
  method get_tag_default_style id =
    new source_tag_style (SourceLanguage.get_tag_default_style obj id)
end

(** {2 GtkSourceLanguagesManager} *)

class source_languages_manager
  (obj: GtkSourceView_types.source_languages_manager obj) =
object (self)
  method get_oid = Gobject.get_oid obj
  method as_source_languages_manager = obj
  method get_available_languages =
    List.map (fun o -> new source_language o)
      (SourceLanguagesManager.get_available_languages obj)
  method get_language_from_mime_type s =
    match SourceLanguagesManager.get_language_from_mime_type obj s with
    | None -> None
    | Some obj -> Some (new source_language obj)
  method lang_files_dirs = SourceLanguagesManager.get_lang_files_dirs obj
end

(* let source_languages_manager ?lang_files_dirs () =
  let properties =
    match lang_files_dirs with
    | None -> []
    | Some dirs ->
        let list_obj = gslist_of_string_list dirs in
        [Gobject.param
          "lang-files-dirs"
          (`OBJECT (Some list_obj))]
  in
  new source_languages_manager (SourceLanguagesManager.create properties) *)

let source_languages_manager () =
  new source_languages_manager (SourceLanguagesManager.create [])

let source_language_from_file ?languages_manager fname =
  let languages_manager =
    match languages_manager with
    | None -> source_languages_manager ()
    | Some lm -> lm
  in
  let manager_obj = languages_manager#as_source_languages_manager in
  match SourceLanguage.new_from_file fname manager_obj with
  | None -> None
  | Some lang_obj -> Some (new source_language lang_obj)


(** {2 GtkSourceMarker} *)

class source_marker  (obj: GtkSourceView_types.source_marker obj) =
object (self)
  method as_source_marker = obj
  method set_type = SourceMarker.set_type obj
  method get_type = SourceMarker.get_type obj
  method get_line = SourceMarker.get_line obj
  method get_name = SourceMarker.get_name obj
  method get_buffer = new source_buffer (SourceMarker.get_buffer obj)
  method next = new source_marker (SourceMarker.next obj)
  method prev = new source_marker (SourceMarker.prev obj)
end

(** {2 GtkSourceBuffer} *)

and source_buffer_signals obj' =
object
  inherit ['a] gobject_signals (obj' : [> GtkSourceView_types.source_buffer] obj)
  inherit GText.buffer_signals_skel
  inherit source_buffer_sigs
end

and source_buffer (obj: GtkSourceView_types.source_buffer obj) =
object (self)
  inherit GText.buffer_skel obj as text_buffer
  method as_source_buffer = obj
  method connect = new source_buffer_signals obj
  method misc = new gobject_ops obj
  method check_brackets = get_bool (self#misc#get_property "check-brackets")
  method set_check_brackets x = self#misc#set_property "check-brackets" (bool x)
  method set_bracket_match_style (st:source_tag_style) =
    SourceBuffer.set_bracket_match_style obj st#as_source_tag_style
  method highlight = get_bool (self#misc#get_property "highlight")
  method set_highlight x = self#misc#set_property "highlight" (bool x)
  method max_undo_levels = get_int (self#misc#get_property "max-undo-levels")
  method set_max_undo_levels x =
    self#misc#set_property "max-undo-levels" (int x)
  method language =
    match get_gobject (self#misc#get_property "language") with
    | None -> None
    | Some obj ->
        Some (new source_language (Gobject.try_cast obj "GtkSourceLanguage"))
  method set_language (x: source_language) =
    self#misc#set_property "language" (gobject x#as_source_language)
  method escape_char = get_uint (self#misc#get_property "escape-char")
  method set_escape_char x = self#misc#set_property "escape-char" (uint x)
  method can_undo = SourceBuffer.can_undo obj
  method can_redo = SourceBuffer.can_redo obj
  method undo () = SourceBuffer.undo obj
  method redo () = SourceBuffer.redo obj
  method begin_not_undoable_action () =
    SourceBuffer.begin_not_undoable_action obj
  method end_not_undoable_action () =
    SourceBuffer.end_not_undoable_action obj
  method source_tag_table =
    new source_tag_table (Gobject.try_cast self#tag_table "GtkSourceTagTable")

  method create_marker ?name ?typ (iter:GText.iter) =
    new source_marker(SourceBuffer.create_marker obj name typ iter#as_iter)

  method move_marker (m:source_marker) (iter:GText.iter) =
    SourceBuffer.move_marker obj m#as_source_marker iter#as_iter

  method delete_marker (m:source_marker) =
    SourceBuffer.delete_marker obj m#as_source_marker

  method get_marker s =
    match SourceBuffer.get_marker obj s
    with
      Some m -> new source_marker m
    | None -> raise Not_found
  method get_markers_in_region ~(start:GText.iter) ~(stop:GText.iter) =
    List.map (fun m -> new source_marker m)
      (SourceBuffer.get_markers_in_region obj start#as_iter stop#as_iter)
  method get_iter_at_marker (m:source_marker) =
    new GText.iter (SourceBuffer.get_iter_at_marker obj m#as_source_marker)
  method get_first_marker =
    map_opt (new source_marker) (SourceBuffer.get_first_marker obj)
  method get_last_marker =
    map_opt (new source_marker) (SourceBuffer.get_last_marker obj)
  method get_next_marker (it:GText.iter) =
    map_opt (new source_marker) (SourceBuffer.get_next_marker obj it#as_iter)
  method get_prev_marker (it:GText.iter) =
    map_opt (new source_marker) (SourceBuffer.get_prev_marker obj it#as_iter)

end

let source_buffer ?language ?(tag_table : source_tag_table option) ?text =
  let language =
    match language with
    | None -> None
    | Some source_language -> Some (source_language#as_source_language)
  in
  SourceBuffer.make_params [] ?language
    ~cont:(fun pl () ->
      let buf =
	match tag_table with
	  None ->
	    new source_buffer (SourceBuffer.create pl)
	| Some tt ->
	    let obj = SourceBuffer.new_ tt#as_source_tag_table in
	    Gobject.set_params (Gobject.try_cast obj "GtkSourceBuffer") pl;
	    new source_buffer obj
      in
      (match text with
      | None -> ()
      | Some text -> buf#set_text text);
      buf)

  (* alias used below, needed because "source_buffer" is a name in scope *)
let source_buffer' = source_buffer

(** {2 GtkSourceView} *)

class source_view_signals obj' =
object
  inherit widget_signals_impl (obj' : [> GtkSourceView_types.source_view] obj)
  inherit GText.view_signals obj'
  inherit source_view_sigs
end

class source_view (obj': GtkSourceView_types.source_view obj) =
object (self)
  inherit GText.view_skel obj'
  val source_buf =
    let buf_obj =
      Gobject.try_cast (GtkText.View.get_buffer obj') "GtkSourceBuffer"
    in
    new source_buffer buf_obj
  method source_buffer = source_buf
  method connect = new source_view_signals obj'
  method set_show_line_numbers x =
    self#misc#set_property "show_line_numbers" (bool x)
  method show_line_numbers =
    get_bool (self#misc#get_property "show_line_numbers")
  method set_show_line_markers x =
    self#misc#set_property "show_line_markers" (bool x)
  method show_line_markers =
    get_bool (self#misc#get_property "show_line_markers")
  method set_tabs_width x = self#misc#set_property "tabs_width" (uint x)
  method tabs_width = get_uint (self#misc#get_property "tabs_width")
  method set_auto_indent x = self#misc#set_property "auto_indent" (bool x)
  method auto_indent = get_bool (self#misc#get_property "auto_indent")
  method set_insert_spaces_instead_of_tabs x =
    self#misc#set_property "insert_spaces_instead_of_tabs" (bool x)
  method insert_spaces_instead_of_tabs =
    get_bool (self#misc#get_property "insert_spaces_instead_of_tabs")
  method set_highlight_current_line x = self#misc#set_property "highlight_current_line" (bool x)
  method highlight_current_line = get_bool (self#misc#get_property "highlight_current_line")
  method set_show_margin x = self#misc#set_property "show_margin" (bool x)
  method show_margin = get_bool (self#misc#get_property "show_margin")
  method set_margin x = self#misc#set_property "margin" (uint x)
  method margin = get_uint (self#misc#get_property "margin")
  method set_smart_home_end x = self#misc#set_property "smart_home_end" (bool x)
  method smart_home_end = get_bool (self#misc#get_property "smart_home_end")
  method set_marker_pixbuf = SourceView.set_marker_pixbuf obj
  method marker_pixbuf = SourceView.get_marker_pixbuf obj
  method set_cursor_color = SourceView.set_cursor_color obj
  method set_cursor_color_by_name s = SourceView.set_cursor_color obj (color_of_string s)
end

let source_view ?source_buffer =
  SourceView.make_params [] ~cont:(
    GtkText.View.make_params ~cont:(
      GContainer.pack_container ~create:(fun pl ->
        let obj =
          match source_buffer with
          | Some buf ->
              SourceView.new_with_buffer
                (Gobject.try_cast buf#as_buffer "GtkSourceBuffer")
          | None -> SourceView.new_ ()
        in
        Gobject.set_params (Gobject.try_cast obj "GtkSourceView") pl;
        new source_view obj)))

(** {2 Misc} *)

let find_matching_bracket iter =
  let iter = iter#copy in
  if SourceViewMisc.find_matching_bracket iter#as_iter then
    Some iter
  else
    None

let iter_forward_search (iter:GText.iter) flags
    ~start ~stop ?limit str =
  let limit = map_opt (fun x -> x#as_iter) limit in
  match SourceViewMisc.iter_forward_search iter#as_iter str
      flags ~start: start#as_iter ~stop: stop#as_iter limit
  with
    None -> None
  | Some (it1,it2) -> Some (new GText.iter it1, new GText.iter it2)

let iter_backward_search (iter:GText.iter) flags
    ~start ~stop ?limit str =
  let limit = map_opt (fun x -> x#as_iter) limit in
  match SourceViewMisc.iter_backward_search iter#as_iter str
      flags ~start: start#as_iter ~stop: stop#as_iter limit
  with
    None -> None
  | Some (it1,it2) -> Some (new GText.iter it1, new GText.iter it2)
