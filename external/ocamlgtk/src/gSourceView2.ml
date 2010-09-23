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
open GtkSourceView2
open Gobject
open Gtk
open GtkBase
open GtkSourceView2_types
open OgtkSourceView2Props
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

(** {2 GtkSourceStyleScheme} *)

class source_style_scheme (obj: GtkSourceView2_types.source_style_scheme obj) =
object(self)
  method as_source_style_scheme = obj
  method name = SourceStyleScheme.get_name obj
  method description = SourceStyleScheme.get_description obj
end


(** {2 GtkSourceStyleSchemeManager} *)

class source_style_scheme_manager
	(obj: GtkSourceView2_types.source_style_scheme_manager obj) =
  object(self)
    val obj = obj
    inherit source_style_scheme_manager_props

    method search_path =
      SourceStyleSchemeManager.get_search_path obj
    method set_search_path =
      SourceStyleSchemeManager.set_search_path obj
    method style_scheme_ids =
      SourceStyleSchemeManager.get_scheme_ids obj
    method style_scheme s =
      may_map (new source_style_scheme)
	(SourceStyleSchemeManager.get_scheme obj s)
  end

let source_style_scheme_manager ~default =
  let mgr =
    if default then SourceStyleSchemeManager.default ()
    else SourceStyleSchemeManager.new_ () in
  new source_style_scheme_manager mgr


(** {2 GtkSourceLanguage} *)

class source_language (obj: GtkSourceView2_types.source_language obj) =
object (self)
  method as_source_language = obj
  val obj = obj
  method misc = new gobject_ops obj

  method id = SourceLanguage.get_id obj
  method name = SourceLanguage.get_name obj
  method section = SourceLanguage.get_section obj
  method hidden = SourceLanguage.get_hidden obj

  method metadata s = SourceLanguage.metadata obj s
  method mime_types = SourceLanguage.mime_types obj
  method globs = SourceLanguage.globs obj
  method style_name s = SourceLanguage.style_name obj s
  method style_ids = SourceLanguage.style_ids obj
end

(** {2 GtkSourceLanguageManager} *)

class source_language_manager
  (obj: GtkSourceView2_types.source_language_manager obj) =
object (self)
  method get_oid = Gobject.get_oid obj
  method as_source_language_manager = obj

  method set_search_path p = SourceLanguageManager.set_search_path obj p
  method search_path = SourceLanguageManager.search_path obj
  method language_ids = SourceLanguageManager.language_ids obj

  method language id =
    may_map
      (new source_language)
      (SourceLanguageManager.language obj id )

  method guess_language ?filename ?content_type () =
    may_map
      (new source_language)
      (SourceLanguageManager.guess_language obj filename content_type)
end

let source_language_manager ~default =
  new source_language_manager
    (if default then SourceLanguageManager.default ()
     else SourceLanguageManager.create [])


(** {2 GtkSourceMark} *)

class source_mark  (obj: GtkSourceView2_types.source_mark obj) =
object (self)
  method coerce = (`MARK (GtkText.Mark.cast obj):GText.mark)
  method as_source_mark = obj
  val obj = obj
  inherit source_mark_props

  method next ?category () =
    may_map (fun m -> new source_mark m) (SourceMark.next obj category)
  method prev ?category () =
    may_map (fun m -> new source_mark m) (SourceMark.prev obj category)

end

let source_mark ?category () =
  new source_mark (SourceMark.create ?category [])

(** {2 GtkSourceBuffer} *)

class source_buffer_signals obj' =
object
  inherit ['a] gobject_signals (obj' : [> GtkSourceView2_types.source_buffer] obj)
  inherit GText.buffer_signals_skel
  inherit source_buffer_sigs
end

and source_buffer (_obj: GtkSourceView2_types.source_buffer obj) =
object (self)
  inherit GText.buffer_skel _obj as text_buffer
  val obj = _obj
  method private obj = _obj
  inherit source_buffer_props
  method as_source_buffer = obj
  method connect = new source_buffer_signals obj
  method misc = new gobject_ops obj
  method language = may_map (new source_language) (get SourceBuffer.P.language obj)
  method set_language (l:source_language option) =
    set SourceBuffer.P.language obj
      (may_map (fun l -> l#as_source_language) l)

  method style_scheme =
    may_map (new source_style_scheme) (get SourceBuffer.P.style_scheme obj)
  method set_style_scheme (s:source_style_scheme option) =
      match s with
        None -> ()
      | Some scheme -> set SourceBuffer.P.style_scheme obj
          (Some scheme#as_source_style_scheme)

  method undo () = SourceBuffer.undo obj
  method redo () = SourceBuffer.redo obj
  method begin_not_undoable_action () =
    SourceBuffer.begin_not_undoable_action obj
  method end_not_undoable_action () =
    SourceBuffer.end_not_undoable_action obj

  method create_source_mark ?name ?category (iter:GText.iter) =
    new source_mark(SourceBuffer.create_source_mark obj name category iter#as_iter)

  method source_marks_at_line ?category line =
    List.map
      (fun mark -> new source_mark mark)
      (SourceBuffer.get_source_marks_at_line obj line category)

  method source_marks_at_iter ?category (iter:GText.iter) =
    List.map
      (fun mark -> new source_mark mark)
      (SourceBuffer.get_source_marks_at_iter obj iter#as_iter category)

  method remove_source_marks ?category ~(start:GText.iter) ~(stop:GText.iter) () =
    SourceBuffer.remove_source_marks obj start#as_iter stop#as_iter category

  method forward_iter_to_source_mark ?category (iter:GText.iter) =
    SourceBuffer.forward_iter_to_source_mark obj iter#as_iter category

  method backward_iter_to_source_mark ?category (iter:GText.iter) =
    SourceBuffer.backward_iter_to_source_mark obj iter#as_iter category

  method ensure_highlight ~(start:GText.iter) ~(stop:GText.iter) =
    SourceBuffer.ensure_highlight obj start#as_iter stop#as_iter
end

let source_buffer ?(language:source_language option)
  ?(style_scheme:source_style_scheme option)
  ?(tag_table : GText.tag_table option) ?text =
  let language =
    match language with
    | None -> None
    | Some source_language -> Some (source_language#as_source_language)
  in
  let style_scheme =
    match style_scheme with
    | None -> None
    | Some schm -> Some (schm#as_source_style_scheme)
  in
  SourceBuffer.make_params [] ?language ?style_scheme
    ~cont:(fun pl () ->
      let buf =
	match tag_table with
	  None ->
	    new source_buffer (SourceBuffer.create pl)
	| Some tt ->
	    let obj = SourceBuffer.new_ tt#as_tag_table in
	    Gobject.set_params (Gobject.try_cast obj "GtkSourceBuffer") pl;
	    new source_buffer obj
      in
      (match text with
      | None -> ()
      | Some text -> buf#set_text text);
      buf)

(** {2 GtkSourceView} *)

class source_view_signals obj' =
object
  inherit widget_signals_impl (obj' : [> GtkSourceView2_types.source_view] obj)
  inherit GText.view_signals obj'
  inherit source_view_sigs
end

class source_view (obj': GtkSourceView2_types.source_view obj) =
object (self)
  inherit GText.view_skel obj'
  inherit source_view_props

  val source_buf =
    let buf_obj =
      Gobject.try_cast (GtkText.View.get_buffer obj') "GtkSourceBuffer"
    in
    new source_buffer buf_obj
  method source_buffer = source_buf
  method connect = new source_view_signals obj'

  method set_cursor_color = SourceView.set_cursor_color obj
  method set_cursor_color_by_name s = SourceView.set_cursor_color obj (color_of_string s)

  method draw_spaces = SourceView.get_draw_spaces obj
  method set_draw_spaces flags = SourceView.set_draw_spaces obj flags
end

let source_view ?source_buffer ?draw_spaces =
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
	may (SourceView.set_draw_spaces obj) draw_spaces;
        new source_view obj)))

(** {2 Misc} *)

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
