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

 (* $Id: gText.ml 1499 2010-04-08 08:00:42Z garrigue $ *)

open StdLabels
open Gaux
open Gtk
open GtkBase
open GtkText
open OgtkTextProps
open GObj

type mark_name = [`INSERT | `SEL_BOUND | `NAME of string]
let mark_name = function
    `INSERT -> "insert"
  | `SEL_BOUND -> "selection_bound"
  | `NAME s -> s

type mark = [mark_name | `MARK of text_mark]

class child_anchor obj = object
  method get_oid = Gobject.get_oid obj
  method as_childanchor : text_child_anchor = obj
  method widgets = List.map (new widget) (ChildAnchor.get_widgets obj)
  method deleted = ChildAnchor.get_deleted obj
end

let child_anchor () = new child_anchor (ChildAnchor.create [])


class tag_signals obj = object (self)
  inherit ['a] gobject_signals obj
  method event = self#connect Tag.S.event
end

type tag_property0 = [
  | `BACKGROUND of string
  | `BACKGROUND_FULL_HEIGHT of bool
  | `BACKGROUND_FULL_HEIGHT_SET of bool
  | `BACKGROUND_GDK of Gdk.color
  | `BACKGROUND_SET of bool
  | `BACKGROUND_STIPPLE of Gdk.bitmap
  | `BACKGROUND_STIPPLE_SET of bool
  | `DIRECTION of Tags.text_direction
  | `EDITABLE of bool
  | `EDITABLE_SET of bool
  | `FAMILY of string
  | `FAMILY_SET of bool
  | `FONT of string
  | `FONT_DESC of Pango.font_description
  | `FOREGROUND of string
  | `FOREGROUND_GDK of Gdk.color
  | `FOREGROUND_SET of bool
  | `FOREGROUND_STIPPLE of Gdk.bitmap
  | `FOREGROUND_STIPPLE_SET of bool
  | `INDENT of int
  | `INDENT_SET of bool
  | `INVISIBLE of bool
  | `INVISIBLE_SET of bool
  | `JUSTIFICATION of Tags.justification
  | `JUSTIFICATION_SET of bool
  | `LANGUAGE of string
  | `LANGUAGE_SET of bool
  | `LEFT_MARGIN of int
  | `LEFT_MARGIN_SET of bool
  | `PIXELS_ABOVE_LINES of int
  | `PIXELS_ABOVE_LINES_SET of bool
  | `PIXELS_BELOW_LINES of int
  | `PIXELS_BELOW_LINES_SET of bool
  | `PIXELS_INSIDE_WRAP of int
  | `PIXELS_INSIDE_WRAP_SET of bool
  | `RIGHT_MARGIN of int
  | `RIGHT_MARGIN_SET of bool
  | `RISE of int
  | `RISE_SET of bool
  | `SCALE_SET of bool
  | `SIZE of int
  | `SIZE_POINTS of float
  | `SIZE_SET of bool
  | `STRETCH of Pango.Tags.stretch
  | `STRETCH_SET of bool
  | `STRIKETHROUGH of bool
  | `STRIKETHROUGH_SET of bool
  | `STYLE of Pango.Tags.style
  | `STYLE_SET of bool
  | `TABS_SET of bool
  | `UNDERLINE of Pango.Tags.underline
  | `UNDERLINE_SET of bool
  | `VARIANT of Pango.Tags.variant
  | `VARIANT_SET of bool
  | `WEIGHT_SET of bool
  | `WRAP_MODE of Tags.wrap_mode
  | `WRAP_MODE_SET of bool
]

type tag_property = [
  | `WEIGHT of Pango.Tags.weight
  | `SCALE of Pango.Tags.scale
  | tag_property0
]

let text_tag_param (x : tag_property) = text_tag_param
    (match x with
    | `WEIGHT w -> `WEIGHT (Pango.Tags.weight_to_int w)
    | `SCALE s  -> `SCALE (Pango.Tags.scale_to_float s)
    | #tag_property0 as x -> x)

class tag obj = object (self)
  method get_oid = Gobject.get_oid obj
  method as_tag : text_tag = obj
  method connect = new tag_signals obj
  method priority = Tag.get_priority obj
  method set_priority p = Tag.set_priority obj p
  (* [BM] my very first polymorphic method in OCaml...*)
  method event : 'a. 'a Gtk.obj -> GdkEvent.any -> Gtk.text_iter -> bool = 
    Tag.event obj 
  method set_property p = 
    Gobject.set_params obj [text_tag_param p]
  method set_properties l = 
    Gobject.set_params obj (List.map text_tag_param l)
  method get_property : 'a. (_,'a) Gobject.property -> 'a =
    Gobject.Property.get obj
end

let tag ?name () = new tag (Tag.create ?name [])

type contents =
    [ `CHAR of Glib.unichar
    | `PIXBUF of GdkPixbuf.pixbuf
    | `CHILD of child_anchor
    | `UNKNOWN ]

class nocopy_iter it =
object(self)
  val it = (it:text_iter)
  method forward_char = Iter.forward_char it
  method backward_char = Iter.backward_char it
  method forward_chars n = Iter.forward_chars it n
  method backward_chars n = Iter.backward_chars it n
  method forward_line = Iter.forward_line it
  method backward_line = Iter.backward_line it
  method forward_lines n = Iter.forward_lines it n
  method backward_lines n = Iter.backward_lines it n
  method forward_word_end = Iter.forward_word_end it
  method forward_word_ends n = Iter.forward_word_ends it n
  method backward_word_start = Iter.backward_word_start it
  method backward_word_starts n = Iter.backward_word_starts it n
  method forward_cursor_position = Iter.forward_cursor_position it
  method backward_cursor_position = Iter.backward_cursor_position it
  method forward_cursor_positions n = Iter.forward_cursor_positions it n
  method backward_cursor_positions n = Iter.backward_cursor_positions it n
  method forward_sentence_end = Iter.forward_sentence_end it
  method backward_sentence_start = Iter.backward_sentence_start it
  method forward_sentence_ends n = Iter.forward_sentence_ends it n
  method backward_sentence_starts n = Iter.backward_sentence_starts it n
  method forward_to_end = Iter.forward_to_end it
  method forward_to_line_end = Iter.forward_to_line_end it
  method forward_to_tag_toggle (tag : tag option) = 
    Iter.forward_to_tag_toggle it (may_map tag ~f:(fun t -> t#as_tag))
  method backward_to_tag_toggle (tag : tag option) = 
    Iter.backward_to_tag_toggle it (may_map tag ~f:(fun t -> t#as_tag))
  method set_offset = Iter.set_offset it
  method set_line = Iter.set_line it
  method set_line_offset = Iter.set_line_offset it
  method set_line_index = Iter.set_line_index it
  method set_visible_line_index = Iter.set_visible_line_index it
  method set_visible_line_offset = Iter.set_visible_line_offset it
  method forward_find_char ?(limit : iter option) f = 
    Iter.forward_find_char it f (may_map limit ~f:(fun t -> t#as_iter))
  method backward_find_char ?(limit : iter option) f = 
    Iter.backward_find_char it f (may_map limit ~f:(fun t -> t#as_iter))
end

and iter it =
object (self)
  val nocopy = new nocopy_iter it
  val it = (it: text_iter)
  method nocopy = nocopy 
  method as_iter = it
  method copy = new iter (Iter.copy it)

  method buffer = Iter.get_buffer it
  method offset = Iter.get_offset it
  method line = Iter.get_line it
  method line_offset = Iter.get_line_offset it
  method line_index = Iter.get_line_index it
  method visible_line_index = Iter.get_visible_line_index it
  method visible_line_offset = Iter.get_visible_line_offset it

  method char = Iter.get_char it
  method contents : contents =
    let c = Iter.get_char it in
    if c <> 0xfffc then `CHAR c else
    match Iter.get_pixbuf it with
      Some p -> `PIXBUF p
    | None -> match Iter.get_child_anchor it with
        Some c -> `CHILD (new child_anchor c)
      | None -> `UNKNOWN
  method get_slice ~(stop:iter) = Iter.get_slice it stop#as_iter
  method get_text ~(stop:iter) = Iter.get_text it stop#as_iter
  method get_visible_slice ~(stop:iter) = 
    Iter.get_visible_slice it stop#as_iter
  method get_visible_text ~(stop:iter) = 
    Iter.get_visible_text it stop#as_iter

  method marks = Iter.get_marks it
  method get_toggled_tags b = List.map (fun x -> new tag x) 
			      (Iter.get_toggled_tags it b)
  method begins_tag (tag : tag option) = 
    Iter.begins_tag it (may_map tag ~f:(fun t -> t#as_tag))
  method ends_tag (tag : tag option) = 
    Iter.ends_tag it (may_map tag ~f:(fun t -> t#as_tag))
  method toggles_tag (tag : tag option) = 
    Iter.toggles_tag it (may_map tag ~f:(fun t -> t#as_tag))
  method has_tag (t : tag) = Iter.has_tag it t#as_tag
  method tags = List.map (fun t -> new tag t) (Iter.get_tags it)

  method editable = Iter.editable it
  method can_insert = Iter.can_insert it
  method starts_word = Iter.starts_word it
  method ends_word = Iter.ends_word it
  method inside_word = Iter.inside_word it
  method starts_line = Iter.starts_line it
  method ends_line = Iter.ends_line it
  method starts_sentence = Iter.starts_sentence it
  method ends_sentence = Iter.ends_sentence it
  method inside_sentence = Iter.inside_sentence it
  method is_cursor_position = Iter.is_cursor_position it
  method chars_in_line = Iter.get_chars_in_line it
  method bytes_in_line = Iter.get_bytes_in_line it
  method language = Pango.Language.to_string (Iter.get_language it)
  method is_end = Iter.is_end it
  method is_start = Iter.is_start it

  method forward_char = let s = self#copy in s#nocopy#forward_char; s
  method backward_char = let s = self#copy in s#nocopy#backward_char; s
  method forward_chars n = let s = self#copy in s#nocopy#forward_chars n; s
  method backward_chars n = let s = self#copy in s#nocopy#backward_chars n; s
  method forward_line = let s = self#copy in s#nocopy#forward_line; s
  method backward_line = let s = self#copy in s#nocopy#backward_line; s
  method forward_lines n = let s = self#copy in s#nocopy#forward_lines  n; s
  method backward_lines n = let s = self#copy in s#nocopy#backward_lines  n; s
  method forward_word_end = let s = self#copy in s#nocopy#forward_word_end; s
  method forward_word_ends n =
    let s = self#copy in s#nocopy#forward_word_ends  n; s
  method backward_word_start =
    let s = self#copy in s#nocopy#backward_word_start; s
  method backward_word_starts n =
    let s = self#copy in s#nocopy#backward_word_starts  n; s
  method forward_cursor_position =
    let s = self#copy in s#nocopy#forward_cursor_position; s
  method backward_cursor_position =
    let s = self#copy in s#nocopy#backward_cursor_position; s
  method forward_cursor_positions n =
    let s = self#copy in s#nocopy#forward_cursor_positions  n; s
  method backward_cursor_positions n =
    let s = self#copy in s#nocopy#backward_cursor_positions  n; s
  method forward_sentence_end =
    let s = self#copy in s#nocopy#forward_sentence_end; s
  method backward_sentence_start =
    let s = self#copy in s#nocopy#backward_sentence_start; s
  method forward_sentence_ends n =
    let s = self#copy in s#nocopy#forward_sentence_ends  n; s
  method backward_sentence_starts n =
    let s = self#copy in s#nocopy#backward_sentence_starts  n; s

  method set_offset n = let s = self#copy in s#nocopy#set_offset n; s
  method set_line n = let s = self#copy in s#nocopy#set_line n; s
  method set_line_offset n = let s = self#copy in s#nocopy#set_line_offset n; s
  method set_line_index n = let s = self#copy in s#nocopy#set_line_index n; s
  method set_visible_line_index n =
    let s = self#copy in s#nocopy#set_visible_line_index n; s
  method set_visible_line_offset n =
    let s = self#copy in s#nocopy#set_visible_line_offset n; s

  method forward_to_end = let s = self#copy in s#nocopy#forward_to_end; s
  method forward_to_line_end =
    let s = self#copy in s#nocopy#forward_to_line_end; s
  method forward_to_tag_toggle (tag : tag option) = 
    let s = self#copy in s#nocopy#forward_to_tag_toggle tag; s
  method backward_to_tag_toggle (tag : tag option) = 
    let s = self#copy in s#nocopy#backward_to_tag_toggle tag; s

  method equal (a:iter) = Iter.equal it a#as_iter
  method compare (a:iter) = Iter.compare it a#as_iter
  method in_range ~(start:iter) ~(stop:iter)  = 
    Iter.in_range it start#as_iter stop#as_iter

 method forward_search ?flags ?(limit:iter option) s =
    may_map (Iter.forward_search it s ?flags
               (may_map limit ~f:(fun t -> t#as_iter)))
      ~f:(fun (s,t) -> new iter s, new iter t)
  method backward_search ?flags ?(limit : iter option) s =
    may_map (Iter.backward_search it s ?flags
               (may_map limit ~f:(fun t -> t#as_iter)))
      ~f:(fun (s,t) -> new iter s, new iter t)

  method forward_find_char ?limit f = 
    let s = self#copy in s#nocopy#forward_find_char ?limit f; s
  method backward_find_char ?limit f = 
    let s = self#copy in s#nocopy#backward_find_char ?limit f; s
end

(* let iter i = new iter (Iter.copy i) *)
let as_iter (it : iter) = it#as_iter

class tag_table_signals obj = object
  inherit ['a] gobject_signals obj
  inherit text_tag_table_sigs
end

class tag_table_skel obj = 
object
  val obj = (obj :> text_tag_table)
  method get_oid = Gobject.get_oid obj
  method as_tag_table : text_tag_table = obj
  method add =  TagTable.add obj
  method remove =  TagTable.remove obj
  method lookup =  TagTable.lookup obj
  method size = TagTable.get_size obj
end

class tag_table obj = 
object 
  inherit tag_table_skel obj
  method connect = new tag_table_signals obj
end
  
let tag_table () = 
  new tag_table (TagTable.create [])

class type buffer_signals_skel_type = 
  object
    method apply_tag :
      callback:(tag -> start:iter -> stop:iter -> unit) -> GtkSignal.id
    method begin_user_action : callback:(unit -> unit) -> GtkSignal.id
    method changed : callback:(unit -> unit) -> GtkSignal.id
    method delete_range :
      callback:(start:iter -> stop:iter -> unit) -> GtkSignal.id
    method end_user_action : callback:(unit -> unit) -> GtkSignal.id
    method insert_child_anchor :
      callback:(iter -> Gtk.text_child_anchor -> unit) -> GtkSignal.id
    method insert_pixbuf :
      callback:(iter -> GdkPixbuf.pixbuf -> unit) -> GtkSignal.id
    method insert_text : callback:(iter -> string -> unit) -> GtkSignal.id
    method mark_deleted : callback:(Gtk.text_mark -> unit) -> GtkSignal.id
    method mark_set :
      callback:(iter -> Gtk.text_mark -> unit) -> GtkSignal.id
    method modified_changed : callback:(unit -> unit) -> GtkSignal.id
    method remove_tag :
      callback:(tag -> start:iter -> stop:iter -> unit) -> GtkSignal.id
  end

class type ['b] buffer_signals_type = 
object ('a)
  inherit buffer_signals_skel_type
  method after : 'a
  method private connect :
    'c. ('b, 'c) GtkSignal.t -> callback:'c -> GtkSignal.id
end

class virtual buffer_signals_skel = 
object(self)
  inherit text_buffer_sigs
  method apply_tag ~callback = 
    self#connect Buffer.S.apply_tag
      ~callback:(fun tag start stop ->
        callback (new tag tag) ~start:(new iter start) ~stop:(new iter stop))
   method delete_range ~callback = 
    self#connect Buffer.S.delete_range
      ~callback:(fun start stop ->
        callback ~start:(new iter start) ~stop:(new iter stop))
   method insert_child_anchor ~callback = 
    self#connect Buffer.S.insert_child_anchor
      ~callback:(fun iter -> callback (new iter iter))
  method insert_pixbuf ~callback = 
    self#connect Buffer.S.insert_pixbuf
      ~callback:(fun iter -> callback (new iter iter))
  method insert_text ~callback = 
    self#connect Buffer.S.insert_text
      ~callback:(fun iter -> callback (new iter iter))
   method mark_set ~callback = 
    self#connect Buffer.S.mark_set
      ~callback:(fun it -> callback (new iter it))
   method remove_tag ~callback = 
    self#connect Buffer.S.remove_tag
      ~callback:(fun tag start stop ->
        callback (new tag tag) ~start:(new iter start) ~stop:(new iter stop))
end

class buffer_signals obj = 
object 
  inherit ['a] gobject_signals obj
  inherit buffer_signals_skel
end

exception No_such_mark of string

type position =
    [ `OFFSET of int | `LINE of int
    | `LINECHAR of int * int | `LINEBYTE of int * int
    | `START | `END | `ITER of iter | mark ]

class buffer_skel obj = object(self)
  val obj = (obj :> text_buffer)
  method private obj = obj
  inherit text_buffer_props
  method get_oid = Gobject.get_oid obj
  method as_buffer = obj
  method line_count = Buffer.get_line_count obj
  method char_count = Buffer.get_char_count obj
  (* method tag_table =  Buffer.get_tag_table obj *)
  method insert
    ?iter 
    ?(tag_names : string list = [])
    ?(tags : tag list = []) 
    text
    =  
    match tags,tag_names with
      | [],[] -> 
	  begin match iter with
	  | None      -> Buffer.insert_at_cursor obj text
	  | Some iter -> Buffer.insert obj (as_iter iter) text
	  end
      | _ ->
          begin match iter with
	  | None -> 
	      let insert_iter () =
                self#get_iter_at_mark `INSERT in
	      let start_offset = (insert_iter ())#offset in
	      Buffer.insert_at_cursor obj text;
	      let start = self#get_iter_at_char start_offset in
	      List.iter tags ~f:(self#apply_tag ~start ~stop:(insert_iter ()));
	      List.iter tag_names 
		~f:(self#apply_tag_by_name ~start ~stop:(insert_iter ())) 
	  | Some iter -> 
	      let start_offset = iter#offset in
	      Buffer.insert obj (as_iter iter) text;
	      let start = self#get_iter_at_char start_offset in
	      List.iter tags ~f:(self#apply_tag ~start ~stop:iter);
	      List.iter tag_names 
		~f:(self#apply_tag_by_name ~start ~stop:iter)
	end
  method insert_interactive ?iter ?(default_editable = true) text = 
    match iter with
    | None -> 
	Buffer.insert_interactive_at_cursor obj text default_editable
    | Some iter -> 
	Buffer.insert_interactive obj (as_iter iter) text default_editable
  method insert_range ~iter ~start ~stop = 
    Buffer.insert_range obj
      (as_iter iter) (as_iter start) (as_iter stop)
  method insert_range_interactive ~iter ~start ~stop
      ?(default_editable = true) () = 
    Buffer.insert_range_interactive obj (as_iter iter) (as_iter start)
      (as_iter stop)  default_editable
  method delete ~start ~stop = Buffer.delete obj (as_iter start) 
				 (as_iter stop)
  method delete_interactive ~start ~stop ?(default_editable = true) () = 
    Buffer.delete_interactive obj (as_iter start) 
      (as_iter stop) default_editable
  method set_text text = 
    Buffer.set_text obj text
  method get_text ?start ?stop ?(slice=false) ?(visible=false) () =
    let start,stop = 
      match start,stop with 
	| None,None -> Buffer.get_bounds obj
	| Some start,None -> as_iter start, Buffer.get_start_iter obj
	| None,Some stop -> Buffer.get_end_iter obj, as_iter stop
	| Some start,Some stop -> as_iter start, as_iter stop
    in
    (if slice then Buffer.get_slice else Buffer.get_text)
      obj start stop (not visible)
  method insert_pixbuf ~iter ~pixbuf = 
    Buffer.insert_pixbuf obj (as_iter iter) pixbuf
  method create_mark ?name ?(left_gravity=true) iter = 
    Buffer.create_mark obj name (as_iter iter) left_gravity
  method get_mark : mark -> _ = function
      `MARK mark -> mark
    | #mark_name as  mark ->
        let name = mark_name mark in
        match Buffer.get_mark obj name with 
        | None -> raise (No_such_mark name)
	| Some m -> m
  method move_mark mark ~where =
    Buffer.move_mark obj (self#get_mark mark) (as_iter where)
  method delete_mark mark = Buffer.delete_mark obj (self#get_mark mark)
  method place_cursor ~where = 
    Buffer.place_cursor obj (as_iter where)
  method select_range ins bound = 
    Buffer.select_range obj (as_iter ins) (as_iter bound)
  method apply_tag (tag : tag) ~start ~stop = 
    Buffer.apply_tag obj tag#as_tag (as_iter start) (as_iter stop)
  method remove_tag (tag : tag) ~start ~stop = 
    Buffer.remove_tag obj tag#as_tag (as_iter start) (as_iter stop)
  method apply_tag_by_name name ~start ~stop = 
    Buffer.apply_tag_by_name obj name (as_iter start) (as_iter stop)
  method remove_tag_by_name name ~start ~stop = 
    Buffer.remove_tag_by_name obj name (as_iter start) (as_iter stop)
  method remove_all_tags ~start ~stop =
    Buffer.remove_all_tags obj (as_iter start) (as_iter stop)
  method create_tag ?name properties =
    let t = new tag (Buffer.create_tag_0 obj name) in
    if properties <> [] then t#set_properties properties;
    t
  method get_iter (pos : position) =
    let it =
      match pos with
        `START -> Buffer.get_start_iter obj
      | `END -> Buffer.get_end_iter obj
      | `OFFSET n -> Buffer.get_iter_at_offset obj n
      | `LINE n -> Buffer.get_iter_at_line obj n
      | `LINECHAR (l,c) -> Buffer.get_iter_at_line_offset obj l c
      | `LINEBYTE (l,c) -> Buffer.get_iter_at_line_index  obj l c
      | `ITER it -> it#as_iter
      | #mark as mark ->
          Buffer.get_iter_at_mark obj (self#get_mark mark)
    in new iter it
  method get_iter_at_char ?line char_offset =
    match line,char_offset with
    | Some v, 0   -> new iter (Buffer.get_iter_at_line obj v)
    | None  , v -> new iter (Buffer.get_iter_at_offset obj v)
    | Some l, c -> new iter (Buffer.get_iter_at_line_offset obj l c)
  method get_iter_at_byte ~line index =
    new iter (Buffer.get_iter_at_line_index  obj line index)
  method get_iter_at_mark mark = 
    new iter (Buffer.get_iter_at_mark obj (self#get_mark mark))
  method start_iter = new iter (Buffer.get_start_iter obj)
  method end_iter = new iter (Buffer.get_end_iter obj)
  method bounds = 
    let s,t=Buffer.get_bounds obj in
    new iter s,new iter t
				
  method modified = Buffer.get_modified  obj
  method set_modified setting = Buffer.set_modified  obj setting
  method delete_selection ?(interactive=true) ?(default_editable=true) () = 
    Buffer.delete_selection obj interactive default_editable
  method selection_bounds =
    let start, stop = Buffer.get_selection_bounds obj in
    (new iter start, new iter stop)
  method begin_user_action () = Buffer.begin_user_action obj
  method end_user_action () = Buffer.end_user_action obj
  method create_child_anchor (iter:iter) = 
    new child_anchor (Buffer.create_child_anchor obj iter#as_iter)
  method insert_child_anchor (iter:iter) (child_anchor:child_anchor) = 
    Buffer.insert_child_anchor obj iter#as_iter child_anchor#as_childanchor
  method paste_clipboard ?iter ?(default_editable=true) clipboard = 
    Buffer.paste_clipboard obj (GData.as_clipboard clipboard)
      (may_map as_iter iter) default_editable
  method copy_clipboard clip =
    Buffer.copy_clipboard obj (GData.as_clipboard clip)
  method cut_clipboard ?(default_editable=true) clipboard = 
    Buffer.cut_clipboard obj (GData.as_clipboard clipboard) default_editable
  method add_selection_clipboard clip =
    Buffer.add_selection_clipboard obj (GData.as_clipboard clip)
  method remove_selection_clipboard clip =
    Buffer.remove_selection_clipboard obj (GData.as_clipboard clip)
end

class buffer obj = object
  inherit buffer_skel obj
  method connect = new buffer_signals obj
end

let buffer ?tag_table ?text () =
  let tag_table =
    match tag_table with None -> None | Some x -> Some x#as_tag_table in
  let b = new buffer (Buffer.create ?tag_table []) in
  match text with None -> b | Some t -> b#set_text t; b


class view_signals obj = object
  inherit widget_signals_impl (obj : [> Gtk.text_view] obj)
  inherit text_view_sigs
end

class view_skel obj = object (self)
  inherit [_] widget_impl obj
  inherit text_view_props
  method event = new GObj.event_ops obj
  method as_view = (obj :> text_view obj)
  method set_buffer (b:buffer) = View.set_buffer obj (b#as_buffer)
  method buffer = new buffer (View.get_buffer obj)
  method scroll_to_mark 
    ?(within_margin=0.) ?(use_align=false)  
    ?(xalign=0.) ?(yalign=0.) mark =  
    View.scroll_to_mark obj (self#buffer#get_mark mark)
      within_margin use_align xalign yalign
  method scroll_to_iter  ?(within_margin=0.) ?(use_align=false)
      ?(xalign=0.) ?(yalign=0.) iter =
    View.scroll_to_iter obj (as_iter iter) within_margin
      use_align xalign yalign
  method scroll_mark_onscreen mark =  
    View.scroll_mark_onscreen obj (self#buffer#get_mark mark)
  method move_mark_onscreen mark =  
    View.move_mark_onscreen obj (self#buffer#get_mark mark)
  method place_cursor_onscreen () =  
    View.place_cursor_onscreen obj
  method visible_rect =  View.get_visible_rect obj
  method get_iter_location iter = View.get_iter_location obj (as_iter iter)
  method get_line_at_y y =
    let it, n = View.get_line_at_y obj y in (new iter it, n)
  method get_line_yrange iter = View.get_line_yrange obj (as_iter iter)
  method get_iter_at_location ~x ~y =
    new iter (View.get_iter_at_location obj x y)
  method buffer_to_window_coords ~tag ~x ~y =
    View.buffer_to_window_coords obj tag x y
  method window_to_buffer_coords  ~tag ~x ~y =
    View.window_to_buffer_coords obj tag x y
  method get_window win = 
    View.get_window obj win
  method get_window_type win = 
    View.get_window_type obj win
  method set_border_window_size ~typ ~size =
    View.set_border_window_size obj typ size
  method get_border_window_size typ = 
    View.get_border_window_size obj typ
  method forward_display_line iter =
    View.forward_display_line obj (as_iter iter)
  method backward_display_line iter =
    View.backward_display_line obj (as_iter iter)
  method forward_display_line_end iter =
    View.forward_display_line_end obj (as_iter iter)
  method backward_display_line_start iter =
    View.backward_display_line_start obj (as_iter iter)
  method starts_display_line iter =
    View.starts_display_line obj (as_iter iter)
  method move_visually iter count =
    View.move_visually obj (as_iter iter) count
  method add_child_at_anchor (w : widget) (anchor : child_anchor) =
    View.add_child_at_anchor obj w#as_widget anchor#as_childanchor
  method add_child_in_window ~(child : widget) ~which_window ~x ~y =
    View.add_child_in_window obj child#as_widget which_window x y
  method move_child ~(child : widget) ~x ~y =
    View.move_child obj child#as_widget x y
end

class view obj = object
  inherit view_skel obj
  method connect = new view_signals obj
end

let view ?(buffer:buffer option) =
  View.make_params [] ~cont:(
  GContainer.pack_container ~create:(fun pl ->
    let w = match buffer with 
      | None -> View.create []
      | Some b -> View.create_with_buffer b#as_buffer
    in
    Gobject.set_params w pl; new view w))

