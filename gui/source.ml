open Common

open Highlight_code

type buffer_info = { 
  buffer: GText.buffer;
  pixbuf: GdkPixbuf.pixbuf;

  (* adjusted when insert pixbuf, must be larger than  *)
  posarray_adjust: int array; 
  (* prefs: highlighter_preferences; *)
}

(*****************************************************************************)
(* Buffer info *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* GText buffer and ast related position *)
(* ------------------------------------------------------------------------- *)

(* because inserting a pixbug actually shift the mapping filepos->bufpos
 * position information. 
 * if in file buffer has a 
 * X1 X2 X3 X4 X5
 * and in display buffer a 
 * X1 X2 PIX1 X3 X4 PIX2 X5
 * then the actual offset get shifted and you got 
 * X1 X2 PIX1_3 X3_4 X4_5 PIX2_6 X6_7
 * and in posarray_adjust we will have done a 
 * 0  0 +1 +1 +2  for the X1-X5 file buffer.
*)
let offset p bufinfo = 
  p + bufinfo.posarray_adjust.(p)

let file_offset_to_buf_offset p bufinfo = 
  offset p bufinfo

(* wrong: p - bufinfo.posarray_adjust.(p) 
 * to find original pos of let's say in previous example a X7_7, then
 * we have to go backward from the end of the buffer and try to find
 * the entry such as pos+i will give 7. Could use a reverse array
 * of posarray_adjust, but for now we need that info only in string_at_point
 * so ok to do it in a naive way.
 *)
let buf_offset_to_file_offset p bufinfo = 
  Common.array_find_index_typed (fun (Idx i) -> 
    i + bufinfo.posarray_adjust.(i) = p
  ) bufinfo.posarray_adjust
  +> Common.int_of_idx

(* ------------------------------------------------------------------------- *)
(* GText buffer and ast related position *)
(* ------------------------------------------------------------------------- *)

let iter_range_of_range bufinfo (p1, p2) = 
  let buffer = bufinfo.buffer in
  buffer#get_iter (`OFFSET (offset p1 bufinfo)),
  buffer#get_iter (`OFFSET (offset p2 bufinfo))


(* ------------------------------------------------------------------------- *)
(* gtk special need for color mapping *)
(* ------------------------------------------------------------------------- *)
(* gtk wants string for tags and can not use my category type, so need 
 * small workaround 
 *)

let _h_color_mapping = Hashtbl.create 101 
let _cnt = ref 0

let stag (buffer : GText.buffer) tag = 
  Common.memoized _h_color_mapping tag (fun () -> 

    incr _cnt;

    let info = info_of_category tag in
    let name = spf "%d" !_cnt in
    (* ex: buffer#create_tag ~name:"notparsed" [`BACKGROUND "lightgray"]; *)
    ignore (buffer#create_tag ~name:name info);
    name
  )



(* ------------------------------------------------------------------------- *)
(* GtkText color database *)
(* ------------------------------------------------------------------------- *)
(* obsolete *)
let build_tags (buffer : GText.buffer) = 
  
  raise Todo
  (* pad: todo I removed all_categories from highlight_code.ml *)
  (*
  all_categories +> List.iter (fun categ -> 
    let info = info_of_category categ in
    let name = stag buffer categ in
    (* ex: buffer#create_tag ~name:"notparsed" [`BACKGROUND "lightgray"]; *)
    ignore (buffer#create_tag ~name:name info)
  );
  *)


