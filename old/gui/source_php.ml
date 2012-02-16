open Common

open Highlight_php
open Source

module Db = Database_php

let process_one_ast 
    bufinfo 
    (id, toplevel, toks) 
    prefs 
    apply_tag 
    iter_end_of_info
    db 
    current_file  
  =

  let buffer = bufinfo.buffer in

  let tag info v = apply_tag bufinfo info v in

  let maybe_add_has_type_icon id info db =
    
    let pixbuf = bufinfo.pixbuf  in
    let iter = iter_end_of_info bufinfo (info) in
    
    if db.Db.defs.Db.id_type#haskey id
    then begin
      
      buffer#insert_pixbuf ~iter ~pixbuf;
      let pos = iter#offset in
      for i = pos to Array.length bufinfo.posarray_adjust - 1 do
        bufinfo.posarray_adjust.(i) <- bufinfo.posarray_adjust.(i) + 1;
      done;
    end;
  in

  visit_toplevel
    ~tag
    ~maybe_add_has_type_icon
    prefs
    (Some (id, current_file, db))
    (toplevel, toks)

