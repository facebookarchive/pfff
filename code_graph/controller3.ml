(* Yoann Padioleau
 * 
 * Copyright (C) 2012 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

let _set_title = ref (fun _s ->
  failwith "_set_title not defined"
)
let _statusbar_addtext = ref (fun _s ->
  failwith "_statusbar_addtext not defined"
)

let _label_settext = ref (fun _s ->
  failwith "_label_settext not defined"
)

let _refresh_drawing_area = ref (fun () ->
  failwith "_refresh_drawing_area not defined"
)

let current_motion_refresher = ref None
