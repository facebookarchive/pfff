(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(*
FIX:
- example at first
*)
module Html = Dom_html

let (>>=) = Lwt.bind

let replace_child p n =
  Js.Opt.iter (p##firstChild) (fun c -> Dom.removeChild p c);
  Dom.appendChild p n

let onload _ =
  let d = Html.document in
  let body =
    Js.Opt.get (d##getElementById(Js.string "wiki_demo"))
      (fun () -> assert false) in
  let textbox = Html.createTextarea d in
  textbox##rows <- 20; textbox##cols <- 80;
  let preview = Html.createDiv d in
  preview##style##border <- Js.string "1px black dashed";
  preview##style##padding <- Js.string "5px";
  Dom.appendChild body textbox;
  Dom.appendChild body (Html.createBr d);
  Dom.appendChild body preview;
  let rec dyn_preview old_text n =
    let text = Js.to_string (textbox##value) in
    let n =
      if text <> old_text then begin
        begin try
          let rendered = Wiki_syntax.xml_of_wiki text in
          replace_child preview rendered;
        with _ -> () end;
        20
      end else
        max 0 (n - 1)
    in
    Lwt_js.sleep (if n = 0 then 0.5 else 0.1) >>= fun () ->
    dyn_preview text n
  in
  ignore (dyn_preview "" 0);
  Js._false

let _ =
Html.window##onload <- Html.handler onload
