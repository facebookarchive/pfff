(* Ocsimore
 * Copyright (C) 2008
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(**
   Pretty print wiki to DOM elements
   @author Vincent Balat
*)

module Html = Dom_html

module W = Wikicreole

(*
let create n ?attrs children = 
  let m = create n ?attrs () in
  List.iter (Js.Node.append m) children ;
  m
*)
let node x = (x : #Dom.node Js.t :> Dom.node Js.t)

let (<|) e l = List.iter (fun c -> Dom.appendChild e c) l; node e

let list_builder d tag c =
  d##createElement (Js.string tag) <|
    (List.map
       (fun (c, l) ->
          d##createElement (Js.string "li") <|
            (c @ match l with Some v -> [v] | None -> []))
       c)

let builder =
  let d = Html.document in
  { W.chars = (fun s -> node (d##createTextNode (Js.string s)));
    W.strong_elem = (fun s -> d##createElement (Js.string "strong") <| s);
    W.em_elem = (fun s -> d##createElement (Js.string "em") <| s);
    W.a_elem =
      (fun addr s ->
         let a = Html.createA d in a##href <- Js.string addr; a <| s);
    W.br_elem = (fun () -> node (d##createElement (Js.string "br")));
    W.img_elem =
      (fun addr alt ->
         let i = Html.createImg d in
         i##src <- Js.string addr; i##alt <- Js.string alt;
         node i);
    W.tt_elem = (fun s -> d##createElement (Js.string "tt") <| s);
    W.p_elem = (fun s -> d##createElement (Js.string "p") <| s);
    W.pre_elem =
      (fun s ->
         let p = d##createElement (Js.string "pre") in
         Dom.appendChild p
           (d##createTextNode (Js.string (String.concat "" s)));
         node p);
    W.h1_elem = (fun s -> d##createElement (Js.string "h1") <| s);
    W.h2_elem = (fun s -> d##createElement (Js.string "h2") <| s);
    W.h3_elem = (fun s -> d##createElement (Js.string "h3") <| s);
    W.h4_elem = (fun s -> d##createElement (Js.string "h4") <| s);
    W.h5_elem = (fun s -> d##createElement (Js.string "h5") <| s);
    W.h6_elem = (fun s -> d##createElement (Js.string "h6") <| s);
    W.ul_elem = (fun s -> list_builder d "ul" s);
    W.ol_elem = (fun s -> list_builder d "ol" s);
    W.hr_elem = (fun () -> node (d##createElement (Js.string "hr")));
    W.table_elem =
      (fun rows ->
         let rows =
           List.map
             (fun entries ->
                d##createElement (Js.string "tr") <|
                  (List.map
                     (fun (h, c) ->
                        let kind = if h then "th" else "td" in
                        d##createElement (Js.string kind) <| c)
                     entries))
             rows
         in
         d##createElement (Js.string "table") <|
           [d##createElement (Js.string "tbody") <| rows]);
    W.inline = (fun x -> x)
  }

let xml_of_wiki s = Html.createDiv Html.document <| W.from_string builder s
