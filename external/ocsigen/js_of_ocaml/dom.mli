(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
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

(** DOM binding

This is a partial binding to the DOM Core API.
*)

open Js

(** {2 DOM objects} *)

(** Specification of [NodeList] objects. *)
class type ['node] nodeList = object
  method item : int -> 'node t optdef meth
  method length : int readonly_prop
end

type nodeType =
    OTHER (* Will not happen *)
  | ELEMENT
  | ATTRIBUTE
  | TEXT
  | CDATA_SECTION
  | ENTITY_REFERENCE
  | ENTITY
  | PROCESSING_INSTRUCTION
  | COMMENT
  | DOCUMENT
  | DOCUMENT_TYPE
  | DOCUMENT_FRAGMENT
  | NOTATION

(** Specification of [Node] objects. *)
class type node = object
  method nodeName : js_string t readonly_prop
  method nodeValue : js_string t opt readonly_prop
  method nodeType : nodeType readonly_prop
  method parentNode : node t opt prop
  method childNodes : node nodeList t prop
  method firstChild : node t opt prop
  method lastChild : node t opt prop
  method previousSibling : node t opt prop
  method nextSibling : node t opt prop

  method insertBefore : node t -> node t opt -> node t meth
  method replaceChild : node t -> node t -> node t meth
  method removeChild : node t -> node t meth
  method appendChild : node t -> node t meth
  method hasChildNodes : bool t meth
  method cloneNode : bool t -> node t meth
end

(** Specification of [Element] objects. *)
class type element = object
  inherit node
  method tagName : js_string t readonly_prop
  method getAttribute : js_string t -> js_string t meth
  method setAttribute : js_string t -> js_string t -> unit meth
  method removeAttribute : js_string t -> unit meth
  method hasAttribute : js_string t -> bool t meth
  method getElementsByTagName : js_string t -> 'element nodeList t meth
end

(** Specification of [CharacterData] objects. *)
class type characterData = object
  inherit node
  method data : js_string t prop
  method length : int readonly_prop
  method subjs_stringData : int -> int -> js_string t meth
  method appendData : js_string t -> unit meth
  method insertData : int -> js_string t -> unit meth
  method deleteData : int -> int -> unit meth
  method replaceData : int -> int -> js_string t meth
end

(** Specification of [Text] objects. *)
class type text = characterData

(** Specification of [DocumentFragment] objects. *)
class type documentFragment = node

(** Specification of [Document] objects. *)
class type ['element] document = object
  inherit node
  method documentElement : 'element t readonly_prop
  method createDocumentFragment : documentFragment t meth
  method createElement : js_string t -> 'element t meth
  method createTextNode : js_string t -> text t meth
  method getElementById : js_string t -> 'element t opt meth
  method getElementsByTagName : js_string t -> 'element nodeList t meth
end

(** {2 Helper functions} *)

val insertBefore : #node t -> #node t -> #node t opt -> unit
  (** The expression [insertBefore n c p] behave the same as
      [n##insertBefore(c, p)] but avoid the need of coercing the
      different objects to [node t]. *)
val replaceChild : #node t -> #node t -> #node t -> unit
  (** The expression [replaceChild n c p] behave the same as
      [n##replaceChild(c, p)] but avoid the need of coercing the
      different objects to [node t]. *)
val removeChild : #node t -> #node t -> unit
  (** The expression [removeChild n c] behave the same as
      [n##removeChild(c)] but avoid the need of coercing the
      different objects to [node t]. *)
val appendChild : #node t -> #node t -> unit
  (** The expression [appendChild n c] behave the same as
      [n##appendChild(c)] but avoid the need of coercing the
      different objects to [node t]. *)
