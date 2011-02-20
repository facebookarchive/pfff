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

(** Firebug API (debugging console).

@see <http://getfirebug.com/wiki/index.php/Console_API>
the Firebug console API
*)

open Js

class type console = object
  method log : _ -> unit meth
  method log_2 : _ -> _ -> unit meth
  method log_3 : _ -> _ -> _ -> unit meth
  method log_4 : _ -> _ -> _ -> _ -> unit meth
  method log_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method log_6 : _ -> _ -> _ -> _ -> _ -> _ -> unit meth
  method log_7 : _ -> _ -> _ -> _ -> _ -> _ -> _ -> unit meth
  method log_8 : _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> unit meth
  method debug : _ -> unit meth
  method debug_2 : _ -> _ -> unit meth
  method debug_3 : _ -> _ -> _ -> unit meth
  method debug_4 : _ -> _ -> _ -> _ -> unit meth
  method debug_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method info : _ -> unit meth
  method info_2 : _ -> _ -> unit meth
  method info_3 : _ -> _ -> _ -> unit meth
  method info_4 : _ -> _ -> _ -> _ -> unit meth
  method info_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method warn : _ -> unit meth
  method warn_2 : _ -> _ -> unit meth
  method warn_3 : _ -> _ -> _ -> unit meth
  method warn_4 : _ -> _ -> _ -> _ -> unit meth
  method warn_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method error : _ -> unit meth
  method error_2 : _ -> _ -> unit meth
  method error_3 : _ -> _ -> _ -> unit meth
  method error_4 : _ -> _ -> _ -> _ -> unit meth
  method error_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method assert_ : bool t -> unit meth
  method assert_1 : bool t -> _ -> unit meth
  method assert_2 : bool t -> _ -> _ -> unit meth
  method assert_3 : bool t -> _ -> _ -> _ -> unit meth
  method assert_4 : bool t -> _ -> _ -> _ -> _ -> unit meth
  method assert_5 : bool t -> _ -> _ -> _ -> _ -> _ -> unit meth
  method dir : _ -> unit meth
  method dirxml : Dom.node t -> unit meth
  method trace : unit meth
  method group : _ -> unit meth
  method group_2 : _ -> _ -> unit meth
  method group_3 : _ -> _ -> _ -> unit meth
  method group_4 : _ -> _ -> _ -> _ -> unit meth
  method group_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method groupCollapsed : _ -> unit meth
  method groupCollapsed_2 : _ -> _ -> unit meth
  method groupCollapsed_3 : _ -> _ -> _ -> unit meth
  method groupCollapsed_4 : _ -> _ -> _ -> _ -> unit meth
  method groupCollapsed_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method groupEnd : unit meth
  method time : js_string t -> unit meth
  method timeEnd : js_string t -> unit meth
end

val console : console t
