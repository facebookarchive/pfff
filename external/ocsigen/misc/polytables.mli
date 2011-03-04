(* Ocsimore
 * Copyright (C) 2008
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
   Polymorphic tables (using Map) 
   @author Vincent Balat
   @author Jérôme Vouillon
*)



(** Warning: this module is not thread safe! *)

(** The type of key for a piece of data of type 'a *)
type 'a key

(** The type of tables *)
type t

(** creates a new table *)
val create : unit -> t

(** create a new key for each data you want to save *)
val make_key : unit -> 'a key

(** [set t k v] associates [v] to [k] in [t] *)
val set : table:t -> key:'a key -> value:'a -> unit

(** [get t k] returns the current binding of [k] in [t] or raises [Not_found] *)
val get : table:t -> key:'a key -> 'a

(** [remove t k] remove the current binding of [k] in [t] if it exists *)
val remove : table:t -> key:'a key -> unit

(** [clear t] remove all data from t *)
val clear : table:t -> unit

