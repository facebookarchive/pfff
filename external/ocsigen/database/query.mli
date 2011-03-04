(* macaque : query.mli
    MaCaQue : Macros for Caml Queries
    Copyright (C) 2009 Gabriel Scherer, Jérôme Vouillon

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library; see the file LICENSE.  If not, write to
    the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA.
*)

module type THREAD = sig
  include PGOCaml_generic.THREAD
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module type QUERY = sig
  module Db : PGOCaml_generic.PGOCAML_GENERIC
  val query : _ Db.t -> ?log:out_channel -> 'a Sql.query -> 'a Db.monad
  val view : _ Db.t -> ?log:out_channel -> ('a, _) Sql.view -> 'a list Db.monad
  val view_one : _ Db.t -> ?log:out_channel -> ('a, _) Sql.view -> 'a Db.monad
  val view_opt : _ Db.t -> ?log:out_channel -> ('a, _) Sql.view -> 'a option Db.monad
end

module Make : functor (Thread : THREAD) ->
  QUERY with type 'a Db.monad = 'a Thread.t

module Make_with_Db (Thread : THREAD)
  (Db : PGOCaml_generic.PGOCAML_GENERIC
   with type 'a monad = 'a Thread.t) :
    QUERY with module Db = Db

include QUERY
      with type 'a Db.monad = 'a PGOCaml.monad
      and type 'a Db.t = 'a PGOCaml.t
