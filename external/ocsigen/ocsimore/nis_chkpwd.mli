(* NIS password checker using ypmatch
 *
 * Copyright (C) 2008 Stéphane Glondu
 *   (Laboratoire PPS - CNRS - Université Paris Diderot)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.  *)


val crypt_passwd : string -> string Lwt.t
(** [crypt_passwd passwd] encrypts [passwd] using the crypt(3) function.
    The salt is automatically randomly chosen *)

val check_passwd : passwd:string -> hash:string -> bool Lwt.t
(** [check_passwd passwd hash] checks whether [hash] is correct for [passwd] *)

val check_nis : login:string -> passwd:string -> bool Lwt.t
(** [check_nis login passwd] checks whether [passwd] is password of
    [login] in NIS. *)

val userinfo : string -> Unix.passwd_entry option Lwt.t
(** [userinfo user] returns the information associated
    to the user [user], or [None] if [user] is not
    in the NIS database. *)
