(**************************************************************************)
(*     Lablgtk - Applications                                             *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*      Benjamin Monate  <Benjamin.Monate@free.fr>                        *)
(*      Olivier Andrieu  <oandrieu@nerim.net>                             *)
(*      Jun Furuse       <Jun.Furuse@inria.fr>                            *)
(*      Hubert Fauque    <hubert.fauque@wanadoo.fr>                       *)
(*      Koji Kagawa      <kagawa@eng.kagawa-u.ac.jp>                      *)
(*                                                                        *)
(**************************************************************************)

(* $Id: jg_memo.ml 1352 2007-07-12 08:56:18Z zoggy $ *)

type ('a, 'b) assoc_list =
    Nil
  | Cons of 'a * 'b * ('a, 'b) assoc_list

let rec assq key = function
    Nil -> raise Not_found
  | Cons (a, b, l) ->
      if key == a then b else assq key l

let fast ~f =
  let memo = ref Nil in
  fun key ->
    try assq key !memo
    with Not_found ->
      let data = f key in
      memo := Cons(key, data, !memo);
      data
  
  
