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

(* $Id: searchid.mli 1352 2007-07-12 08:56:18Z zoggy $ *)

val start_env : Env.t ref
val module_list : string list ref
val longident_of_path :  Path.t ->Longident.t

type pkind =
    Pvalue
  | Ptype
  | Plabel
  | Pconstructor
  | Pmodule
  | Pmodtype
  | Pclass
  | Pcltype

val string_of_kind :  pkind -> string

exception Error of int * int

val search_string_type :
      string -> mode:[`Exact|`Included] -> (Longident.t * pkind) list
val search_pattern_symbol : string -> (Longident.t * pkind) list
val search_string_symbol : string -> (Longident.t * pkind) list

val search_structure :
    Parsetree.structure ->
    name:string -> kind:pkind -> prefix:string list -> int
val search_signature :
    Parsetree.signature ->
    name:string -> kind:pkind -> prefix:string list -> int
