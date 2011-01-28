(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Common

open Ast_php

module Ast = Ast_php

module V = Visitor_php
module Db = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * There are many places where we need to get access to the list of
 * entities defined in a file or used in a file.
 * 
 * This file is concerned with entities, that is Ast_php.name.
 * So for completness C-s for name in ast_php.ml and
 * see if all uses of it are covered. Other files are more concerned
 * about checks related to variables, that is Ast_php.dname,
 * as in check_variables_php.ml
 * 
 * todo: factorize code in
 *  - check_module.ml
 *  - lib_parsing_php manu get_xxx_any
 *  - database_php_build.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type def = unit

type use =
  Database_code.entity_kind * Ast_php.name

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

(* 
 * todo: similar to what is in database_php_build.ml
 *)
let defs_of_any any =
  V.do_visit_with_ref (fun aref -> { V.default_visitor with

    V.kqualifier = (fun (k, bigf) x ->
      (match x with
      | Qualifier _ -> () 
      | Self _ | Parent _ -> failwith "defs_uses_php: call unsugar_self_parent"
      );
      k x
    );


  }) any


(* 
 * Cover every cases ? C-s for 'name' in ast_php.ml.
 * update: C-s for 'xhp_tag' too.
 *
 * todo: check_module.ml and the places where we call checkClassName,
 * same than here ?
 * 
 * todo? right now I don't make a difference between Class and Interface.
 * If want then just intercept in the clas_def hook the processing
 * of 'implements'.
 * 
 * todo? do stuff for dynamic stuff like ClassNameRefDynamic ?
 * return a special Tag ? DynamicStuff ? So at least know they
 * are connections to more entities than one can infer statically.
 *)
let uses_of_any any = 

  V.do_visit_with_ref (fun aref -> { V.default_visitor with

    V.kqualifier = (fun (k, bigf) x ->
      (match x with
      | Qualifier _ -> ()
      | Self _ | Parent _ -> failwith "defs_uses_php: call unsugar_self_parent"
      );
      k x
    );

    (* This covers
     * - new X, instanceof X 
     *   (via class_name_reference), 
     * - X::Cst, X::$var, X::method(), X::$f() 
     *   (via qualifier)
     * - extends X, implements X, catch(X), functin foo(X $f)
     *   (via fully_qualified_class_name)
     *)
    V.kfully_qualified_class_name = (fun (k, bigf) classname ->
      (* todo? can interface define constant ? in which case
       * there is some ambiguity when seeing X::cst ...
       * could be the use of a Class or Interface.
       * So right now I just merge Class and Interace
       *)
      Common.push2 (Db.Class, classname) aref;
      k classname
    );

    (* xhp: there is also implicitely a new when we use a XHP tag *)
    V.kxhp_html = (fun (k, _) x ->
      match x with
      | Xhp (xhp_tag, _attrs, _tok, _body, _end) ->
          Common.push2 (Db.Class, (XhpName xhp_tag)) aref;
          k x
      | XhpSingleton (xhp_tag, _attrs, _tok) ->
          Common.push2 (Db.Class, (XhpName xhp_tag)) aref;
          k x
      (* todo: do it also for xhp attributes ? kxhp_tag then ?
       * (but take care to not include doublon because have
       * a xhp_tag for the open tag and one for the close.
       *)
    );
  }) any




