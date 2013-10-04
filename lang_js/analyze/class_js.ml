(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2012 Facebook
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

open Ast_js

module Ast = Ast_js
module V = Visitor_js
module Db = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Reverse-engineering the class structure of a Javascript file.
 * 
 * Javascript has no notion of class (or module); it's an object-based
 * language like Self using *prototypes*. Nevertheless people have
 * defined different libraries and conventions to define classes on
 * top of those objects. Prototype and Jquery are probably the 
 * most famous ones.
 * 
 * This module tries to reverse-engineer those conventions.
 * By understanding the class structure we can then
 * provide better completion on entities by knowing for instance that 
 * some nested functions are actually static methods of a specific class.
 * 
 * In addition to the Prototype library, this module also handles the 
 * Javelin conventions and the copy_properties() idiom.
 * Javelin, aka JX, is a javascript framework developed by Facebook.
 * See http://javelin.fbdocs.com/ It adds some functions like
 * JX.install and conventions such as using the 'statics', 'members' 
 * fields to give a class structure to javascript code.
 * 
 * Because the highlighter needs also to recognize those idioms,
 * highlight_js.ml now calls this module.
 * 
 * to test:
 *   $ ./pfff_db_light -batch_mode -readable_db -lang js ~/www/html/intern/js/
 * which right now does not generate any warning.
 * 
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* Many of the patterns were written with the help of
 * ./pfff_misc -dump_js_ml tests/facebook/mini_www/html/js/bar.js 
 *)
let extract_complete_name_of_info ast =

  let in_class = ref (None: string option) in

  (* we want to differentiate static from non static methods *)
  let in_statics = ref false in
  let in_members = ref false in

  let (h: (Ast_js.tok, Database_code.entity_kind * string) Hashtbl.t) =
    Hashtbl.create 101 in

  let v = V.mk_visitor { V.default_visitor with

    (* recognize the class *)

    V.kstmt = (fun (k, _) st ->
      match st with
      (* var Foo = { ... } *)
      | Variable(i_1, [Left { v_name = (class_name, info_class); v_type = _;
          v_init = Some((i_3, (Object(body_object))))}], scopt) ->
       (* could restrict to only toplevel first column var declarations ? *)

          Hashtbl.add h info_class 
            (Db.Class Db.RegularClass, spf "Misc.%s" class_name);
          Common.save_excursion in_class (Some class_name) (fun () ->
          (* todo? really in_members ? or in_statics ? *)
          Common.save_excursion in_members true (fun () ->
            k st
          ))
      (* var Foo = (function() { ... })() *)
      | Variable(i_1, [Left{ v_name = (class_name, info_class); v_type = _;
         v_init = Some((i_3,
               (Apply(
                  (Paren((i_4, (Function(body_func)), i_11))), 
                 (i_13, [], i_14)))))}], scopt) ->

          Hashtbl.add h info_class 
            (Db.Class Db.RegularClass, spf "Misc.%s" class_name);
          Common.save_excursion in_class (Some class_name) (fun () ->
          Common.save_excursion in_members true (fun () ->
            k st
          ))
      | _ -> k st
    );

    V.kexpr = (fun (k, _) e ->
      match e with

      (* Foo.prototype = { ... } *)
      | Assign(
          (Period((V((class_name, info_class))), i_3, ("prototype", i_4))), 
          (A_eq, i_6), 
          (Object(body_object))) ->

          Hashtbl.add h info_class 
            (Db.Class Db.RegularClass, spf "Prototype.%s" class_name);
          Common.save_excursion in_class (Some class_name) (fun () ->
          Common.save_excursion in_members true (fun () ->
            k e
          ))

      (* Foo = { } *)
      | Assign(
          (V((class_name, info_class))), 
          (A_eq, i_56), 
          (Object(body_object))) ->
       (* could restrict to only toplevel first column var declarations ? *)

          Hashtbl.add h info_class 
            (Db.Class Db.RegularClass, spf "Misc.%s" class_name);
          Common.save_excursion in_class (Some class_name) (fun () ->
          Common.save_excursion in_members true (fun () ->
            k e
          ))

      (*---------------------------------------------------------------------*)
      (* Javelin *)
      (*---------------------------------------------------------------------*)

      (* JX.install('Foo', { ... } ) *)
      | Apply(
          (Period((V(("JX", i_1))), i_3, ("install", i_4))),
          (i_6,
          (Left((L(String((class_name, info_class)))))::Right(i_9)::rest_args),
          i_13)) ->

          Hashtbl.add h info_class 
            (Db.Class Db.RegularClass, spf "JX.%s" class_name);

          (* was just %s before, but then get conflict between for instance
           * DOM.setContent and JX.DOM.setContent
           *)
          Common.save_excursion in_class (Some (spf "JX.%s" class_name))(fun()->
            k e
          )
      (* JX.copy(Foo.prototype, { ... }) *)
      | Apply(
          (Period((V(("JX", i_1))), i_3, ("copy", i_4))),
          (i_6,
           [Left((Period((V((class_name, info_class))),i_9,
                        ("prototype", i_10)))); 
            Right(i_12); 
            Left((Object(object_body)))], i_16)) ->

          Hashtbl.add h info_class 
            (Db.Class Db.RegularClass, spf "JX.%s" class_name);
          Common.save_excursion in_class (Some class_name) (fun () ->
          Common.save_excursion in_statics true (fun () ->
              k e
          ))

      (* Foo.mixin('Arbitrer', { ... }) *)
      | Apply(
         (Period((V((class_name, info_class))), i_3, ("mixin", i_4))),
         (i_6,
          [Left((L(String((mixin_name, info_mixin))))); Right(i_9);
           Left((Object(body_object)))], i_13)) ->

          Hashtbl.add h info_class 
            (Db.Class Db.RegularClass, spf "%s<%s" class_name mixin_name);
          Common.save_excursion in_class (Some class_name) (fun () ->
          Common.save_excursion in_statics true (fun () ->
              k e
          ))

      (* FB.subclass('FB.ApiClient', 'FB.Class', { }); *)
      | Apply(
         (Period((V(("FB", i_1))), i_3, ("subclass", i_4))),
         (i_6,
          [Left((L(String((class_name, info_class))))); Right(i_9);
           Left((L(String((parent_class, i_10))))); Right(i_12);
           Left((Object(body_object)))], i_16)) ->

          Hashtbl.add h info_class
            (Db.Class Db.RegularClass, spf "%s" class_name);
          Common.save_excursion in_class (Some class_name) (fun () ->
          Common.save_excursion in_statics true (fun () ->
            k e
          ))

      (* FB.provide('FBIntern.Cookie', { } ); *)
      | Apply(
         (Period((V(("FB", i_19))), i_21, ("provide", i_22))),
         (i_24,
          [Left((L(String((class_name, info_class))))); Right(i_27);
           Left((Object(body_object)))], i_31)) ->

          Hashtbl.add h info_class
            (Db.Class Db.RegularClass, spf "%s" class_name);

          Common.save_excursion in_class (Some class_name) (fun () ->
          Common.save_excursion in_statics true (fun () ->
            k e
          ))
         
      (*---------------------------------------------------------------------*)
      (* copy_properties *)
      (*---------------------------------------------------------------------*)

      (* copy_properties(Foo, { ... } ) *)
      | Apply(
          (V(("copy_properties", i_16))),
          (i_18,
          [Left((V((class_name, info_class)))); Right(i_21);
           Left((Object(body_object)))], i_25)) ->

          Hashtbl.add h info_class 
            (Db.Class Db.RegularClass, spf "Copy_properties.%s" class_name);
          Common.save_excursion in_class (Some class_name) (fun () ->
          Common.save_excursion in_statics true (fun () ->
            k e
          ))

      (* copy_properties(Foo.prototype, { ... } ) *)
      | Apply(
          (V(("copy_properties", info_class))),
          (i_3,
          [Left((Period((V((class_name, i_4))), i_6, ("prototype", i_7)))); 
           Right(i_9); 
           Left((Object(body_object)))], i_13)) ->

          Hashtbl.add h info_class 
            (Db.Class Db.RegularClass, spf "Copy_properties.%s" class_name);
          Common.save_excursion in_class (Some class_name) (fun () ->
          Common.save_excursion in_statics true (fun () ->
            k e
          ))

      | _ -> k e
    );


    (* recognize the methods *)

    V.kfield = (fun (k, _) e ->
      match e with

      (* Javelin specifics ? *)
      | (PN_String(("statics", i_20)), i_21, body) ->
          Common.save_excursion in_statics true (fun () -> k e)
      | (PN_String(("members", i_20)), i_21, body) ->
          Common.save_excursion in_members true (fun () -> k e)

      (* fld: function (...) { ... } *)
      | (PN_String((method_name, info_method_name)), i_40, (Function(_))) ->

          let fullname = 
          (match !in_class, !in_statics, !in_members with
          | Some classname, true, false ->
              spf "%s::%s" classname method_name
          | Some classname, false, true ->
              spf "%s->%s" classname method_name
          | Some classname, true, true ->
              pr2 ("WEIRD: both statics and members at " ^
                      Parse_info.string_of_info info_method_name);
              ""
          | Some classname, false, false ->
              (match method_name with
              | "initialize" | "construct" ->
                  spf "%s::%s" classname method_name
              | _ ->
                  pr2 ("WEIRD: in class but no statics or members at " ^
                          Parse_info.string_of_info info_method_name);
                  ""
              )
          | None, true, _ 
          | None, _, true 
              ->
              pr2 ("WEIRD: no class but statics or members at " ^
                      Parse_info.string_of_info info_method_name);
              ""
          | None, _, _ ->
              (* jsspec use strings for method names *)
              let raw_str = Parse_info.str_of_info info_method_name in
              if  raw_str =~ "^[']"
              then ()
              else begin
                (* TODO too many FPs
                pr2 ("WEIRD: no class but function field at " ^
                        Ast.string_of_info info_method_name);
                *)
              end;
              ""
          )
          in
          Hashtbl.add h info_method_name 
            (Db.Method 
                (if !in_statics then Db.StaticMethod else Db.RegularMethod), 
            fullname)


      | _ -> k e
    );
  }
  in
  v (Program ast);
  h
