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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Should I put this file in parsing_php/ ? On one hand it is some information
 * that are in comment and so that could be considered as parsing related.
 * But adding this info on the AST then requires to load the AST to 
 * access the information. Like for function types, it is useful
 * to have faster acess to high level information about functions.
 * Hence annotations/tags are stored in the database in the extra_id 
 * field. 
 * 
 * Moreover we may want to have as "annotations" things
 * which are not really in comment, like the THIS_FUNCTION_EXPIRES
 * macro call in the body of the function, or has_call_to_func_num_args
 * etc.
 * 
 * Most of this file is quite facebook specific, but other projects
 * may find it useful. 
 * 
 * less: should I move it inside facebook/ ?
 * 
 * less: could also extra the message associated with the annotation, such
 * as the reason for the @not-dead-code
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type email = string
 (* with tarzan *)
let is_email s = 
  s =~ ".*@.*"

type unixname = string
 (* with tarzan *)

type annotation = 
  | Owner of unixname
  | Emails of (email * notification_kind option) list
  | Status of string (* inactive usually *)

  (* deprecated *)
  | Author of string

  (* see http://www.intern.facebook.com/intern/wiki/index.php/Dead_code_reaper 
   * coupling: Deadcode_php.default_hooks contains a list that you should
   * update if you add new entries here. C-s for CalledFromPhpsh in 
   * deadcode_php.ml
  *)
  | CalledFromPhpsh
  | CalledOutsideTfb
  | CalledDynamically
  | NotDeadCode
  | Have_THIS_FUNCTION_EXPIRES_ON

  | Other of string

 (* see http://www.intern.facebook.com/intern/wiki/index.php/UnitTests/PHP#Controlling_Failure_Notification *)
 and notification_kind = 
   | Immediate
   | Consistent
   | Daily
 (* with tarzan *)

(*****************************************************************************)
(* string -> annotation *)
(*****************************************************************************)

(* str is usually a comment *)
let extract_annotations str =
  let lines = Common.lines str in 

  lines +> Common.map_flatten (fun str ->

    let str = Comment_php.strip_comment_marks str in

    match () with

    (*  
     * We allow both space and ',' as separator. That is both 
     * 
     *    @emails xxx@foo.com yyy@lists.bar.com
     *    @emails xxx@foo.com, yyy@lists.bar.com
     * 
     * are valid. Moreover each email can have its own notification.
     *)

    | _ when str =~ "@emails[ ]+\\(.*\\)$" ->

        let s = Common.matched1 str in
        let xs = Common.split "[ ,]+" s in

        let emails = xs +> List.map (fun s ->
          
          if s =~ "^\\([^:]+\\):?\\([a-z]*\\)$" 
          then 
            let (email, annot) = Common.matched2 s in

            let notification = 
              match annot with
              | "" -> None
              | "immediate" -> Some Immediate
              | "consistent" -> Some Consistent
              | "daily" -> Some Daily

              (* 
               * sgrimm uses 'trash' in his own tests on the 
               * unittest infrastructure, so remove this false positive
               * see flib/intern/unittest/__tests__/util.php:
               *   @emails bad-addr@nowhere.xx:trash
               * 
               *)
              | "trash" -> None
                  
              | s ->
                  failwith ("wrong notification: " ^ s)
            in

            if (not (is_email email)) 
            then failwith ("not a valid email address: " ^ s);
            
            (email, notification)
          else failwith ("not a valid email address: " ^ s)
        ) in

        [Emails emails]

    | _ when str =~ "@owner[ ]+\\([a-z]+\\)" ->
        let name = Common.matched1 str in
        [Owner name]
    | _ when str =~ "@status[ ]+\\([a-z]+\\)" ->
        let name = Common.matched1 str in
        [Status name]

    | _ when str =~ "@author[ ]+\\([^ ]+\\)" ->
        let name = Common.matched1 str in
        [Author name]
       
    | _ -> 
        let xs = Common.all_match "\\(@[A-Za-z-]+\\)" str in
        xs +> List.map (function
        | "@called-from-phpsh" -> CalledFromPhpsh
        | "@called-outside-tfb" -> CalledOutsideTfb
        | "@called-dynamically" -> CalledDynamically
        | "@not-dead-code" -> NotDeadCode
        | s -> Other s
        )
  )

let _ = example 
  (extract_annotations "@emails foo@bar:immediate, bar-list@foo\n@owner pad" = 
      [Emails [("foo@bar",      Some Immediate);
               ("bar-list@foo", None)];
       Owner "pad";
      ]
  )

let _ = example 
  (extract_annotations "// @emails foo@bar" = 
      [Emails [("foo@bar",      None);]]
  )
let _ = example 
  (extract_annotations "// @emails    foo@bar" = 
      [Emails [("foo@bar",      None);]]
  )

(*****************************************************************************)
(* annotation -> string *)
(*****************************************************************************)


(*****************************************************************************)
(* Reflection *)
(*****************************************************************************)

(* auto generated by './ocamltarzan -choice vof ../analyze_php/annotation_php.ml' *)
let vof_email v = Ocaml.vof_string v
  
let vof_unixname v = Ocaml.vof_string v
  
let rec vof_annotation =
  function
  | Owner v1 -> let v1 = vof_unixname v1 in Ocaml.VSum (("Owner", [ v1 ]))
  | Emails v1 ->
      let v1 =
        Ocaml.vof_list
          (fun (v1, v2) ->
             let v1 = vof_email v1
             and v2 = Ocaml.vof_option vof_notification_kind v2
             in Ocaml.VTuple [ v1; v2 ])
          v1
      in Ocaml.VSum (("Emails", [ v1 ]))
  | Author v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("Author", [ v1 ]))
  | Status v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("Status", [ v1 ]))
  | CalledFromPhpsh -> Ocaml.VSum (("CalledFromPhpsh", []))
  | CalledOutsideTfb -> Ocaml.VSum (("CalledOutsideTfb", []))
  | CalledDynamically -> Ocaml.VSum (("CalledDynamically", []))
  | NotDeadCode -> Ocaml.VSum (("NotDeadCode", []))
  | Have_THIS_FUNCTION_EXPIRES_ON ->
      Ocaml.VSum (("Have_THIS_FUNCTION_EXPIRES_ON", []))
  | Other v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("Other", [ v1 ]))
and vof_notification_kind =
  function
  | Immediate -> Ocaml.VSum (("Immediate", []))
  | Consistent -> Ocaml.VSum (("Consistent", []))
  | Daily -> Ocaml.VSum (("Daily", []))


(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let str_debug_of_annotation a = 
  let v = vof_annotation a in
  Ocaml.string_of_v v
