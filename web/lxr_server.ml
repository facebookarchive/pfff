open Common

(* still needed? *)
open Eliom_pervasives

module Db = Database_php
module HC = Highlight_code

module H = HTML5.M

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * The goal of this module is to provide a code browser a la LXR.
 * See  http://lxr.linux.no/#linux+v2.6.37.1/mm/compaction.c as an
 * example.
 * 
 * It's also an exercise in learning ocsigen. A code browser does
 * not require anything fancy like Depot. No need for a ORM,
 * or forms. Just need to htmlize a source file and add
 * hrefs into it to make it hypertextable.
 * 
 * todo: add search, add nice html, add fast html
 * 
 * alternatives:
 * - http://en.wikipedia.org/wiki/LXR_Cross_Referencer
 * - http://en.wikipedia.org/wiki/OpenGrok
 * - https://wiki.mozilla.org/DXR
 * 
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let htmlize_dir ~link dir db =
  let subdirs = Common.readdir_to_dir_list dir +> Common.sort in
  let files = Common.readdir_to_file_list dir +> Common.sort in
  let files = files +> Common.exclude (fun file ->
    let (d,b,e) = Common.dbe_of_filename_noext_ok file in
    e = "git_annot" || e = "cm_cache"
  )
  in

  let elements = subdirs ++ files in
  (H.html (*~a:[H.a_xmlns `W3_org_1999_xhtml; H.a_xml_lang "en"]*)
    (H.head
        (H.title (H.pcdata "XHTML"))
        [
          H.style [H.pcdata Htmlize_php2.style ]
        ])
    (H.body
        ((H.h1 [H.pcdata dir] )
          ::
          (elements +> List.map (fun subelement ->
            let fullpath = Filename.concat dir subelement in
            let readable = Db.absolute_to_readable_filename fullpath db in
            [(*H.h3 [H.pcdata subelement]; *)
             H.h1 [
               Eliom_output.Html5.a link [H.pcdata readable] readable;
             ];
             (* H.pre [H.pcdata readable]; *)
            ]
          ) +> List.flatten)
        )
    )
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main_service = 
  Eliom_services.service ["lxr"] (Eliom_parameters.string "path") ()

let _ = Eliom_output.Html5.register main_service
  (fun readable_path () ->
    (* todo? sanitized path ? *)
    let path = Db.readable_to_absolute_filename readable_path Global_db.db in

    let hook_token s tok categ =
      match categ with
      | Some (HC.Function (HC.Use2 _)) ->

          (try 
              let id = Db.id_of_function s Global_db.db in
              let file = Db.readable_filename_of_id id Global_db.db in
              Eliom_output.Html5.a main_service [H.pcdata s] file
            with (Not_found | Multi_found) as exn ->
              Eliom_output.Html5.a main_service [H.pcdata s] 
                (Common.exn_to_s exn)
            )
      | _ -> H.pcdata s
    in
    let html = 
      if Common.is_directory path
      then htmlize_dir ~link:main_service path Global_db.db
      else Htmlize_php2.htmlize_with_headers ~hook_token path Global_db.db
    in
    Lwt.return html
  )
