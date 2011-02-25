open Common

module Db = Database_php
module H = XHTML.M

module HC = Highlight_code

let init () = 
  Common_extra.set_link();
  Database_php_storage.set_link();
  ()

let _ = init()
(* todo: ugly, would be better to have access to this information from
 * the ocsigen command line.
 * Also ugly that have to put that globally so that the
 * endpoint can access it :(
 *)
let db = !(Db._current_open_db_backend) "/tmp/pfff_db"

let _ =
  Sys.set_signal Sys.sigint (Sys.Signal_handle   (fun _ -> 
    pr2 "C-c intercepted, will do some cleaning before exiting";
    Db.close_db db
  ))

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
        [H.style ~contenttype:"text/css" [H.pcdata Htmlize_php2.style ]])
    (H.body
        ((H.h1 [H.pcdata dir] )
          ::
          (elements +> List.map (fun subelement ->
            let fullpath = Filename.concat dir subelement in
            let readable = Db.absolute_to_readable_filename fullpath db in
            [(*H.h3 [H.pcdata subelement]; *)
             H.h1 [
               Eliom_output.Xhtml.a link [H.pcdata readable] readable;
             ];
             (* H.pre [H.pcdata readable]; *)
            ]
          ) +> List.flatten)
        )
    )
  )


let lxr = Eliom_services.service [""] (Eliom_parameters.string "path") ()

(* from the eliom tutorial *)
let _ = Eliom_output.Xhtml.register lxr
    (fun readable_path () ->
      (* todo? sanitized path ? *)

      let path = Db.readable_to_absolute_filename readable_path db in
      let hook_token s tok categ =
        match categ with
        | Some (HC.Function (HC.Use2 _)) ->

            (try 
              let id = Db.id_of_function s db in
              let file = Db.readable_filename_of_id id db in
              Eliom_output.Xhtml.a lxr [H.pcdata s] file
            with (Not_found | Multi_found) as exn ->
              Eliom_output.Xhtml.a lxr [H.pcdata s] (Common.exn_to_s exn)
            )
        | _ -> H.pcdata s
      in
      let html = 
        if Common.is_directory path
        then htmlize_dir ~link:lxr path db
        else Htmlize_php2.htmlize_with_headers ~hook_token path db 
      in
      Lwt.return html
    )
