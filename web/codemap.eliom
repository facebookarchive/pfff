open Common
module H = Eliom_content.Html5.D

module Flag = Flag_web

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* App *)
(*****************************************************************************)
(* Without this, I get some "sitedata not defined" js error.
 * Maybe some headers sent back by eliom apps contain
 * some important information.
 *)
module App = Eliom_registration.App (struct
    let application_name = "app"
end)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main_service =
  App.register_service 
    ~path:["codemap"] 
    ~get_params:(Eliom_parameter.string "path")
  (fun path () ->
    pr2 path;
    Treemap.follow_symlinks := true;
    let path = Common2.relative_to_absolute path in
    let rects = 
         Server_codemap.treemap_generator [path] 
     
     (* Common.cache_computation ~verbose:true path "_treemap.marshall" (fun () ->
       ) *)
    in
    (* optimize and filter very small rectangles so
     * that we send less data
     *)
    let rects = rects +> Common.exclude (fun rect ->
      let r = rect.Treemap.tr_rect in
      let w = Figures.rect_width r in
      let h = Figures.rect_height r in
      w *. h <= 0.000005
    )
    in
    pr2 (spf "nb rects after filtering: %d" (List.length rects));

    ignore
      {unit { Client_codemap.draw_treemap_rendering %rects }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "Codemap")) [ 

         (* this is now included by default
         H.js_script
           ~uri:(H.make_uri  (Eliom_service.static_dir ())["app.js"]) ();
         *)
          ])
	  (H.body [
          ]))
  )

let test_codemap_micro =
  App.register_service 
    ~path:["micro"] 
    ~get_params:(Eliom_parameter.string "path")
  (fun path () ->
    pr2 path;
    let path = Common2.relative_to_absolute path in
    let lines = Common.cat path in

    ignore
      {unit{ Client_codemap.draw_file %lines }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "Micro")) [ 
          ])
	  (H.body [
          ]))
  )
