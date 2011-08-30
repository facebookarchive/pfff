open Common
module H = HTML5.M

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
module App = Eliom_output.Eliom_appl (struct
    let application_name = "app"
end)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main_service =
  App.register_service 
    ~path:["codemap"] 
    ~get_params:(Eliom_parameters.string "path")
  (fun path () ->
    pr2 path;
    Treemap.follow_symlinks := true;
    let path = Common.relative_to_absolute path in
    let rects = 
      Common.cache_computation ~verbose:true path "_treemap.marshall" (fun () ->
        Server.treemap_generator [path] 
      )
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

    Eliom_services.onload
      {{ Client.draw_treemap_rendering %rects }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "Codemap")) [ 
            H.unique (H.script ~a:[H.a_src (H.uri_of_string "app.js")]
                         (H.pcdata ""))
          ])
	  (H.body [
          ]))
  )

let test_codemap_micro =
  App.register_service 
    ~path:["micro"] 
    ~get_params:(Eliom_parameters.string "path")
  (fun path () ->
    pr2 path;
    let path = Common.relative_to_absolute path in
    let lines = Common.cat path in

    Eliom_services.onload
      {{ Client.draw_file %lines }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "Micro")) [ 
            H.unique (H.script ~a:[H.a_src (H.uri_of_string "app.js")] 
                         (H.pcdata ""))
          ])
	  (H.body [
          ]))
  )
