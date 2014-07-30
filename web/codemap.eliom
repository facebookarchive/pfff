open Common

module H = Eliom_content.Html5.D

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* App *)
(*****************************************************************************)
module App = Eliom_registration.App (struct let application_name = "app" end)

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)

let size_data x = 
  let s = Marshal.to_string x [] in
  String.length s

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main_service =
  Eliom_service.Http.service 
    ~path:["codemap"] 
    ~get_params:Eliom_parameter.
      (string "size" ** string "project" ** string "path") ()

let _ =
  App.register ~service:main_service
  (fun (size, (project, path)) () ->

    let (width, height) = Globals.dimensions_of_size size in

    let rects = Globals.rects_of_project_and_path (project, path) in
    let root = Globals.root_of_project project in

    pr2 (spf "obj size before = %d" (size_data rects));
    let rects = Server_codemap.optimize_rects root rects in
    pr2 (spf "obj size after = %d" (size_data rects));

    let w = { Model_codemap.
       project; path; size;
       rects; 
       root;
       width;
       height;
    }
    in

    ignore
      {unit { Client_codemap.paint %w %main_service }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "Codemap")) [ 
          ])
          (H.body [
            H.div ~a:[H.a_id "output";] [];
            H.canvas 
              ~a:[H.a_id "main_canvas"; H.a_width width; H.a_height height] [];
          ]))
  )

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

let test_codemap_micro =
  App.register_service 
    ~path:["test_micro"] 
    ~get_params:(Eliom_parameter.unit)
  (fun () () ->

    let size = "small" in
    let (width, height) = Globals.dimensions_of_size size in
    let rects = [] in
    let w = { Model_codemap.
       rects;
       project = "whatever";
       path = "";
       width = width;
       height = height;
       root = "/";
       size;
    }
    in
    let file = 
      (* "/home/pad/pfff/Makefile" *)
      (*"/home/pad/pfff/facebook/tests/mini_www/flib/cmf/check_module/test.php"*)
      "/home/pad/pfff/main.ml"
    in
    let fileinfo = Server_codemap.fileinfo_of_file file in

    ignore
      {unit{ Client_codemap.test_paint_micro %w %fileinfo }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "Micro")) [ 
          ])
          (H.body [
            H.div 
              ~a:[H.a_id "output";] [];
            H.canvas 
              ~a:[H.a_id "main_canvas"; H.a_width width; H.a_height height] [];

          ]))
  )
