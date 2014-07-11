open Common

module H = Eliom_content.Html5.D

module DM = Dependencies_matrix_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* App *)
(*****************************************************************************)
module App = Eliom_registration.App (struct let application_name = "app" end)

(*****************************************************************************)
(* Logging *)
(*****************************************************************************)
let log str = 
  Lwt_io.write_line Lwt_io.stdout str
let rpc_log = 
  Eliom_pervasives.server_function Json.t<string> log

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main_service =
  Eliom_service.Http.service 
    ~path:["codegraph"]
    ~get_params:Eliom_parameter.
      (string "size" ** string "project" ** string "path") ()

let _ =
  App.register ~service:main_service 
  (fun (size, (project, path)) () ->
    let path = Str.split (Str.regexp "/") path in
    pr2_gen path;
    let (width, height) = Globals.dimensions_of_size size in

    let gopti_ref = Globals.gopti_of_project project in
    let m = Server_codegraph.build gopti_ref path in

    let test () =
      Lwt.return "42" in
    let rpc_test = Eliom_pervasives.server_function Json.t<unit> test in

    let explain_cell (i, j) =
      let deps = 
        DM.explain_cell_list_use_edges  (i, j) m !gopti_ref in
      let str = 
        deps +> Common.take_safe 50 +> List.map (fun (n1, n2) ->
          spf "            %s --> %s" 
            (Graph_code.string_of_node n1)  
            (Graph_code.string_of_node n2)
        ) +> Common.join "\n"
      in
      (*Lwt.return (spf "%d %d" i j)*)
      Lwt.return str
    in
    let rpc_explain_cell =
      Eliom_pervasives.server_function Json.t<int * int> explain_cell in

    let w = { Model_codegraph.
       project; path; size;
       m;
       (* too big and apparently pb with Hashtbl.find :( *)
       (* gopti = Globals.gopti; *)

       width = width;
       height = height;
       interactive_regions = [];
    } in
    (*
    pr2_gen 
      (Hashtbl.find w.Model.gopti.Graph_code_opti.name_to_i 
          ("external", Database_code.Dir));
    *)

    ignore
      {unit { 
        Lwt.async (fun() -> Client_codegraph.paint %w 
            %rpc_log %rpc_test %rpc_explain_cell
            %main_service
        )
      }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "CodeGraph")) [ 
          ])
          (H.body [
            H.div 
              ~a:[H.a_id "output";] [];
            H.canvas
              ~a:[H.a_id "main_canvas"; 
                  H.a_width width; 
                  H.a_height height] [];
          ]))
  )
