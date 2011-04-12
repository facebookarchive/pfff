module H = XHTML5.M

module App = Eliom_output.Eliom_appl (struct
  let application_name = "client"
  let params = Eliom_output.default_appl_params
end)

(*****************************************************************************)
(* main entry points *)
(*****************************************************************************)

let result_service = App.register_service ~path:["dumper_result"]
  ~get_params:(Eliom_parameters.string "content")
  (fun (content) () ->
    let s =
      Common.with_tmp_file ~str:content ~ext:".php" (fun file ->
        let ast = Parse_php.parse_program file in
        Export_ast_php.ml_pattern_string_of_program ast
      )
    in
    Lwt.return [
      H.h1 [H.pcdata "Dumper result"];
      H.pre [H.pcdata s];
    ]
  )

let main_service = App.register_service ~path:["dumper"]
  ~get_params:(Eliom_parameters.unit)
  (fun () () ->
    Lwt.return [
      H.h1 [H.pcdata "Welcome to the Dumper"];
      App.get_form ~service:result_service
        (fun (name) ->
          [H.p [
            H.pcdata "File content: ";
            H.br ();
            App.textarea ~name ~rows:10 ~cols:80 ~value:"<?php\n" ();
            H.br ();
            App.string_input ~input_type:`Submit ~value:"Go" ()
          ]]);
    ])
