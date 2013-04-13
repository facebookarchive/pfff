open Eliom_pervasives

module H = Eliom_content.Html5.D

(*****************************************************************************)
(* Result service *)
(*****************************************************************************)

let result_service = Eliom_registration.Html5.register_service 
  ~path:["dumper_result"]
  ~get_params:(Eliom_parameter.string "content")
  (fun (content) () ->
    let s =
      Common2.with_tmp_file ~str:content ~ext:".php" (fun file ->
        let ast = Parse_php.parse_program file in
        Export_ast_php.ml_pattern_string_of_program ast
      )
    in
    Lwt.return (H.html (H.head (H.title (H.pcdata "Demo"))[]) (H.body [
      H.h1 [H.pcdata "Dumper result"];
      H.pre [H.pcdata s];
    ]
    )))

(*****************************************************************************)
(* main entry point *)
(*****************************************************************************)

let main_service = Eliom_registration.Html5.register_service 
  ~path:["dumper"]
  ~get_params:(Eliom_parameter.unit)
  (fun () () ->
    Lwt.return (H.html (H.head (H.title (H.pcdata "Demo")) []) (H.body [

      H.h1 [H.pcdata "Welcome to the Dumper"];
      H.get_form ~service:result_service
        (fun (name) ->
          [H.p [
            H.pcdata "File content: ";
            H.br ();
            H.textarea ~name ~a:[H.a_rows  10; H.a_cols 80] ~value:"<?php\n" ();
            H.br ();
            H.string_input ~input_type:`Submit ~value:"Go" ()
            ]
          ]);
    ]
    )))
