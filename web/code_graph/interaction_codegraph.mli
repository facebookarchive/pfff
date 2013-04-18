
val mouseclick:
  Canvas_helpers.context ->
  Model_codegraph.world_client ->

  (int * int, string) Eliom_pervasives.server_function ->

  (string * string, unit, [< Eliom_service.service_kind ],
            [< Eliom_service.suff ], 'c, 'd, [< Eliom_service.registrable ],
            [< Eliom_registration.non_caml_service ]) Eliom_service.service->

  (Dom_html.mouseEvent Js.t -> unit Lwt.t)
