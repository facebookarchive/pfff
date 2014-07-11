
val mouseclick:
  Canvas_helpers.context ->
  Model_codemap.world_client ->

  (string * (string * string), unit, [< Eliom_service.service_kind ],
            [< Eliom_service.suff ], 'c, 'd, [< Eliom_service.registrable ],
            [< Eliom_registration.non_ocaml_service ]) Eliom_service.service->

  (Dom_html.element Js.t -> Dom_html.mouseEvent Js.t -> unit Lwt.t)
