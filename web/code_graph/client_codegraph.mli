
val paint: 
  Model_codegraph.world_client -> 
  (string, unit) Eliom_pervasives.server_function ->
  (unit, string) Eliom_pervasives.server_function ->

  (int * int, string) Eliom_pervasives.server_function ->

  (string * (string * string), unit, [< Eliom_service.service_kind ],
            [< Eliom_service.suff ], 'c, 'd, [< Eliom_service.registrable ],
            [< Eliom_registration.non_ocaml_service ]) Eliom_service.service->

  unit Lwt.t
