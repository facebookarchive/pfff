
(* from the eliom tutorial *)
let coucou =
  Eliom_output.Xhtml5.register_service
    ~path:["coucou"]
    ~get_params:Eliom_parameters.unit
    (fun () () ->
      Lwt.return
        (XHTML5.M.html
          (XHTML5.M.head (XHTML5.M.title (XHTML5.M.pcdata "")) [])
          (XHTML5.M.body [XHTML5.M.h1 [XHTML5.M.pcdata "Hallo!"]])))

(* with some open and a counter *)
open XHTML5.M
open Eliom_parameters

let count =
  let next =
    let c = ref 0 in
    (fun () -> c := !c + 1; !c)
  in
  Eliom_output.Xhtml5.register_service
    ~path:["count"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        (html
         (head (title (pcdata "counter")) [])
         (body [p [pcdata (string_of_int (next ()))]])))

