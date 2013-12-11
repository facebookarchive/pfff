open Eliom_pervasives

open Common

module J = Json_type

(*****************************************************************************)
(* main entry point *)
(*****************************************************************************)

let main_service = Eliom_registration.String.register_service 
  ~path:["blueprint"]
  ~get_params:(Eliom_parameter.string "class")
  (fun (classname) () ->

    let json = 
      J.Object [
        "nodes", J.Array [
          J.Object [
            "name", J.String classname;
            "width", J.Int 2;
            "height", J.Int 3;
            "layer", J.Int 0;
            "tooltip", J.String "class B extends A";
          ];
        ];
        "links", J.Array [
          J.Array [
            J.String classname; J.String classname
          ]
        ]
      ]
    in
    let str = Json_out.string_of_json json in
    Lwt.return (str, "json")
  )
