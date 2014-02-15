open Ast_html

module Ast = Ast_html
module PI = Parse_info
module M = Meta_ast_generic

let _current_precision = ref M.default_precision

let rec vof_info v = 
  Parse_info.vof_info v

and vof_wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = vof_info v2 in Ocaml.VTuple [ v1; v2 ]
  
let rec vof_html_tree =
  function
  | Element ((v1, v2, v3)) ->
      let v1 = vof_tag v1
      and v2 =
        Ocaml.vof_list
          (fun (v1, v2) ->
             let v1 = vof_attr_name v1
             and v2 = vof_attr_value v2
             in Ocaml.VTuple [ v1; v2 ])
          v2
      and v3 = Ocaml.vof_list vof_html_tree v3
      in Ocaml.VSum (("Element", [ v1; v2; v3 ]))
  | Data v1 ->
      let v1 = vof_wrap Ocaml.vof_string v1 in Ocaml.VSum (("Data", [ v1 ]))
and vof_tag =
  function
  | Tag v1 ->
      let v1 = vof_wrap Ocaml.vof_string v1 in Ocaml.VSum (("Tag", [ v1 ]))
and vof_attr_name =
  function
  | Attr v1 ->
      let v1 = vof_wrap Ocaml.vof_string v1 in Ocaml.VSum (("Attr", [ v1 ]))
and vof_attr_value =
  function
  | Val v1 ->
      let v1 = vof_wrap Ocaml.vof_string v1 in Ocaml.VSum (("Val", [ v1 ]))
