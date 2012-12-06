open Common

(*
let dump_cmt_headers file =
  let info = Cmt_format.read_cmt file in
  pr2_gen info.Cmt_format.cmt_modname;
  pr2_gen info.Cmt_format.cmt_imports;
  ()
*)

let dump_cmt file =
  let info = Cmt_format.read_cmt file in
  match info.Cmt_format.cmt_annots with
  | Cmt_format.Implementation x ->
      let v = Meta_ast_cmt.vof_structure x in
      let str = Ocaml.string_of_v v in
      pr str
  | _ -> raise Todo

let actions () = [
  "-dump_cmt", " <file>",
  Common.mk_action_1_arg dump_cmt;
]
