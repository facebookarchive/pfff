open Common

let dump_cmt file =
  let info = Cmt_format.read_cmt file in
  pr2_gen info.Cmt_format.cmt_modname;
  pr2_gen info.Cmt_format.cmt_imports;
  ()

let actions () = [
  "-dump_cmt", " <file>",
  Common.mk_action_1_arg dump_cmt;
]
