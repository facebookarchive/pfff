(* This file is written by netsys/configure *)

let have_printexc_register_printer = true

let printexc_register_printer f =
  Printexc.register_printer f

