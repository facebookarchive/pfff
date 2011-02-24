(* $Id: netsys_gprof_init.ml 1401 2010-02-03 23:25:36Z gerd $ *)

external netsys_moncontrol : bool -> unit = "netsys_moncontrol"

let () =
  Netsys.set_moncontrol netsys_moncontrol

let init() =
  ()
