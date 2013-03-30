open Common

module Db = Database_php

let init () = 
  Common_extra.set_link();
  Database_php_storage.set_link();
  ()

let _ = init()
(* todo: ugly, would be better to have access to this information from
 * the ocsigen command line.
 * Also ugly that have to put that globally so that the
 * endpoint can access it :(
 *)
let db = !(Db._current_open_db_backend) "/tmp/pfff_db"

let _ =
  Sys.set_signal Sys.sigint (Sys.Signal_handle   (fun _ -> 
    pr2 "C-c intercepted, will do some cleaning before exiting";
    Db.close_db db
  ))
