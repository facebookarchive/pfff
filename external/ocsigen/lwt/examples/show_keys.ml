
(* Show code of key pressed *)

open Lwt_unix
open Lwt
open Lwt_text

let rec loop () =
  lwt raw_key = Lwt_term.parse_key_raw Lwt_term.standard_input in
  let key = Lwt_term.decode_key raw_key in
  lwt () = printlf "raw_key = %S, key = %s" raw_key (Lwt_term.string_of_key key) in
  if key = Lwt_term.key_escape then
    return ()
  else
    loop ()

let _ =
  Lwt_main.run
    (printl "Press escape to exit." >> Lwt_term.with_raw_mode loop)
