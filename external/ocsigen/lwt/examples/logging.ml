
(* The logging section for this module: *)
let section = Lwt_log.Section.make "test"

lwt () =
  (* Enable all logging levels superior from [Info] to [Fatal]: *)
  Lwt_log.Section.set_level section Lwt_log.Info;

  (* A message with the default logger: *)
  lwt () = Lwt_log.log ~section ~level:Lwt_log.Info "this message will appear only on stderr" in

  (* Same as begore, but using [Lwt_log.info]: *)
  lwt () = Lwt_log.info ~section "this one too" in

  (* A message to a custom logger, logging simultaneously to [stderr]
     and to the system logger daemon: *)
  let logger =
    Lwt_log.broadcast
      [Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr ();
       Lwt_log.syslog ~facility:`User ()]
  in
  lwt () = Lwt_log.info ~section ~logger "this message will appear on stderr and in '/var/log/user.log'" in

  (* Logging of exceptions: *)
  Printexc.record_backtrace true;
  let f () : unit = raise Exit in
  let g () = f () in
  let h () = g () in
  try
    h ();
    Lwt.return ()
  with exn ->
    Lwt_log.error ~section ~exn "h failed with"
