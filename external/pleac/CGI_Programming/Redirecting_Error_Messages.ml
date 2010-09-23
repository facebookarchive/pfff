(* ********************************************************************** *)
(* Redirecting Error Messages *)
(* ********************************************************************** *)
let pleac_Redirecting_Error_Messages () = 
  (* The default Netcgi configuration sends all exceptions to the browser
     in nicely formatted error pages. This is helpful during development
     but may be inappropriate for production. The exception pages can be
     disabled by setting the "default_exn_handler" configuration field: *)
  let config = {Netcgi.default_config with
                  Netcgi.default_exn_handler=false}
  
  (* Most web servers send standard error to the error log, which is
     typically /var/log/apache2/error.log for a default Apache 2
     configuration. You can define a "warn" function to include the
     script name in warning messages: *)
  let warn = Printf.eprintf "%s: %s\n" (Filename.basename Sys.argv.(0))
  let () =
    warn "This goes to the error log."
  
  (* You can also use Printf.kprintf to define a fancier warning function
     that supports Printf formatting. *)
  let warn =
    Printf.kprintf
      (Printf.eprintf "%s: %s\n" (Filename.basename Sys.argv.(0)))
  let () =
    warn "So does %s." "this"
  

