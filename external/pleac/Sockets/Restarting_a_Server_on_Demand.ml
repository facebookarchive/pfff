(* ********************************************************************** *)
(* Restarting a Server on Demand *)
(* ********************************************************************** *)
let pleac_Restarting_a_Server_on_Demand () = 
  #load "unix.cma";;
  
  let self = "/usr/bin/ocaml"
  let args = self :: Array.to_list Sys.argv
  
  let phoenix _ =
    (* close all your connections, kill your children, and *)
    (* generally prepare to be reincarnated with dignity. *)
    try
      ignore (Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sighup]);
      Unix.execv self (Array.of_list args)
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "Couldn't restart: %s\n%!"
        (Unix.error_message e)
  
  let () =
    Sys.set_signal Sys.sighup (Sys.Signal_handle phoenix)
  
  (*-----------------------------*)
  
  (* This recipe uses the Ocaml-Syck YAML parser available at:
     http://ocaml-syck.sourceforge.net/ *)
  
  #directory "+yaml";;
  #load "yaml.cma";;
  #load "unix.cma";;
  
  let yaml_parser = YamlParser.make ()
  
  let config_file = "/usr/local/etc/myprog/server_conf.yaml"
  let config = ref (YamlNode.SCALAR ("", ""))
  
  let read_config _ =
    let in_channel = open_in config_file in
    let lines = ref [] in
    try
      while true do
        let line = input_line in_channel in
        lines := line :: !lines
      done
    with End_of_file ->
      close_in in_channel;
      config :=
        YamlParser.parse_string yaml_parser
          (String.concat "\n" (List.rev !lines))
  
  let () =
    read_config ();
    Sys.set_signal Sys.sighup (Sys.Signal_handle read_config)
  

