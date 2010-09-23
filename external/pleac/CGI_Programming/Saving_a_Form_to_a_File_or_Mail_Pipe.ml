(* ********************************************************************** *)
(* Saving a Form to a File or Mail Pipe *)
(* ********************************************************************** *)
let pleac_Saving_a_Form_to_a_File_or_Mail_Pipe () = 
  #!/usr/bin/env ocaml
  
  #use "topfind";;
  #require "netcgi2";;
  
  let escape   = Netencoding.Url.encode ~plus:false
  let unescape = Netencoding.Url.decode ~plus:false
  
  let save_arguments (ch : Netchannels.out_obj_channel) args =
    List.iter
      (fun arg ->
         ch#output_string (escape arg#name);
         ch#output_char '=';
         ch#output_string (escape arg#value);
         ch#output_char '\n')
      args;
    ch#output_string "=\n"
  
  let process (cgi : Netcgi.cgi) =
    (* first open and exclusively lock the file *)
    let ch = open_out_gen [Open_append; Open_creat] 0o666 "/tmp/formlog" in
    Unix.lockf (Unix.descr_of_out_channel ch) Unix.F_LOCK 0;
  
    (* locally set some additional arguments *)
    let arguments =
      Netcgi.Argument.set
        [
          Netcgi.Argument.simple "_timestamp"
            (string_of_float (Unix.time ()));
          Netcgi.Argument.simple "_environs"
            (String.concat "\n" (Array.to_list (Unix.environment ())));
        ]
        cgi#arguments in
  
    (* wrap output in a Netchannel and save *)
    let ch = new Netchannels.output_channel ch in
    save_arguments ch arguments;
    ch#close_out ();
  
    (* send in an email *)
    let body = Buffer.create 256 in
    let ch = new Netchannels.output_buffer body in
    save_arguments ch arguments;
    Netsendmail.sendmail
      (Netsendmail.compose
         ~from_addr:("your cgi script", Sys.argv.(0))
         ~to_addrs:[("hisname", "hisname@hishost.com")]
         ~subject:"mailed form submission"
         (Buffer.contents body))
  
  let () =
    let config = Netcgi.default_config in
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    let output_type = `Transactional buffered in
    if Unix.isatty Unix.stdin
    then Netcgi_test.run ~config ~output_type process
    else Netcgi_cgi.run ~config ~output_type process
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  
  #use "topfind";;
  #require "str";;
  #require "unix";;
  #require "netstring";;
  
  let escape   = Netencoding.Url.encode ~plus:false
  let unescape = Netencoding.Url.decode ~plus:false
  
  let parse_env data =
    let result = Hashtbl.create 16 in
    List.iter
      (fun line ->
         try
           let index = String.index line '=' in
           Hashtbl.add result
             (String.sub line 0 index)
             (String.sub line (index + 1) (String.length line - index - 1))
         with Not_found -> ())
      (Str.split (Str.regexp "\n") data);
    result
  
  let ends_with suffix s =
    try Str.last_chars s (String.length suffix) = suffix
    with Invalid_argument _ -> false
  
  let () =
    let forms = open_in "/tmp/formlog" in
    let args = Hashtbl.create 8 in
    let count = ref 0 in
    Unix.lockf (Unix.descr_of_in_channel forms) Unix.F_RLOCK 0;
    try
      while true do
        let line = input_line forms in
        if line = "=" then
          begin
            let his_env = parse_env (Hashtbl.find args "_environs") in
            let host =
              try Hashtbl.find his_env "REMOTE_HOST"
              with Not_found -> "" in
            if host <> "perl.com" && not (ends_with ".perl.com" host)
            then (count :=
                    (!count +
                       int_of_string
                       (try Hashtbl.find args "items requested"
                        with Not_found -> "0")));
            Hashtbl.clear args
          end
        else
          begin
            let index = String.index line '=' in
            Hashtbl.add args
              (unescape (String.sub line 0 index))
              (unescape
                 (String.sub
                    line
                    (index + 1)
                    (String.length line - index - 1)))
          end
      done
    with End_of_file ->
      close_in forms;
      Printf.printf "Total orders: %d\n" !count
  

