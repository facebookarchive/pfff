(* ********************************************************************** *)
(* Parsing a Web Server Log File *)
(* ********************************************************************** *)
let pleac_Parsing_a_Web_Server_Log_File () = 
  (* Use the Weblogs library by Richard Jones:
     http://merjis.com/developers/weblogs
  
     You will also need the HostIP library:
     http://merjis.com/developers/hostip *)
  
  let log = Weblogs.import_file "/var/log/apache2/access.log"
  
  let () =
    Array.iter
      (fun {Weblogs.src_ip=client;
            remote_username=identuser;
            username=authuser;
            t=datetime;
            http_method=method';
            full_url=url;
            http_version=protocol;
            rcode=status;
            size=bytes;
            (* Many more fields are available.
               See Weblogs API documentation for details. *)
           } ->
         (* ... *)
         ())
      log
  

