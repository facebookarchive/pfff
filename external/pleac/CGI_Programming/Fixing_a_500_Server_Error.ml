(* ********************************************************************** *)
(* Fixing a 500 Server Error *)
(* ********************************************************************** *)
let pleac_Fixing_a_500_Server_Error () = 
  #!/usr/bin/env ocaml
  (* webwhoami - show web users id *)
  
  #use "topfind";;
  #require "netcgi2";;
  #require "unix";;
  
  let process (cgi : Netcgi.cgi) =
    cgi#set_header ~content_type:"text/plain" ();
    cgi#out_channel#output_string
      (Printf.sprintf "Running as %s\n"
         (Unix.getpwuid (Unix.geteuid ())).Unix.pw_name);
    cgi#out_channel#commit_work ()
  
  let () =
    let config = Netcgi.default_config in
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    Netcgi_cgi.run ~config ~output_type:(`Transactional buffered) process
  
  (*-----------------------------*)
  
  (* By using Netcgi_test.run instead of Netcgi_run, you can enable a
     command-line testing mechanism. *)
  
  let () =
    let config = Netcgi.default_config in
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    let output_type = `Transactional buffered in
    if Unix.isatty Unix.stdin
    then Netcgi_test.run ~config ~output_type process
    else Netcgi_cgi.run ~config ~output_type process
  
  (* Now, you can run the CGI script from the command line to test for
     compilation and runtime errors. *)
  $ ./webwhoami -help
  ocaml [options] name1=value1 ... nameN=valueN
    -get               Set the method to GET (the default)
    -head              Set the method to HEAD
    -post              Set the method to POST
    -put file          Set the method to PUT with the file as argument
    -delete            Set the method to DELETE
    -mimetype type     Set the MIME type for the next file argument(s) (default: text/plain)
    -filename path     Set the filename property for the next file argument(s)
    -filearg name=file Specify a file argument whose contents are in the file
    -user name         Set REMOTE_USER to this name
    -prop name=value   Set the environment property
    -header name=value Set the request header field
    -o file            Set the output file (default: stdout)
    -help              Display this list of options
    --help             Display this list of options
  

