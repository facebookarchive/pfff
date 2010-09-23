(* ********************************************************************** *)
(* Writing a CGI Script *)
(* ********************************************************************** *)
let pleac_Writing_a_CGI_Script () = 
  #!/usr/bin/env ocaml
  (* hiweb - load CGI module to decode information given by web server *)
  
  #use "topfind";;                        (* Findlib *)
  #require "netcgi2";;                    (* Ocamlnet *)
  
  (* Create an HTML escaping function for the UTF-8 encoding. *)
  let escape_html = Netencoding.Html.encode ~in_enc:`Enc_utf8 ()
  
  (* Construct the beginning of an (X)HTML document. *)
  let start_html title =
    Printf.sprintf "\
  <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
  <html xmlns=\"http://www.w3.org/1999/xhtml\">
      <head>
          <title>%s</title>
          <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
      </head>
      <body>
  
  " (escape_html title)
  
  (* Construct the end of an (X)HTML document. *)
  let end_html = "
  
      </body>
  </html>
  "
  
  (* Construct a few common elements. *)
  let p contents =
    Printf.sprintf "<p>%s</p>" (String.concat "" contents)
  let tt contents =
    Printf.sprintf "<tt>%s</tt>" (String.concat "" contents)
  
  (* Process a page request. *)
  let process (cgi : Netcgi.cgi) =
    (* Get a parameter from a form. *)
    let value = cgi#argument_value "PARAM_NAME" in
  
    (* Output a document. *)
    let out = cgi#out_channel#output_string in
    out (start_html "Howdy there!");
    out (p ["You typed: "; tt [escape_html value]]);
    out end_html;
  
    (* Flush the output buffer. *)
    cgi#out_channel#commit_work ()
  
  (* Initialize and run the Netcgi process. *)
  let () =
    let config = Netcgi.default_config in
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    Netcgi_cgi.run ~config ~output_type:(`Transactional buffered) process
  
  (*-----------------------------*)
  
  (* Set the output mime-type and expiration time. *)
  cgi#set_header ~content_type:"text/html" ~cache:(`Max_age 3600) ()
  
  (*-----------------------------*)
  
  (* Read multiple form fields, one containing multiple values. *)
  let who = cgi#argument_value "Name" in
  let phone = cgi#argument_value "Number" in
  let picks =
    List.map
      (fun arg -> arg#value)
      (cgi#multiple_argument "Choices") in
  (* ... *)
  

