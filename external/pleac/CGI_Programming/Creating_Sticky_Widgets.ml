(* ********************************************************************** *)
(* Creating Sticky Widgets *)
(* ********************************************************************** *)
let pleac_Creating_Sticky_Widgets () = 
  #!/usr/bin/env ocaml
  (* who.cgi - run who(1) on a user and format the results nicely *)
  
  #use "topfind";;
  #require "netcgi2";;
  #require "str";;
  
  open Printf
  
  let escape_html = Netencoding.Html.encode ~in_enc:`Enc_utf8 ()
  
  let start_html title =
    sprintf "\
  <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
  <html xmlns=\"http://www.w3.org/1999/xhtml\">
      <head>
          <title>%s</title>
          <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
      </head>
      <body>
  " (escape_html title)
  
  let end_html = "
      </body>
  </html>
  "
  
  let h1 contents = sprintf "<h1>%s</h1>" (String.concat "" contents)
  let p contents = sprintf "<p>%s</p>" (String.concat "" contents)
  let pre contents = sprintf "<pre>%s</pre>" (String.concat "" contents)
  
  let start_form ?(action="") ?(method'="get") () =
    sprintf "<form action=\"%s\" method=\"%s\">"
      (escape_html action) (escape_html method')
  let end_form = "</form>"
  
  let textfield ?(name="") ?(value="") () =
    sprintf "<input type=\"text\" name=\"%s\" value=\"%s\" />"
      (escape_html name) (escape_html value)
  
  let submit ?(name="") ?(value="") () =
    sprintf "<input type=\"submit\" name=\"%s\" value=\"%s\" />"
      (escape_html name) (escape_html value)
  
  let process (cgi : Netcgi.cgi) =
    let print s =
      cgi#out_channel#output_string s;
      cgi#out_channel#output_string "\n" in
  
    let name = cgi#argument_value "WHO" in
  
    (* print search form *)
    cgi#set_header ~content_type:"text/html" ();
    print (start_html "Query Users");
    print (h1 ["Search"]);
    print (start_form ~method':"post" ());
    print (p ["Which user? ";
              textfield ~name:"WHO" ~value:name ()]);
    print (submit ~value:"Query" ());
    print end_form;
  
    (* print results of the query if we have someone to look for *)
    if name <> "" then
      begin
        print (h1 ["Results"]);
        let regexp = Str.regexp name in
        let proc = Unix.open_process_in "who" in
        let found = ref false in
        let html = Buffer.create 0 in
        begin
          (* call who and build up text of response *)
          try
            while true do
              let line = input_line proc in
              (* only lines matching [name] *)
              if Str.string_match regexp line 0 then
                begin
                  Buffer.add_string html (escape_html line ^ "\n");
                  found := true;
                end
            done
          with End_of_file ->
            close_in proc;
            (* nice message if we didn't find anyone by that name *)
            if not !found
            then Buffer.add_string html
              (escape_html name ^ " is not logged in");
        end;
        print (pre [Buffer.contents html]);
      end;
  
    print end_html
  
  let () =
    let config = Netcgi.default_config in
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    let output_type = `Transactional buffered in
    if Unix.isatty Unix.stdin
    then Netcgi_test.run ~config ~output_type process
    else Netcgi_cgi.run ~config ~output_type process
  

