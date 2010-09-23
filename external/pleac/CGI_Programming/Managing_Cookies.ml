(* ********************************************************************** *)
(* Managing Cookies *)
(* ********************************************************************** *)
let pleac_Managing_Cookies () = 
  (* Read a cookie: *)
  Netcgi_common.Cookie.value (cgi#environment#cookie "preference name")
  
  (* Make a cookie: *)
  let cookie =
    Netcgi_common.Cookie.make
      ~max_age:(60 * 60 * 24 * 365 * 2)  (* 2 years *)
      "preference name"                  (* name *)
      "whatever you'd like"              (* value*)
  
  (* Write a cookie: *)
  cgi#set_header ~set_cookies:[cookie] ()
  
  (*-----------------------------*)
  
  #!/usr/bin/env ocaml
  (* ic_cookies - sample CGI script that uses a cookie *)
  
  #use "topfind";;
  #require "netcgi2";;
  
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
  let hr = "<hr />"
  let p contents = sprintf "<p>%s</p>" (String.concat "" contents)
  
  let start_form ?(action="") ?(method'="get") () =
    sprintf "<form action=\"%s\" method=\"%s\">"
      (escape_html action) (escape_html method')
  let end_form = "</form>"
  
  let textfield ?(name="") ?(value="") () =
    sprintf "<input type=\"text\" name=\"%s\" value=\"%s\" />"
      (escape_html name) (escape_html value)
  
  let process (cgi : Netcgi.cgi) =
    let cookname = "favorite ice cream" in
    let favorite = cgi#argument_value "flavor" in
    let tasty =
      try Netcgi_common.Cookie.value (cgi#environment#cookie cookname)
      with Not_found -> "mint" in
    let print s =
      cgi#out_channel#output_string s;
      cgi#out_channel#output_string "\n" in
  
    cgi#set_header ~content_type:"text/html" ();
    if favorite = ""
    then
      begin
        print (start_html "Ice Cookies");
        print (h1 ["Hello Ice Cream"]);
        print hr;
        print (start_form ~method':"post" ());
        print (p ["Please select a flavor: ";
                  textfield ~name:"flavor" ~value:tasty ()]);
        print end_form;
        print hr;
        print end_html;
      end
    else
      begin
        let cookie =
          Netcgi_common.Cookie.make
            ~max_age:(60 * 60 * 24 * 365 * 2)  (* 2 years *)
            cookname favorite in
        cgi#set_header ~set_cookies:[cookie] ();
        print (start_html "Ice Cookies, #2");
        print (h1 ["Hello Ice Cream"]);
        print (p ["You chose as your favorite flavor `";
                  escape_html favorite; "'."]);
        print end_html;
      end;
    cgi#out_channel#commit_work ()
  
  let () =
    let config = Netcgi.default_config in
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    let output_type = `Transactional buffered in
    if Unix.isatty Unix.stdin
    then Netcgi_test.run ~config ~output_type process
    else Netcgi_cgi.run ~config ~output_type process
  

