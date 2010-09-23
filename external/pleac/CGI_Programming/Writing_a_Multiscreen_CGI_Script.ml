(* ********************************************************************** *)
(* Writing a Multiscreen CGI Script *)
(* ********************************************************************** *)
let pleac_Writing_a_Multiscreen_CGI_Script () = 
  #!/usr/bin/env ocaml
  
  #use "topfind";;
  #require "netcgi2";;
  
  open Printf
  
  let ( => ) k v = (k, v)
  
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
  
  let hidden ?(name="") ?(value="") () =
    sprintf "<input type=\"hidden\" name=\"%s\" value=\"%s\" />"
      (escape_html name) (escape_html value)
  
  let submit ?(name="") ?(value="") () =
    sprintf "<input type=\"submit\" name=\"%s\" value=\"%s\" />"
      (escape_html name) (escape_html value)
  
  let popup_menu ?(name="") ?(value="") values =
    let options =
      List.map
        (fun (value', label) ->
           sprintf "<option %s value=\"%s\">%s</option>"
             (if value = value' then "selected=\"selected\"" else "")
             (escape_html value')
             (escape_html label))
        values in
    sprintf "<select name=\"%s\">\n%s\n</select>"
      (escape_html name) (String.concat "\n" options)
  
  let standard_header () = h1 ["Program Title"]
  let standard_footer () = "<hr />"
  
  let to_page value = submit ~name:".State" ~value ()
  
  (* when we get a .State that doesn't exist *)
  let no_such_page (cgi : Netcgi.cgi) print = ()
  
  let front_page (cgi : Netcgi.cgi) print active = ()
  let sweater (cgi : Netcgi.cgi) print active = ()
  let checkout (cgi : Netcgi.cgi) print active = ()
  let credit_card (cgi : Netcgi.cgi) print active = ()
  let order (cgi : Netcgi.cgi) print active = ()
  
  let t_shirt (cgi : Netcgi.cgi) print active =
    let size = cgi#argument_value "size" in
    let color = cgi#argument_value "color" in
    if active then
      begin
        print (p ["You want to buy a t-shirt?"]);
        print (p ["Size: ";
                  popup_menu ~name:"size" ~value:size
                    ["XL" => "X-Large";
                     "L"  => "Large";
                     "M"  => "Medium";
                     "S"  => "Small";
                     "XS" => "X-Small"]]);
        print (p ["Color: ";
                  popup_menu ~name:"color" ~value:color
                    ["Black" => "Black"; "White" => "White"]]);
        print (p [to_page "Shoes"; to_page "Checkout"]);
      end
    else
      begin
        print (hidden ~name:"size" ~value:size ());
        print (hidden ~name:"color" ~value:color ());
      end
  
  let states =
    [
      "Default"  => front_page;
      "Shirt"    => t_shirt;
      "Sweater"  => sweater;
      "Checkout" => checkout;
      "Card"     => credit_card;
      "Order"    => order;
      "Cancel"   => front_page;
    ]
  
  let process (cgi : Netcgi.cgi) =
    let page = cgi#argument_value ~default:"Default" ".State" in
    cgi#set_header ~content_type:"text/html" ();
    let print s =
      cgi#out_channel#output_string s;
      cgi#out_channel#output_string "\n" in
    print (start_html "Program Title");
    print (standard_header ());
    print (start_form ());
    if List.mem_assoc page states
    then List.iter (fun (state, sub) ->
                      sub cgi print (page = state)) states
    else no_such_page cgi print;
    print (standard_footer ());
    print end_form;
    print end_html;
    cgi#out_channel#commit_work ()
  
  let () =
    let config = Netcgi.default_config in
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    let output_type = `Transactional buffered in
    if Unix.isatty Unix.stdin
    then Netcgi_test.run ~config ~output_type process
    else Netcgi_cgi.run ~config ~output_type process
  

