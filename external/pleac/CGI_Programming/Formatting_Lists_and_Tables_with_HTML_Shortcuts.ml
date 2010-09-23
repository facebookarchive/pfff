(* ********************************************************************** *)
(* Formatting Lists and Tables with HTML Shortcuts *)
(* ********************************************************************** *)
let pleac_Formatting_Lists_and_Tables_with_HTML_Shortcuts () = 
  open Printf
  
  (* Define some HTML helper functions. *)
  let ol contents = sprintf "<ol>%s</ol>" (String.concat "" contents)
  let ul contents = sprintf "<ul>%s</ul>" (String.concat "" contents)
  let li ?(typ="") content =
    if typ = ""
    then sprintf "<li>%s</li>" content
    else sprintf "<li type=\"%s\">%s</li>" typ content
  let tr contents = sprintf "<tr>%s</tr>" (String.concat "" contents)
  let th content = sprintf "<th>%s</th>" content
  let td content = sprintf "<td>%s</td>" content
  
  (* Main CGI process. *)
  let process (cgi : Netcgi.cgi) =
  
    (* Define a print function for convenience. *)
    let print s =
      cgi#out_channel#output_string s;
      cgi#out_channel#output_string "\n" in
  
    (* Print a numbered list. *)
    print (ol (List.map li ["red"; "blue"; "green"]));
  
    (* Print a bulleted list. *)
    let names = ["Larry"; "Moe"; "Curly"] in
    print (ul (List.map (li ~typ:"disc") names));
  
    (* The "li" function gets applied to a single item. *)
    print (li "alpha");
  
    (* If there are multiple items, use List.map. *)
    print (String.concat " " (List.map li ["alpha"; "omega"]));
  
    (* Build a table of states and their cities. *)
    let ( => ) k v = (k, v) in
    let state_cities =
      [
        "Wisconsin"  => [ "Superior"; "Lake Geneva"; "Madison" ];
        "Colorado"   => [ "Denver"; "Fort Collins"; "Boulder" ];
        "Texas"      => [ "Plano"; "Austin"; "Fort Stockton" ];
        "California" => [ "Sebastopol"; "Santa Rosa"; "Berkeley" ];
      ] in
  
    (* Print the table in sorted order. *)
    print "<TABLE> <CAPTION>Cities I Have Known</CAPTION>";
    print (tr (List.map th ["State"; "Cities"]));
    List.iter
      (fun (state, cities) ->
         print (tr (th state :: List.map td (List.sort compare cities))))
      (List.sort compare state_cities);
    print "</TABLE>";
  
    (* Flush the output buffer. *)
    cgi#out_channel#commit_work ()
  
  (*-----------------------------*)
  
  (* salcheck - check for salaries *)
  
  (* Requires ocaml-mysql, available here:
     http://raevnos.pennmush.org/code/ocaml-mysql/
  
     For netcgi_apache, the following configuration directive is needed:
     NetcgiLoad mysql/mysql.cma *)
  
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
  
  let start_form ?(action="") ?(method'="get") () =
    sprintf "<form action=\"%s\" method=\"%s\">"
      (escape_html action) (escape_html method')
  let end_form = "</form>"
  
  let p contents = sprintf "<p>%s</p>" (String.concat "" contents)
  let h1 contents = sprintf "<h1>%s</h1>" (String.concat "" contents)
  
  let textfield ?(name="") ?(value="") () =
    sprintf "<input type=\"text\" name=\"%s\" value=\"%s\" />"
      (escape_html name) (escape_html value)
  
  let submit ?(name="") ?(value="") () =
    sprintf "<input type=\"submit\" name=\"%s\" value=\"%s\" />"
      (escape_html name) (escape_html value)
  
  let tr contents = sprintf "<tr>%s</tr>" (String.concat "" contents)
  let td content = sprintf "<td>%s</td>" content
  
  let process (cgi : Netcgi.cgi) =
    let limit = cgi#argument_value "LIMIT" in
  
    cgi#set_header ~content_type:"text/html" ();
  
    let print s =
      cgi#out_channel#output_string s;
      cgi#out_channel#output_string "\n" in
  
    print (start_html "Salary Query");
    print (h1 ["Search"]);
    print (start_form ());
    print (p ["Enter minimum salary ";
              textfield ~name:"LIMIT" ~value:limit ()]);
    print (submit ~value:"Submit" ());
    print end_form;
  
    if limit <> "" then
      begin
        let db =
          Mysql.quick_connect
            ~user:"username"
            ~password:"password"
            ~database:"somedb"
            ~host:"localhost"
            ~port:3306 () in
        let sql =
          sprintf "
              SELECT name, salary
              FROM   employees
              WHERE  salary > %s
          " (Mysql.ml2float (float_of_string limit)) in
        let result = Mysql.exec db sql in
        print (h1 ["Results"]);
        print "<table border=\"1\">";
        print (String.concat "\n"
                 (Mysql.map result
                    (fun values ->
                       tr [td (escape_html
                                 (Mysql.not_null
                                    Mysql.str2ml values.(0)));
                           td (sprintf "%.2f"
                                 (Mysql.not_null
                                    Mysql.float2ml values.(1)))])));
        print "</table>";
        Mysql.disconnect db;
      end;
  
    print end_html;
    cgi#out_channel#commit_work ()
  
  let () =
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    Netcgi_apache.run
      ~output_type:(`Transactional buffered)
      (fun cgi -> process (cgi :> Netcgi.cgi))
  

