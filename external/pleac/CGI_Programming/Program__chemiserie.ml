(* ********************************************************************** *)
(* Program: chemiserie *)
(* ********************************************************************** *)
let pleac_Program__chemiserie () = 
  #!/usr/bin/env ocaml
  (* chemiserie - simple CGI shopping for shirts and sweaters *)
  
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
  let h2 contents = sprintf "<h2>%s</h2>" (String.concat "" contents)
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
  
  let textfield ?(name="") ?(value="") () =
    sprintf "<input type=\"text\" name=\"%s\" value=\"%s\" />"
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
  
  let defaults label =
    sprintf "<input type=\"button\" value=\"%s\" onclick=\"%s\" />"
      (escape_html label) "javascript:location.href='?'"
  
  let to_page value = submit ~name:".State" ~value ()
  
  (********************************
   * header, footer, menu functions
   ********************************)
  
  let standard_header print =
    print (start_html "Shirts");
    print (start_form ())
  
  let standard_footer print =
    print end_form;
    print end_html
  
  let shop_menu print =
    print (p [defaults "Empty My Shopping Cart";
              to_page "Shirt";
              to_page "Sweater";
              to_page "Checkout"])
  
  (*****************************
   * subroutines for each screen
   *****************************)
  
  (* The default page. *)
  let front_page cgi print active =
    if active then
      begin
        print (h1 ["Hi!"]);
        print "Welcome to our Shirt Shop!  Please make your selection ";
        print "from the menu below.";
        shop_menu print;
      end
  
  (* Page to order a shirt from. *)
  let shirt (cgi : Netcgi.cgi) print active =
    let sizes = ["XL" => "X-Large";
                 "L"  => "Large";
                 "M"  => "Medium";
                 "S"  => "Small";
                 "XS" => "X-Small"] in
    let colors = ["Black" => "Black"; "White" => "White"] in
  
    let size, color, count =
      cgi#argument_value "shirt_size",
      cgi#argument_value "shirt_color",
      cgi#argument_value "shirt_count" in
  
    (* sanity check *)
    let size =
      if List.mem_assoc size sizes
      then size
      else fst (List.hd sizes) in
    let color =
      if List.mem_assoc color colors
      then color
      else fst (List.hd colors) in
  
    if active then
      begin
        print (h1 ["T-Shirt"]);
        print (p ["What a shirt!  This baby is decked out with all the ";
                  "options. It comes with full luxury interior, cotton ";
                  "trim, and a collar to make your eyes water! ";
                  "Unit price: $33.00"]);
        print (h2 ["Options"]);
        print (p ["How Many? ";
                  textfield
                    ~name:"shirt_count"
                    ~value:count ()]);
        print (p ["Size? ";
                  popup_menu ~name:"shirt_size" ~value:size sizes]);
        print (p ["Color? ";
                  popup_menu ~name:"shirt_color" ~value:color colors]);
        shop_menu print;
      end
    else
      begin
        if size <> ""
        then print (hidden ~name:"shirt_size" ~value:size ());
        if color <> ""
        then print (hidden ~name:"shirt_color" ~value:color ());
        if count <> ""
        then print (hidden ~name:"shirt_count" ~value:count ());
      end
  
  (* Page to order a sweater from. *)
  let sweater (cgi : Netcgi.cgi) print active =
    let sizes = ["XL" => "X-Large";
                 "L"  => "Large";
                 "M"  => "Medium"] in
    let colors = ["Chartreuse" => "Chartreuse";
                  "Puce" => "Puce";
                  "Lavender" => "Lavender"] in
  
    let size, color, count =
      cgi#argument_value "sweater_size",
      cgi#argument_value "sweater_color",
      cgi#argument_value "sweater_count" in
  
    (* sanity check *)
    let size =
      if List.mem_assoc size sizes
      then size
      else fst (List.hd sizes) in
    let color =
      if List.mem_assoc color colors
      then color
      else fst (List.hd colors) in
  
    if active then
      begin
        print (h1 ["Sweater"]);
        print (p ["Nothing implies preppy elegance more than this fine ";
                  "sweater.  Made by peasant workers from black market ";
                  "silk, it slides onto your lean form and cries out ";
                  "``Take me, for I am a god!''.  Unit price: $49.99."]);
        print (h2 ["Options"]);
        print (p ["How Many? ";
                  textfield
                    ~name:"sweater_count"
                    ~value:count ()]);
        print (p ["Size? ";
                  popup_menu ~name:"sweater_size" ~value:size sizes]);
        print (p ["Color? ";
                  popup_menu ~name:"sweater_color" ~value:color colors]);
        shop_menu print;
      end
    else
      begin
        if size <> ""
        then print (hidden ~name:"sweater_size" ~value:size ());
        if color <> ""
        then print (hidden ~name:"sweater_color" ~value:color ());
        if count <> ""
        then print (hidden ~name:"sweater_count" ~value:count ());
      end
  
  let calculate_price (cgi : Netcgi.cgi) =
    let shirts =
      try int_of_string (cgi#argument_value "shirt_count")
      with Failure _ -> 0 in
    let sweaters =
      try int_of_string (cgi#argument_value "shirt_count")
      with Failure _ -> 0 in
    sprintf "$%.2f" (float shirts *. 33.0 +. float sweaters *. 49.99)
  
  (* Returns HTML for the current order ("You have ordered ...") *)
  let order_text (cgi : Netcgi.cgi) =
    let shirt_count = cgi#argument_value "shirt_count" in
    let shirt_size = cgi#argument_value "shirt_size" in
    let shirt_color = cgi#argument_value "shirt_color" in
  
    let sweater_count = cgi#argument_value "sweater_count" in
    let sweater_size = cgi#argument_value "sweater_size" in
    let sweater_color = cgi#argument_value "sweater_color" in
  
    let html = Buffer.create 0 in
  
    if not (List.mem shirt_count [""; "0"]) then
      Buffer.add_string html
        (p ["You have ordered "; escape_html shirt_count;
            " shirts of size "; escape_html shirt_size;
            " and color "; escape_html shirt_color; "."]);
  
    if not (List.mem sweater_count [""; "0"]) then
      Buffer.add_string html
        (p ["You have ordered "; escape_html sweater_count;
            " sweaters of size "; escape_html sweater_size;
            " and color "; escape_html sweater_color; "."]);
  
    let html = Buffer.contents html in
    match html with
      | "" -> p ["Nothing!"]
      | html -> html ^ p ["For a total cost of "; calculate_price cgi]
  
  (* Page to display current order for confirmation. *)
  let checkout (cgi : Netcgi.cgi) print active =
    if active then
      begin
        print (h1 ["Order Confirmation"]);
        print (p ["You ordered the following:"]);
        print (order_text cgi);
        print (p ["Is this right?  Select 'Card' to pay for the items ";
                  "or 'Shirt' or 'Sweater' to continue shopping."]);
        print (p [to_page "Card";
                  to_page "Shirt";
                  to_page "Sweater"]);
      end
  
  (* Page to gather credit-card information. *)
  let credit_card (cgi : Netcgi.cgi) print active =
    let widgets = ["Name"; "Address1"; "Address2"; "City"; "Zip"; "State";
                   "Phone"; "Card"; "Expiry"] in
    if active then
      begin
        print (pre [p ["Name:          ";
                       textfield
                         ~name:"Name"
                         ~value:(cgi#argument_value "Name") ()];
                    p ["Address:       ";
                       textfield
                         ~name:"Address1"
                         ~value:(cgi#argument_value "Address1") ()];
                    p ["               ";
                       textfield
                         ~name:"Address2"
                         ~value:(cgi#argument_value "Address2") ()];
                    p ["City:          ";
                       textfield
                         ~name:"City"
                         ~value:(cgi#argument_value "City") ()];
                    p ["Zip:           ";
                       textfield
                         ~name:"Zip"
                         ~value:(cgi#argument_value "Zip") ()];
                    p ["State:         ";
                       textfield
                         ~name:"State"
                         ~value:(cgi#argument_value "State") ()];
                    p ["Phone:         ";
                       textfield
                         ~name:"Phone"
                         ~value:(cgi#argument_value "Phone") ()];
                    p ["Credit Card *: ";
                       textfield
                         ~name:"Card"
                         ~value:(cgi#argument_value "Card") ()];
                    p ["Expiry:        ";
                       textfield
                         ~name:"Expiry"
                         ~value:(cgi#argument_value "Expiry") ()]]);
  
        print (p ["Click on 'Order' to order the items. ";
                  "Click on 'Cancel' to return shopping."]);
  
        print (p [to_page "Order"; to_page "Cancel"]);
      end
    else
      begin
        List.iter
          (fun widget ->
             print (hidden
                      ~name:widget
                      ~value:(cgi#argument_value widget) ()))
          widgets
      end
  
  (* Page to complete an order. *)
  let order cgi print active =
    if active then
      begin
        (* you'd check credit card values here *)
        print (h1 ["Ordered!"]);
        print (p ["You have ordered the following toppings:"]);
        print (order_text cgi);
  
        print (p [defaults "Begin Again"]);
      end
  
  (* state table mapping pages to functions *)
  type page = Netcgi.cgi -> (string -> unit) -> bool -> unit
  let (states : (string * page) list) =
    [
      "Default"  => front_page;
      "Shirt"    => shirt;
      "Sweater"  => sweater;
      "Checkout" => checkout;
      "Card"     => credit_card;
      "Order"    => order;
      "Cancel"   => front_page;
    ]
  
  let no_such_page (cgi : Netcgi.cgi) print current_screen =
    print ("No screen for " ^ current_screen)
  
  let process (cgi : Netcgi.cgi) =
    let current_screen = cgi#argument_value ~default:"Default" ".State" in
  
    let print s =
      cgi#out_channel#output_string s;
      cgi#out_channel#output_string "\n" in
  
    (* Generate the current page. *)
    cgi#set_header ~content_type:"text/html" ();
    standard_header print;
    if List.mem_assoc current_screen states
    then List.iter (fun (state, sub) ->
                      sub cgi print (current_screen = state)) states
    else no_such_page cgi print current_screen;
    standard_footer print;
    cgi#out_channel#commit_work ()
  
  let () =
    let config = Netcgi.default_config in
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    let output_type = `Transactional buffered in
    if Unix.isatty Unix.stdin
    then Netcgi_test.run ~config ~output_type process
    else Netcgi_cgi.run ~config ~output_type process
  
  

