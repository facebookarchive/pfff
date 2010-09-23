(**************************************************************************)
(*     Lablgtk - Camlirc                                                  *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Nobuaki Yoshida  <nyoshi@dd.iij4u.or.jp>                          *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: server.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
open Unix
open Printf
open Xml_lexer
open Xml

exception Config_error
exception Unknown_server_error of string

class server_info_signals ~(name_changed: (string * string) GUtil.signal) =
  object
    inherit GUtil.ml_signals [name_changed#disconnect]
    method name_changed = name_changed#connect ~after
  end


class server_info ~(setting_name : string)
    ?(server = "") ?(port = 6667) ?(timeout = 0.01) 
    ?(passwd = "") ?(quit = "CamlIRC") ?(part = "bye...") 
     () =
  let user_entry = Constants.user_entry
  and name_changed = new GUtil.signal ()
  in
  object (self)
    val mutable setting_name = setting_name
    val mutable server = server
    val mutable port = port
    val mutable timeout = timeout
    val mutable nick = user_entry.pw_name
    val mutable clienthost = gethostname()
    val mutable username = user_entry.pw_name
    val mutable server_addr = None
    val mutable passwd = passwd
    val mutable fullname = user_entry.pw_gecos
    val mutable quit = quit
    val mutable part = part
    val mutable channel_list  : (string * string) list = []
    val mutable auto_connect = false
    val mutable auto_url_open = false
    method name_changed = name_changed
    method connect = new server_info_signals ~name_changed
    method setting_name () = setting_name
    method server () = server
    method port () = port
    method sock_addr () = 
      match server_addr with Some s -> s 
      | None -> raise Config_error
    method timeout () = timeout
    method clienthost () = clienthost
    method nick () = nick
    method username () = username
    method passwd () = passwd
    method fullname () = fullname
    method quit_message () = quit
    method part_message () = part
    method channel_list () = channel_list
    method auto_connect () = auto_connect
    method auto_url_open () = auto_url_open
    method make_server_addr () = 
      let server_entry = 
	try gethostbyname server 
	with Not_found -> raise Config_error
      in
      server_addr <- 
	Some 
	  (ADDR_INET (Array.get server_entry.h_addr_list 0, port))
    method set_setting_name s = 
      let
	  org_s = setting_name 
      in setting_name <- s; name_changed#call (org_s, s)
    method set_server a =
      begin
	server <- a;
      end
    method set_sock_addr a = server_addr <- a
    method set_timeout a = timeout <- a
    method set_port a = port <- a
    method set_clienthost a = 
      clienthost <- if a = "" then gethostname () else a 
    method set_nick a =
      nick <- if a = "" then user_entry.pw_name else a
    method set_username a =
      username <- if a = "" then user_entry.pw_name else a
    method set_passwd a = passwd <- a
    method set_fullname a = 
      fullname <- if a = "" then user_entry.pw_gecos else a
    method set_quit_message a = quit <- a
    method set_part_message a = part <- a
    method add_channel_list cn cm = 
      if not (List.exists (fun (s,_) -> s = cn) channel_list) then
	channel_list <- (cn, cm) :: channel_list
    method remove_channel_list cn = 
      channel_list <- List.filter (fun (s,_) -> not (cn = s)) channel_list
    method set_channel_list cn = channel_list <- cn
    method set_auto_connect b = auto_connect <- b
    method set_auto_url_open b = auto_url_open <- b
    method self = self
    initializer
      ()
  end

class server_info_list_signals ~(changed: unit GUtil.signal) =
  object
    inherit GUtil.ml_signals [changed#disconnect]
    method changed = changed#connect ~after
  end

class server_info_list ~(servers : server_info list) =
  let changed = new GUtil.signal ()
  in
  object (self)
    val mutable servers = List.map (fun s-> (s#setting_name(), s)) servers
    method servers () = servers
    method changed = changed
    method connect = new server_info_list_signals ~changed:self#changed
    method add_server s = 
      begin 
	servers <- (s#setting_name(), s)::servers;
	changed#call ()
      end
    method delete_server s = 
      begin 
	servers <- List.remove_assoc s servers;
	changed#call ()
      end
    method replace_server s = 
      servers <- List.remove_assoc (s#setting_name()) servers;
      servers <- (s#setting_name(), s)::servers;
      print_string ("replaced "^(s#setting_name())^" as "^
		    (s#server ())^"\n");
	flush Pervasives.stdout;
    method change_setting_name ~from_sn ~(to_sn:string) =
      let
	  s = self#get_server_setting from_sn
      in
      begin 
	self#delete_server from_sn;
	self#add_server s
      end
    method server_names () =  List.map (fun s -> fst s) servers
    method get_server_setting s = 
      try
	List.assoc s servers 
      with Not_found -> raise Config_error
    method get_primary_setting () =
      match servers with
	[] -> None
      | h::_ -> Some h

    method load_settings ~file =
      try
        let setting_dtd =
          { tags =
              ("channel", (["mode",`Required], text)) ::
            List.map (fun tag -> tag, ([], text))
                ["server"; "port"; "nick"; "username"; "passwd"; "fullname";
                 "quitmessage"; "partmessage"; "autoconnect"; "autourlopen"];
            allow = `Tags } in
        let dtd =
          { tags = ["setting", (["name",`Required], setting_dtd)];
            allow = `Tags } in
        let handle_subnode ~e elt =
          match elt.elt_desc with
          | Node(tag, attrs, l) ->
              let l = List.map  
                  (function {elt_desc=Text s} -> s | _ -> assert false) l in
              let s = String.concat "\n" l in
              begin match tag with
	      | "server"      -> e#set_server s
	      | "port"        -> e#set_port (int_of_string s)
	      | "nick"        -> e#set_nick s
	      | "username"    -> e#set_username s
	      | "passwd"      -> e#set_passwd s
	      | "fullname"    -> e#set_fullname s
	      | "quitmessage" -> e#set_quit_message s
	      | "partmessage" -> e#set_part_message s
	      | "channel"     -> e#add_channel_list s (List.assoc "mode" attrs)
	      | "autoconnect" -> e#set_auto_connect (bool_of_string s)
	      | "autourlopen" -> e#set_auto_url_open (bool_of_string s)
	      | _ -> assert false
              end
          | _ -> assert false
        in
        let handle_setting elt =
          match elt.elt_desc with
          | Node ("setting", attrs, subnodes) ->
              let e =
                new server_info ~setting_name:(List.assoc "name" attrs) () in
              List.iter (handle_subnode ~e) subnodes ;
              self#add_server e
	  | _ -> assert false
        in
        List.iter handle_setting
          (Xml.parse_file file ~doctype:Constants.doctype ~dtd)
      with
      | Xml_lexer.Error (err, pos) ->
	  eprintf "In file %s, char %d: %s.\n" file pos
	    (Xml_lexer.error_string err);
          flush Pervasives.stderr
      | Sys_error _ -> ()
	    
    method save_settings ~file =
      let oc = open_out file in
      fprintf oc "<!DOCTYPE \"%s\">\n" Constants.doctype;
      List.iter
        begin fun e ->
	  let e = snd e
	  in
          fprintf oc "<SETTING NAME=\"%s\">\n" (e#setting_name ());
          List.iter
            (fun (tag, text) -> fprintf oc "  <%s>%s</%s>\n" tag text tag)
            [ "SERVER", e#server ();
	      "PORT", string_of_int (e#port ());
              "NICK", e#nick ();
	      "USERNAME", e#username ();
	      "PASSWD", e#passwd ();
	      "FULLNAME", e#fullname ();
	      "AUTOCONNECT", string_of_bool (e#auto_connect ());
	      "AUTOURLOPEN", string_of_bool (e#auto_url_open ());
	      "QUITMESSAGE", e#quit_message ();
	      "PARTMESSAGE", e#part_message ()];
	  List.iter
            (fun (t,m) -> 
	      fprintf oc "  <CHANNEL MODE=\"%s\">%s</CHANNEL>\n" m t)
	    (e#channel_list ());
          fprintf oc "</SETTING>\n"
        end 
	servers ;
      close_out oc
  end
