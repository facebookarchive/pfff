(* ********************************************************************** *)
(* Redirecting to a Different Location *)
(* ********************************************************************** *)
let pleac_Redirecting_to_a_Different_Location () = 
  let process (cgi : Netcgi.cgi) =
    let url = "http://caml.inria.fr/cgi-bin/hump.cgi" in
    cgi#set_redirection_header url;
    (* By default, the above will send a 302 Found. To instead send
       a 301 Moved Permanently, use the following command. *)
    cgi#set_header ~status:`Moved_permanently ()
  
  (*-----------------------------*)
  
  (* oreobounce - set a cookie and redirect the browser *)
  
  let process (cgi : Netcgi.cgi) =
    let oreo =
      Netcgi_common.Cookie.make
        ~max_age:(60 * 60 * 24 * 30 * 3)  (* 3 months *)
        ~domain:".sourceforge.nett"
        "filling" "vanilla crème" in
    let whither = "http://somewhere.sourceforge.net/nonesuch.html" in
    cgi#set_redirection_header ~set_cookies:[oreo] whither
  
  let () =
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    Netcgi_apache.run
      ~output_type:(`Transactional buffered)
      (fun cgi -> process (cgi :> Netcgi.cgi))
  
  (*
  HTTP/1.1 302 Found
  Date: Thu, 06 Nov 2008 04:39:53 GMT
  Server: Apache/2.2.9 (Debian) Netcgi_apache/2.2.9 PHP/5.2.6-5 with Suhosin-Patch
  Set-Cookie: filling=vanilla%20cr%E8me;Version=1;Domain=.sourceforge.nt;Max-Age=7776000;Expires=Wed, 04 Feb 2009 04:39:55 +0000
  Location: http://somewhere.sourceforge.net/nonesuch.html
  Status: 302
  Transfer-Encoding: chunked
  Content-Type: text/html
  *)
  
  (*-----------------------------*)
  
  (* os_snipe - redirect to a Jargon File entry about current OS *)
  
  let process (cgi : Netcgi.cgi) =
    let dir = "http://www.wins.uva.nl/%7Emes/jargon" in
    let page =
      match cgi#environment#user_agent with
        | s when Str.string_match
              (Str.regexp ".*Mac") s 0 ->
            "m/Macintrash.html"
        | s when Str.string_match
              (Str.regexp ".*Win\\(dows \\)?NT") s 0 ->
            "e/evilandrude.html"
        | s when Str.string_match
              (Str.regexp ".*\\(Win\\|MSIE\\|WebTV\\)") s 0 ->
            "m/MicroslothWindows.html"
        | s when Str.string_match
              (Str.regexp ".*Linux") s 0 ->
            "l/Linux.html"
        | s when Str.string_match
              (Str.regexp ".*HP-UX") s 0 ->
            "h/HP-SUX.html"
        | s when Str.string_match
              (Str.regexp ".*SunOS") s 0 ->
            "s/ScumOS.html"
        | _ ->
            "a/AppendixB.html" in
    cgi#set_redirection_header (dir ^ "/" ^ page)
  
  let () =
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    Netcgi_apache.run
      ~output_type:(`Transactional buffered)
      (fun cgi -> process (cgi :> Netcgi.cgi))
  
  (*-----------------------------*)
  
  let process (cgi : Netcgi.cgi) =
    cgi#environment#set_status `No_content
  
  (*
  HTTP/1.1 204 No Content
  Date: Thu, 06 Nov 2008 05:25:46 GMT
  Server: Apache/2.2.9 (Debian) Netcgi_apache/2.2.9 PHP/5.2.6-5 with Suhosin-Patch
  Status: 204
  Content-Type: text/html
  *)
  

