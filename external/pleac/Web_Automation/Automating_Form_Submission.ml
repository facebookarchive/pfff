(* ********************************************************************** *)
(* Automating Form Submission *)
(* ********************************************************************** *)
let pleac_Automating_Form_Submission () = 
  #use "topfind";;
  #require "netclient";;
  open Http_client.Convenience
  
  (* Submit a form using GET. *)
  let url = "http://www.perl.com/cgi-bin/cpan_mod?module=DB_File&readme=1"
  let content = http_get url
  
  (* Submit a form using POST. Since we need to follow a redirect here,
     we can't use the "Convenience" methods. *)
  let url = "http://www.perl.com/cgi-bin/cpan_mod"
  let params = ["module", "DB_File"; "readme", "1"]
  let () =
    let call = new Http_client.post url params in
    call#set_redirect_mode Http_client.Redirect;
    let pipeline = new Http_client.pipeline in
    pipeline#add call;
    pipeline#run ()
  let content = call#response_body#value
  
  (* GET parameters can be URL encoded with Netencoding.Url.encode. *)
  let arg = "\"this isn't <EASY> & <FUN>\""
  Netencoding.Url.encode arg
  (* - : string = "%22this+isn%27t+%3CEASY%3E+%26+%3CFUN%3E%22" *)
  Netencoding.Url.encode ~plus:false arg
  (* - : string = "%22this%20isn%27t%20%3CEASY%3E%20%26%20%3CFUN%3E%22" *)
  
  (* To use a proxy, either set the "http_proxy" environment variable and
     call "set_proxy_from_environment" on the pipeline (done automatically
     for the "Convenience" methods) or set the proxy host and port using
     the "set_proxy" method: *)
  let () = pipeline#set_proxy "localhost" 3128
  

