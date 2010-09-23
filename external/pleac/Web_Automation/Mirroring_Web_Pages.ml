(* ********************************************************************** *)
(* Mirroring Web Pages *)
(* ********************************************************************** *)
let pleac_Mirroring_Web_Pages () = 
  #use "topfind";;
  #require "unix";;
  #require "netclient";;
  
  let mirror url file =
    let call = new Http_client.get url in
    begin
      try
        let mtime = (Unix.stat file).Unix.st_mtime in
        let date = Netdate.mk_mail_date mtime in
        call#set_req_header "If-Modified-Since" date
      with Unix.Unix_error _ -> ()
    end;
    call#set_response_body_storage (`File (fun () -> file));
    let pipeline = new Http_client.pipeline in
    pipeline#add call;
    pipeline#run ();
    if call#response_status = `Ok
    then (let date =
            Netdate.parse
              (call#response_header#field "Last-Modified") in
          Unix.utimes file 0.0 (Netdate.since_epoch date));
    call#response_status
  

