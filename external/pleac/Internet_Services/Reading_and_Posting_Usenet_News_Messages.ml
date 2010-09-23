(* ********************************************************************** *)
(* Reading and Posting Usenet News Messages *)
(* ********************************************************************** *)
let pleac_Reading_and_Posting_Usenet_News_Messages () = 
  (* There is no NNTP library available for OCaml. With a little
     preparation, we can easily use the one that comes with Perl
     using perl4caml (http://merjis.com/developers/perl4caml) *)
  
  #directory "+perl";;
  #load "perl4caml.cma";;
  
  module NNTP = struct
    open Perl
    let _ = eval "use Net::NNTP"
  
    (* Returned by "list" method so that newsgroups stay sorted. *)
    module GroupMap = Map.Make(String)
  
    (* Wrapper for Net::NNTP class. *)
    class nntp host =
      let nntp =
        call_class_method "Net::NNTP" "new" [sv_of_string host] in
  
      (* Raise a Failure exception if we couldn't connect. *)
      let () =
        if sv_is_undef nntp
        then failwith (string_of_sv (eval "$!")) in
  
      (* Helper function to transform nullable string arrays to OCaml. *)
      let maybe_string_list sv =
        if sv_is_undef sv
        then raise Not_found
        else List.map string_of_sv (list_of_av (deref_array sv)) in
  
    object (self)
      val nntp = nntp
  
      method group name =
        match call_method_array nntp "group" [sv_of_string name] with
          | [narticles; first; last; name] ->
              (int_of_sv narticles, int_of_sv first,
               int_of_sv last, string_of_sv name)
          | _ -> raise Not_found
  
      method head msgid =
        maybe_string_list (call_method nntp "head" [sv_of_int msgid])
  
      method body msgid =
        maybe_string_list (call_method nntp "body" [sv_of_int msgid])
  
      method article msgid =
        maybe_string_list (call_method nntp "article" [sv_of_int msgid])
  
      method postok () =
        bool_of_sv (call_method nntp "postok" [])
  
      method post lines =
        let lines = List.map sv_of_string lines in
        if (sv_is_undef (call_method nntp "post" lines))
        then failwith (string_of_sv (eval "$!"))
  
      method list () =
        let hv = deref_hash (call_method nntp "list" []) in
        let map = ref GroupMap.empty in
        List.iter
          (fun (name, info) ->
             map :=
               GroupMap.add
                 name
                 (match list_of_av (deref_array info) with
                    | [last; first; flags] ->
                        (int_of_sv last, int_of_sv first,
                         string_of_sv flags)
                    | _ -> assert false)
                 !map)
          (assoc_of_hv hv);
        !map
  
      method quit () =
        ignore (call_method nntp "quit" [])
    end
  end
  
  (*-----------------------------*)
  
  (* Connect to an NNTP server by creating an "nntp" object. *)
  let server =
    try new NNTP.nntp "news.west.cox.net"
    with Failure s ->
      Printf.eprintf "Can't connect to news server: %s\n" s;
      exit 1
  
  (*-----------------------------*)
  
  (* Select a newsgroup and retrieve its stats. *)
  let (narticles, first, last, name) =
    try server#group "misc.test"
    with Not_found ->
      Printf.eprintf "Can't select misc.test\n";
      exit 1
  
  (*-----------------------------*)
  
  (* Get the headers from the last article. *)
  let headers =
    try server#head last
    with Not_found ->
      Printf.eprintf "Can't get headers from article %d in %s\n"
        last name;
      exit 1
  
  (*-----------------------------*)
  
  (* Get the body from the last article. *)
  let body =
    try server#head last
    with Not_found ->
      Printf.eprintf "Can't get body from article %d in %s\n"
        last name;
      exit 1
  
  (*-----------------------------*)
  
  (* Get the headers and body from the last article. *)
  let article =
    try server#head last
    with Not_found ->
      Printf.eprintf "Can't get article from article %d in %s\n"
        last name;
      exit 1
  
  (*-----------------------------*)
  
  (* Determine if posting is allowed with this server. *)
  let () =
    if not (server#postok ())
    then Printf.eprintf "Server didn't tell me I could post.\n"
  
  (*-----------------------------*)
  
  (* Post a message. *)
  let () =
    begin
      try server#post lines
      with Failure s ->
        Printf.eprintf "Can't post: %s\n" s;
        exit 1
    end
  
  (*-----------------------------*)
  
  (* Get the complete list of newsgroups. *)
  let () =
    let groupmap = server#list () in
    NNTP.GroupMap.iter
      (fun group (last, first, flags) ->
         if flags = "y"
         then (* I can post to [group] *) ())
      groupmap
  

