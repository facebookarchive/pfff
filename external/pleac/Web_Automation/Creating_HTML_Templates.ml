(* ********************************************************************** *)
(* Creating HTML Templates *)
(* ********************************************************************** *)
let pleac_Creating_HTML_Templates () = 
  (* Template replacement using regular expressions from the Str module. *)
  #load "str.cma";;
  
  let slurp_channel channel =
    let buffer_size = 4096 in
    let buffer = Buffer.create buffer_size in
    let string = String.create buffer_size in
    let chars_read = ref 1 in
    while !chars_read <> 0 do
      chars_read := input channel string 0 buffer_size;
      Buffer.add_substring buffer string 0 !chars_read
    done;
    Buffer.contents buffer
  
  let slurp_file filename =
    let channel = open_in_bin filename in
    let result =
      try slurp_channel channel
      with e -> close_in channel; raise e in
    close_in channel;
    result
  
  let template_regexp = Str.regexp "%%\\([^%]+\\)%%"
  
  let template filename fillings =
    let text = slurp_file filename in
    let eval s =
      try Hashtbl.find fillings s
      with Not_found -> "" in
    let replace _ =
      eval (Str.matched_group 1 text) in
    Str.global_substitute template_regexp replace text
  
  (*-----------------------------*)
  
  (* Alternative implementation: a hand-written stream parser. This version
     avoids loading the whole template into memory, so it is efficient for
     large files. *)
  
  let template filename fillings =
    let f = open_in filename in
    try
      let buffer = Buffer.create (in_channel_length f) in
      let text = Stream.of_channel f in
      let eval s =
        try Hashtbl.find fillings s
        with Not_found -> "" in
      let rec search () =
        match Stream.peek text with
          | None -> ()
          | Some '%' ->
              Stream.junk text;
              (match Stream.peek text with
                 | None ->
                     Buffer.add_char buffer '%';
                     search ()
                 | Some '%' ->
                     Stream.junk text;
                     replace ""
                 | Some c ->
                     Stream.junk text;
                     Buffer.add_char buffer '%';
                     Buffer.add_char buffer c;
                     search ())
          | Some c ->
              Stream.junk text;
              Buffer.add_char buffer c;
              search ()
      and replace acc =
        match Stream.peek text with
          | None ->
              Buffer.add_string buffer "%%";
              Buffer.add_string buffer acc
          | Some '%' ->
              Stream.junk text;
              (match Stream.peek text with
                 | None ->
                     Buffer.add_string buffer "%%";
                     Buffer.add_string buffer acc;
                     Buffer.add_char buffer '%'
                 | Some '%' ->
                     Stream.junk text;
                     Buffer.add_string buffer (eval acc);
                     search ()
                 | Some c ->
                     Stream.junk text;
                     replace (acc ^ "%" ^ (String.make 1 c)))
          | Some c ->
              Stream.junk text;
              replace (acc ^ (String.make 1 c)) in
      search ();
      close_in f;
      Buffer.contents buffer
    with e ->
      close_in f;
      raise e
  
  (*-----------------------------*)
  
  (* simple.template contains the following:
  
  <!-- simple.template for internal template() function -->
  <HTML><HEAD><TITLE>Report for %%username%%</TITLE></HEAD>
  <BODY><H1>Report for %%username%%</H1>
  %%username%% logged in %%count%% times, for a total of %%total%% minutes.
  
  *)
  
  let () =
    let fields = Hashtbl.create 3 in
    Hashtbl.replace fields "username" whats_his_name;
    Hashtbl.replace fields "count" (string_of_int login_count);
    Hashtbl.replace fields "total" (string_of_int minute_used);
    print_endline (template "simple.template" fields)
  
  (* Output:
  
  <!-- simple.template for internal template() function -->
  <HTML><HEAD><TITLE>Report for ramen</TITLE></HEAD>
  <BODY><H1>Report for ramen</H1>
  ramen logged in 42 times, for a total of 123 minutes.
  
  *)
  
  (*-----------------------------*)
  
  (* userrep - report duration of user logins using SQL database *)
  
  let process (cgi : Netcgi.cgi) =
    cgi#set_header ~content_type:"text/html" ();
    begin
      match cgi#argument_value "username" with
        | "" ->
            cgi#out_channel#output_string "No username"
        | user ->
            let db =
              Mysql.quick_connect
                ~user:"user"
                ~password:"seekritpassword"
                ~database:"connections" () in
            let sql = Printf.sprintf "
              SELECT COUNT(duration),SUM(duration)
              FROM logins WHERE username='%s'
            " (Mysql.escape user) in
            let result = Mysql.exec db sql in
            let default d = function Some x -> x | None -> d in
            let (count, total) =
              match Mysql.fetch result with
                | None -> ("0", "0")
                | Some row ->
                    (default "0" row.(0),
                     default "0" row.(1)) in
            (* template defined in the solution above *)
            let tpl = template "report.tpl" in
            let vars = Hashtbl.create 3 in
            Hashtbl.replace vars "username" user;
            Hashtbl.replace vars "count" count;
            Hashtbl.replace vars "total" total;
            cgi#out_channel#output_string (tpl vars)
    end;
    cgi#out_channel#commit_work ()
  
  let () =
    let config = Netcgi.default_config in
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    Netcgi_cgi.run ~config ~output_type:(`Transactional buffered) process
  

