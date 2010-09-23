(* ********************************************************************** *)
(* Executing an SQL Command Using DBI and DBD *)
(* ********************************************************************** *)
let pleac_Executing_an_SQL_Command_Using_DBI_and_DBD () = 
  (* This example uses OCaml DBI, a component of the mod_caml web development
     library that provides a database abstraction API very similar to that of
     Perl DBI. It is available for download here:
  
     http://merjis.com/developers/mod_caml
  
     Drivers for particular databases are listed in the introduction. *)
  
  #load "nums.cma";;
  #directory "+num-top";;
  #load "num_top.cma";;
  
  #directory "+mysql";;
  #load "mysql.cma";;
  
  #directory "+dbi";;
  #load "dbi.cma";;
  #load "dbi_mysql.cmo";;
  
  (* With dbi installed via findlib, the above can be shortened to:
  
     #use "topfind";;
     #require "dbi.mysql";;
  *)
  
  let () =
    let dbh =
      Dbi_mysql.connect
        ~user:"user"
        ~password:"auth"
        "database" in
  
    let _ = dbh#ex sql [] in
  
    let sth = dbh#prepare sql in
    sth#execute [];
    sth#iter
      (fun row ->
         print_endline (Dbi.sdebug row);
         (* ... *)
         ());
  
    sth#finish ();
    dbh#close ()
  
  (*-----------------------------*)
  
  (* dbusers - manage MySQL user table *)
  
  (* This example uses the Mysql module directly rather than going through
     OCaml DBI. See the introduction for a link to the Mysql library. *)
  
  #load "unix.cma";;
  #directory "+mysql";;
  #load "mysql.cma";;
  
  let () =
    let db =
      Mysql.quick_connect
        ~user:"user"
        ~password:"password"
        ~database:"dbname" () in
  
    ignore (Mysql.exec db "CREATE TABLE users (uid INT, login CHAR(8))");
  
    let passwd = open_in "/etc/passwd" in
    begin
      try
        while true do
          let line = input_line passwd in
          let user = String.sub line 0 (String.index line ':') in
          let {Unix.pw_uid=uid; pw_name=name} = Unix.getpwnam user in
          let sql =
            Printf.sprintf "INSERT INTO users VALUES( %s, %s )"
              (Mysql.ml2int uid)
              (Mysql.ml2str name) in
          ignore (Mysql.exec db sql)
        done
      with End_of_file ->
        close_in passwd
    end;
  
    ignore (Mysql.exec db "DROP TABLE users");
  
    Mysql.disconnect db
  

