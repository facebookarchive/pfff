(* ********************************************************************** *)
(* Creating a Robot *)
(* ********************************************************************** *)
let pleac_Creating_a_Robot () = 
  #load "str.cma";;
  
  (* Parse "robots.txt" content from a stream of lines and return a
     list of user agents and a multi-valued hash table containing the
     rules for each user agent. *)
  let parse_robots =
    let module S = Set.Make(struct
                              type t = string
                              let compare = compare
                            end) in
  
    (* Precompile regular expressions. *)
    let comments = Str.regexp "#.*" in
    let leading_white = Str.regexp "^[ \t]+" in
    let trailing_white = Str.regexp "[ \t\r]+$" in
    let colon_delim = Str.regexp "[ \t]*:[ \t]*" in
  
    fun stream ->
      let user_agent = ref "*" in
      let user_agents = ref (S.singleton "*") in
      let rules = Hashtbl.create 0 in
  
      Stream.iter
        (fun s ->
           let s = Str.replace_first comments "" s in
           let s = Str.replace_first leading_white "" s in
           let s = Str.replace_first trailing_white "" s in
           if String.length s > 0 then
             match Str.bounded_split_delim colon_delim s 2 with
               | ["User-agent"; value] ->
                   (* Found a new User-agent. *)
                   user_agent := value;
                   user_agents := S.add value !user_agents
               | ["Sitemap"; value] ->
                   (* Sitemaps are always global. *)
                   Hashtbl.add rules "*" ("Sitemap", value)
               | [key; value] ->
                   (* Found a rule for the current User-agent. *)
                   Hashtbl.add rules !user_agent (key, value)
               | _ -> failwith s)
        stream;
      S.elements !user_agents, rules
  
  (* Produce a stream of lines from an input channel. *)
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
  (* Produce a stream of lines from a string in memory. *)
  let line_stream_of_string string =
    Stream.of_list (Str.split (Str.regexp "\n") string)
  
  (*-----------------------------*)
  
  (* Use Ocamlnet to retrieve a "robots.txt" file and print its rules. *)
  
  #use "topfind";;
  #require "netclient";;
  open Http_client.Convenience
  
  let agents, rules =
    parse_robots
      (line_stream_of_string
         (http_get "http://sourceforge.net/robots.txt"))
  
  let () =
    List.iter
      (fun agent ->
         Printf.printf "User-agent: %s\n" agent;
         List.iter
           (fun (key, value) ->
              Printf.printf "\t%s: %s\n" key value)
           (Hashtbl.find_all rules agent))
      agents
  

