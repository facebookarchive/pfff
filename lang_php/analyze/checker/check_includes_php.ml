
module IR = Include_require_php

let check_include_require db = 

  let env = Www.mk_env root in

  (* regular include/require *)

  let increqs = IR.all_increq_of_any (Program [ast]) in
  increqs |> List.iter (fun (inckind, tok, incexpr) ->
    try (
      let path_opt = IR.resolve_path (env,Filename.dirname file) incexpr in

      match path_opt with
      | Some path ->
          if not (Sys.file_exists path)
          then begin
            pr2 (spf "BUG: PHP file not found: %s in %s" path file);
            incr total;
          end
            
      | None -> 
          pr2 (spf "(not a static include/require %s)" 
                  (Ast.string_of_info tok));
          ) 
    with exn -> 
      pr2 "PB: treating a include/require";
      Lib_parsing_php.print_match [tok];
      raise exn
  );
