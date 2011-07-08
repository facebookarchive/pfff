
    (* actions *)
    (* 
     * let cleaner = !tokens2 +> filter_pp_or_comment_stuff in
     * let paren_grouped = TV.mk_parenthised  cleaner in
     * Parsing_hacks_pp.find_actions  paren_grouped;
     * 
     *)


(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let rec find_actions = function
  | [] -> ()

  | PToken ({t=TIdent (s,ii)})
    ::Parenthised (xxs,info_parens)
    ::xs -> 
      find_actions xs;
      xxs +> List.iter find_actions;
      let modified = find_actions_params xxs in
      if modified 
      then msg_macro_higher_order s
        
  | x::xs -> 
      find_actions xs

and find_actions_params xxs = 
  xxs +> List.fold_left (fun acc xs -> 
    let toks = tokens_of_paren xs in
    if toks +> List.exists (fun x -> TH.is_statement x.t)
    then begin
      xs +> iter_token_paren (fun x -> 
        change_tok x (TAny_Action (TH.info_of_tok x.t));
      );
      true (* modified *)
    end
    else acc
  ) false
