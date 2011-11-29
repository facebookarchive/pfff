(*---------------------------------------------------------------------------*)
(* cound macro as id ? *)
let typing_stat_macro_not_id = ref true

let verbose_typing_stat = ref false



let typing_stat_ast ast db h_notfound_type_ident h_notfound_type_field = 

  let good, bad = ref 0, ref 0 in
  let good_id, bad_id = ref 0, ref 0 in
  let good_fld, bad_fld = ref 0, ref 0 in
  let good_funcall, bad_funcall = ref 0, ref 0 in

  let hooks = {V.default_visitor with
    V.kexpr = (fun (k, bigf) expr -> 
      
      let as_type_info =
        match Ast_php.get_type expr with
        | [Type_php.Unknown] -> false
        | _ -> true
      in
      
      if as_type_info 
      then incr good
      else incr bad
      ;

      if not as_type_info && !verbose_typing_stat
      then pr (Export_ast_php.sexp_string_of_expr expr);


      (*
      (match Ast_c.unwrap_expr expr with
      | Ast_c.Ident name -> 
          let s = Ast_c.str_of_name name in

          if (!typing_stat_macro_not_id && 
              s ==~ Parsing_hacks.regexp_macro) || is_define
          then ()
          else
            if as_type_info 
            then incr good_id
            else begin 
              (* if not is_define then pr ("no type ident: " ^ s);*)

              if DbQ.can_find_single_def_ident_global s db && false
              then ()
              else begin 
                Common.hupdate_default s
                  (fun old -> old + 1) (fun() -> 0) h_notfound_type_ident;

                incr bad_id;
              end
            end

      | RecordAccess  (e, namefld)
      | RecordPtAccess (e, namefld) -> 
          let fld = str_of_name namefld in
            if as_type_info 
            then incr good_fld
            else begin
              if is_define 
              then ()
              else 
              (match Ast_c.get_onlytype_expr e with
              | None -> 
                  Common.hupdate_default fld
                  (fun old -> old + 1) (fun() -> 0) h_notfound_type_field;

                  (*if not is_define then pr ("no type field: " ^ fld);*)
                  incr bad_fld

              | Some ft -> 
                  if DbQ.can_find_single_def_struct_global ft db && false
                  then () 
                  else begin 
                    Common.hupdate_default fld
                      (fun old -> old + 1) (fun() -> 0) h_notfound_type_field;
                    incr bad_fld;
                  end
              )
            end

      | Ast_c.FunCall (e,es) -> 
          if as_type_info 
          then incr good_funcall
          else incr bad_funcall
          

      | _ -> ()
      );
      *)

      (* recurse *)
      k expr;

    );
  }
  in 
  (V.mk_visitor hooks)  (Toplevel ast);

  (!good, !bad), 
  (!good_id, !bad_id), (!good_fld, !bad_fld),
  (!good_funcall, !bad_funcall)


(* Normally those stats are quire proportianal to the correctness of 
 * type_annotater and the good flags for cpp_ast_php so go deep enough to
 * have enough symbol table information.
 * 
 * On www (with regexp avoidance stat):
 * ???
 * 
 * todo? count only unique id ? because if ident repeat 100 times 
 * then get 100 typing stat error.
 *)
let typing_stat_db db = 

  let good_total, bad_total = ref 0, ref 0 in
  let good_id_total, bad_id_total = ref 0, ref 0 in
  let good_fld_total, bad_fld_total = ref 0, ref 0 in
  let good_funcall_total, bad_funcall_total = ref 0, ref 0 in

  let numperfect = ref 0 in

  let max_with_elem = ref 0, ref "" in

  let h_notfound_type_ident = Hashtbl.create 101 in
  let h_notfound_type_field = Hashtbl.create 101 in

  db.file_to_topids#tolist +> Common.index_list_and_total +> 
  (fun xs -> Common.execute_and_show_progress (List.length xs) (fun k -> 
   xs +> List.iter
    (fun ((file, ids), i, total) -> 
      (*pr2 (spf "ANALYZING: %s %d/%d " file i total);*)
      k();

      let is_perfect = ref true in

      ids +> List.iter (fun id -> 
        let ast = db.Db.defs.Db.toplevels#assoc id in

        let ((good,bad), 
            (good_id,bad_id), (good_fld,bad_fld),
            (good_funcall, bad_funcall)
           ) = 
          typing_stat_ast ast db   
            h_notfound_type_ident
            h_notfound_type_field
        in
        good_total += good;
        bad_total += bad;
        good_id_total += good_id;
        bad_id_total += bad_id;
        good_fld_total += good_fld;
        bad_fld_total += bad_fld;
        good_funcall_total += good_funcall;
        bad_funcall_total += bad_funcall;
        
        if bad_id > 0 || bad > 0 || bad_fld > 0 then is_perfect := false;

        Common.update_max_with_elem max_with_elem
          ~is_better:(fun newi old -> newi > !old) (bad, file);

      );
      if !is_perfect then incr numperfect;
    )));
  let percent = 
    Common.pourcent_good_bad_float !good_total !bad_total in
  let percent_id = 
    Common.pourcent_good_bad_float !good_id_total !bad_id_total in
  let percent_fld = 
    Common.pourcent_good_bad_float !good_fld_total !bad_fld_total in
  let percent_funcall = 
    Common.pourcent_good_bad_float !good_funcall_total !bad_funcall_total in

  pr2 ("typing stat");

  pr2 ("-- most problematic ident --");
  Common.hash_to_list h_notfound_type_ident 
  +> Common.sort_by_val_highfirst
  +> Common.take_safe 10
  +> List.iter (fun (k,i) -> 
    pr2 (spf "%s: %d" k i);
  );
  pr2 ("-- most problematic field --");
  Common.hash_to_list h_notfound_type_field
  +> Common.sort_by_val_highfirst
  +> Common.take_safe 10
  +> List.iter (fun (k,i) -> 
    pr2 (spf ".%s: %d" k i);
  );

  pr2 ("--");
  pr2 (spf "perfect = %d" !numperfect);
  pr2 (spf "nb good = %d, nb bad = %d, %02.2f%%"
          !good_total !bad_total percent);
  pr2 (spf "nb good fld = %d, nb bad fld = %d, %02.2f%%"  
          !good_fld_total !bad_fld_total percent_fld);
  pr2 (spf "nb good funcall = %d, nb bad funcall = %d, %02.2f%%"  
          !good_funcall_total !bad_funcall_total percent_funcall);
  pr2 (spf "nb good id = %d, nb bad id = %d, %02.2f%%"  
          !good_id_total !bad_id_total percent_id);
  pr2 (spf "max: %d %s" (!(fst max_with_elem)) (!(snd max_with_elem)));

  ()



  "-typing_stat_db", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname typing_stat_db);
