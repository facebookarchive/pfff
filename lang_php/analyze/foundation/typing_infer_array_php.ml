open Env_typing_php 
module THP = Typing_helpers_php
module APS = Ast_php_simple

module Array_typer = struct

  type evidence = 
    | SingleLine of Parse_info.info option(*When a single line of evidence is enough,
    ex a no index access indicating a vector*)
    | DoubleLine of Parse_info.info option * Env_typing_php.t *
    Parse_info.info option * Env_typing_php.t (*When two conflicting
    lines are needed, ex inserting two different types*)

  type container_evidence = {
    supporting: evidence list;
    opposing: evidence list
  }

  type container = 
    | Vector of Env_typing_php.t
    | Tuple (*of Env_typing_php.t * Env_typing_php.t*)
    | Map (*of Env_typing_php.t * Env_typing_php.t*)
    | NoData (*Indicates insufficient data*)
    | NoGuess (*indicates conflicting data*)

  type declaration = 
    | DKeyValue
    | DValue
    | NoDec

  type inferred_container = {
    map: container_evidence;
    tuple: container_evidence; 
    vector: container_evidence;
    guess: container;
    confused: bool;
    mixed_val_ty: bool;
    key_t: Env_typing_php.t option;
    value_t: Env_typing_php.t option;
    declaration: declaration
  }

  type evidence_type = 
    | Supporting
    | Opposing

  let make_container_evidence = {
    supporting = [];
    opposing = [];
  }

  let compose_ce s o = {
    supporting = s;
    opposing = o
  }

  let add_evidence ce et e = 
    let {supporting = s; opposing = o} = ce in
    match et with 
    | Supporting -> let s = e::s in
      compose_ce s o
    | Opposing -> let o = e::o in
      compose_ce s o

  let make_inferred_container = {
    map = make_container_evidence;
    tuple = make_container_evidence;
    vector = make_container_evidence;
    guess = NoData;
    confused = false;
    mixed_val_ty = false;
    key_t = None;
    value_t = None;
    declaration = NoDec
  }

  let compose_ic m t v g c mt k va d = {
    map = m;
    tuple = t;
    vector = v;
    guess = g;
    confused = c;
    mixed_val_ty = mt;
    key_t = k;
    value_t = va;
    declaration = d
  }

  type cont_evi = container * int list

  type val_evi = Env_typing_php.t * evidence

  type t = {
    (*unused as of yet, potentially unneeded if pass parse_info for line info*)
    file: string;
    (*intermediate structure - contains list of value types associated with the
     * array, and a list of line numbers NOTE: need to change to parse_info*)
    values: val_evi list AMap.t ref;
    (*Final inferred type confidences*)
    inferred: inferred_container AMap.t ref
  }

  let make_array_typer f = {
    file = f;
    values = ref AMap.empty;
    inferred = ref AMap.empty;
  }

  let fun_on_aenv env at f =
    THP.AEnv.iter env (
      fun x l -> f at x l;)
 
  let print_keys env =
    THP.AEnv.iter env (
      fun x l ->
      match x with
      | (e, f, c) -> ( let stmt = APS.Expr (e) in
        let v = Meta_ast_php_simple.vof_program [stmt] in
        let s = Ocaml.string_of_v v in
        Common.pr s;)
    )

  let line_of_array_info = function
    | (None, _) -> (-1)
    | (Some (pi), _) -> Parse_info.line_of_info pi

  let pp_arr_id id = 
    match id with 
    | (e, f, c) -> 
        Printf.printf "In %s %s, " c f;
        let stmt = APS.Expr(e) in
        let v = Meta_ast_php_simple.vof_program [stmt] in
        let s = Ocaml.string_of_v v in
        Common.pr s

  let string_equ t1 t2 = 
    match t1, t2 with 
    | Tsum[Tsstring _], Tsum[Tsstring _] 
    | Tsum[Tsstring _], Tsum[Tabstr "string"]
    | Tsum[Tabstr "string"], Tsum[Tsstring _] -> true
    | Tsum[Tabstr s1], Tsum[Tabstr s2] when s1 = s2 -> true
    | Tvar v1, Tvar v2 when v1 = v2 -> true
    | _, _ -> false

  let apply_subst env x = 
    match x with 
    | Tsum _ -> x
    | Tvar v -> THP.TEnv.get env v

  let analyze_noindex env at id pi v =
    let v = apply_subst env v in
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in
    let {map = m; tuple = t; vector = vector; guess = guess; confused =
      confused; mixed_val_ty = mixed_val_ty; key_t = key; value_t = value; declaration = dec} = ic in
    let e = SingleLine(pi) in
    let m = add_evidence m Opposing e in
    let t = add_evidence t Opposing e in
    let k = Tsum[Tabstr "int"] in
    match key, value with
    | Some(key_t), _ when key_t <> k && not (string_equ key_t k) ->
      let ic = compose_ic m t vector guess true true key value dec in
      at.inferred := AMap.add id ic !(at.inferred)
    | _, _ when mixed_val_ty ->
      let ic = compose_ic m t vector guess true true key value dec in
      at.inferred := AMap.add id ic !(at.inferred)
    | _, Some(va) when (va = v || string_equ va v) -> 
      let vector = add_evidence vector Supporting e in
      let guess = Vector(v) in
      let ic = compose_ic m t vector guess confused mixed_val_ty key value dec in
      at.inferred := AMap.add id ic !(at.inferred)
    | _, Some(va) -> 
      let ic = compose_ic m t vector guess true true key value dec in
      at.inferred := AMap.add id ic !(at.inferred)
    | _, None -> 
      let value = Some(v) in
      let key = Some(k) in
      let vector = add_evidence vector Supporting (SingleLine(pi)) in
      let ic = compose_ic m t vector (Vector(v)) confused mixed_val_ty key value dec in
      at.inferred := AMap.add id ic !(at.inferred)
    

  let rec contains_type t l = 
    match l with 
    | [] -> None
    | (y, pi)::xs when (y <> t && not (string_equ y t))-> Some(y, pi)
    | x::xs -> contains_type t xs

  let prev_conflicting_val at id v pi =
    let e = SingleLine(pi) in
    let ve = try AMap.find id !(at.values) with Not_found -> [] in
    let ct = contains_type v ve in
    if (List.length ve = 0) || (List.length ve = 1 && ct = None) 
    then (let ve = (v, e)::ve in 
      at.values := AMap.add id ve !(at.values);
      None) 
    else (let ve = (v, e)::ve in 
      at.values := AMap.add id ve !(at.values);
      ct)

  let pi_of_evidence evi = 
    match evi with 
    | SingleLine(pi) -> pi
    | DoubleLine(pi, _, _, _) -> pi

  let analyze_value env at id pi v =
    let v = apply_subst env v in
    match prev_conflicting_val at id v pi with
    | None -> ()
    | Some(t, tpi) -> 
      let tpi = pi_of_evidence tpi in
      let evi = DoubleLine(pi, v, tpi, t) in
      let ic = try AMap.find id !(at.inferred) with Not_found ->
        make_inferred_container in 
      let {map = map; tuple = tuple; vector = vector; guess = guess; confused =
        con; mixed_val_ty = mt; key_t = key; value_t = value; declaration = dec} = ic in
      let map = add_evidence map Opposing evi in 
      let vector = add_evidence vector Opposing evi in 
      let mt = true in 
      let ic = compose_ic map tuple vector guess con mt key value dec in 
      at.inferred := AMap.add id ic !(at.inferred)

  let print_val_types env va_t v = 
    let penv = Pp2.empty print_string in
    THP.Print2.ty env penv ISet.empty 0 va_t;
    THP.Print2.ty env penv ISet.empty 0 v;
    match va_t, v with 
    | Tsum _, Tsum _  -> Printf.printf "Tsums\n"
    | Tvar v1, Tvar v2 -> Printf.printf "Tvars %d and %d\n" v1 v2
    | Tsum _, Tvar _  -> Printf.printf "v is a var\n"
    | Tvar _, Tsum _ -> Printf.printf "va_t is a var\n"

  let analyze_declaration_value env at id pi v = 
    let v = apply_subst env v in
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in
    let {map = m; tuple = t; vector = ve; guess = g; confused = c; mixed_val_ty
      = mt; key_t = key; value_t = va; declaration = d} = ic in
    let e = SingleLine(pi) in
    let k = Tsum[Tabstr "int"] in
      match d with 
      | DKeyValue -> 
        let ic = compose_ic m t ve NoGuess true mt key va DValue in
        at.inferred := AMap.add id ic !(at.inferred)

      | DValue -> (match key, va with 
        | None, None -> 
          let key = Some (k) in
          let va = Some(v) in
          let ve = add_evidence ve Supporting e in
          let m = add_evidence m Opposing e in 
          let t = add_evidence t Supporting e in
          let ic = compose_ic m t ve g c mt key va DValue in
          at.inferred := AMap.add id ic !(at.inferred)

        | Some(key_t), Some(ty) when (ty = v || string_equ ty v) && (key_t = k
          || string_equ key_t k) -> 
          let ve = add_evidence ve Supporting e in
          let m = add_evidence m Opposing e in 
          let t = add_evidence t Supporting e in
          let ic = compose_ic m t ve g c mt key va DValue in
          at.inferred := AMap.add id ic !(at.inferred)
      
        | Some(key_t), Some (va_t) when key_t = k || string_equ key_t k ->
          let m = add_evidence m Opposing e in
          let ve = add_evidence ve Opposing e in
          let t = add_evidence t Supporting e in
          let ic = compose_ic m t ve Tuple c true key va DValue in
          at.inferred := AMap.add id ic !(at.inferred)

        | _, _ -> 
          let ic = compose_ic m t ve g true mt key va d in
          at.inferred := AMap.add id ic !(at.inferred)
      )

      | NoDec -> 
        let key = Some (Tsum[Tabstr "int"] ) in
        let va = Some(v) in
        let g = Vector v in
        let e = SingleLine(pi) in
        let m = add_evidence m Opposing e in
        let t = add_evidence t Supporting e in
        let ve = add_evidence ve Supporting e in
        let ic = compose_ic m t ve g c mt key va DValue in
        at.inferred := AMap.add id ic !(at.inferred)

  let analyze_declaration_kvalue env at id pi k v  = 
    let k = apply_subst env k in
    let v = apply_subst env v in
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in
    let {map = m; tuple = t; vector = ve; guess = g; confused = c; mixed_val_ty
      = mt; key_t = key; value_t = va; declaration = d} = ic in
      let e = SingleLine(pi) in
      match d with 
      | DValue -> 
        let ic = compose_ic m t ve NoGuess true mt key va DKeyValue in
        at.inferred := AMap.add id ic !(at.inferred)
    
      | DKeyValue
      | NoDec -> ( 
        match key, va with 
        | Some(key_t), Some(va_t) when (key_t = k || string_equ key_t k) &&
        (va_t = v || string_equ va_t v)-> 
          let m = add_evidence m Supporting e in
          let ve = add_evidence ve Opposing e in
          let t = add_evidence t Opposing e in
          let ic = compose_ic m t ve Map c mt key va DKeyValue in 
          at.inferred := AMap.add id ic !(at.inferred)
        
        | None, None -> 
          let key = Some(k) in
          let va = Some(v) in
          let m = add_evidence m Supporting e in
          let ve = add_evidence ve Opposing e in
          let t = add_evidence t Opposing e in
          let ic = compose_ic m t ve Map c mt key va DKeyValue in
          at.inferred := AMap.add id ic !(at.inferred)

        | _, _ -> 
          let ic = compose_ic m t ve NoGuess true mt key va DKeyValue in 
          at.inferred := AMap.add id ic !(at.inferred)
      )

  let analyze_nonstring_access env at id pi k v = 
    let k = apply_subst env k in 
    let v = apply_subst env v in
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in 
    let {map = m; tuple = t; vector = ve; guess = g; confused = c; mixed_val_ty
      = mt; key_t = key; value_t = va; declaration = d} = ic in
      let e = SingleLine(pi) in
      match key, va with 
      | Some(key_t), _ when (key_t <> k && not (string_equ key_t k))->
        Printf.printf "Keys don't match!\n";
        let ic = compose_ic m t ve NoGuess true mt key va d in
        at.inferred := AMap.add id ic !(at.inferred)
      | Some(key_t), Some(va_t) when (key_t = k || string_equ key_t k) && k <>
        Tsum[Tabstr "int"] && (va_t = v || string_equ va_t v)-> 
        let m = add_evidence m Supporting e in 
        let ve = add_evidence ve Opposing e in
        let t = add_evidence t Opposing e in
        let ic = compose_ic m t ve Map c mt key va d in
        at.inferred := AMap.add id ic !(at.inferred)
      | Some(key_t), Some(va_t) when (key_t = k || string_equ key_t k) && k =
        Tsum[Tabstr "int"] && (va_t = v || string_equ va_t v) -> 
          () (*This case tells me nothing, leave all as is*)
      | Some(Tsum[Tabstr "int"]), Some(va_t) when (va_t <> v && not (string_equ
      va_t v)) ->
        Printf.printf "Values not equal\n";
        let m = add_evidence m Opposing e in
        let ve = add_evidence ve Opposing e in 
        let t = add_evidence t Supporting e in
        let ic = compose_ic m t ve Tuple c true key va d in
        at.inferred := AMap.add id ic !(at.inferred)
      | None, None -> (*also doesn't really tell me anything*)
          Printf.printf "Hitting none\n";
          let key = Some(k) in 
        let va = Some(v) in
        let ic = compose_ic m t ve g c mt key va d in
        at.inferred := AMap.add id ic !(at.inferred)
      | _, _ ->
          let ic = compose_ic m t ve g c mt key va d in
          at.inferred := AMap.add id ic !(at.inferred)

  let analyze_string_access env at id pi v = 
    let v = apply_subst env v in 
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in
    let {map = m; tuple = t; vector = ve; guess = g; confused = c; mixed_val_ty
      = mt; key_t = key; value_t = va; declaration = d} = ic in
    let e = SingleLine(pi) in
    let t = add_evidence t Opposing e in 
    let ve = add_evidence ve Opposing e in
    let k = Tsum[Tabstr "string"] in 
    match key, va with 
    | None, None -> 
      Printf.printf "None access\n";
      let key = Some(k) in
      let va = Some(v) in 
      let m = add_evidence m Supporting e in
      let ic = compose_ic m t ve Map c mt key va d in
      at.inferred := AMap.add id ic !(at.inferred)
    | Some(key_t),  Some(va_t) when (va_t = v || string_equ va_t v) && (key_t =
      k || string_equ key_t k) -> 
      Printf.printf "keys and vlas are the same...\n";
      let m = add_evidence m Supporting e in
      let ic = compose_ic m t ve Map c mt key va d in
      at.inferred := AMap.add id ic !(at.inferred)
    | _, _ -> 
      let m = add_evidence m Opposing e in
      let ic = compose_ic m t ve g true true key va d in 
      at.inferred := AMap.add id ic !(at.inferred)

  let initialize_inferred_container at id = 
    if not (AMap.mem id !(at.inferred)) then 
      let ic = make_inferred_container in 
      at.inferred := AMap.add id ic !(at.inferred)

  let analyze_access_info env at id ail = 
    List.iter (fun ai -> 
      match ai with
      | (pi, NoIndex (v)) -> analyze_noindex env at id pi v
      | (pi, VarOrInt (k, v)) -> analyze_nonstring_access env at id pi k v
      | (_, Const _) -> initialize_inferred_container at id
      | (pi, ConstantString v) -> analyze_string_access env at id pi v
      | (pi, DeclarationValue v) -> analyze_declaration_value env at id pi v
      | (pi, DeclarationKValue (k, v)) -> analyze_declaration_kvalue env at id pi k v
      | (_, Declaration _ ) -> initialize_inferred_container at id
      | (pi, Value v) -> analyze_value env at id pi v
      | (_, UnhandledAccess) -> initialize_inferred_container at id
    ) ail

  let analyze_accesses_values env at = 
    THP.AEnv.iter env (fun id ail -> 
      analyze_access_info env at id ail;
    )
  
  let infer_arrays env at =
    Printf.printf "Inferring arrays ...\n";
    analyze_accesses_values env at
  
  (* x is the id, l is the list of arr_info (parse_info option * arr_access )*)
  let string_of_container = function
    | Vector _ -> "vector"
    | Tuple -> "tuple"
    | Map -> "map"
    | NoData -> "no data"
    | NoGuess -> "conflicting data exists"

  let rec pp_evidence e = 
    List.iter (fun x ->
      Printf.printf "    %d\n" x; 
      )
    e

  let pp_cont_evi x =
    match x with
    | (c, e) -> begin Printf.printf "  %s\n" (string_of_container c);
    pp_evidence e; end

  let pp_evidence e = 
    match e with 
    | SingleLine (Some pi) -> Printf.printf "    Evidence at line %d in file %s\n"
    (Parse_info.line_of_info pi) (Parse_info.file_of_info pi)
    | DoubleLine (Some pi1, e1, Some pi2, e2) -> Printf.printf "    Evidence at
      %d in %s and \n %d in %s\n"
      (Parse_info.line_of_info pi1) (Parse_info.file_of_info pi1)
      (Parse_info.line_of_info pi2) (Parse_info.file_of_info pi2)
    | _ -> Printf.printf "    Evidence unavailable"

  let rec pp_evidence_l cel = 
    match cel with 
    | [] -> ()
    | x::xs -> pp_evidence x; pp_evidence_l xs

  let pp_confused_reasoning ic = 
    let {map = m; tuple = t; vector = v; guess = guess; confused = c;
      mixed_val_ty = _; key_t = _; value_t = _; declaration = _} = ic in
    let {supporting = _; opposing = o} = m in
    Printf.printf "    Not a map due to:\n";
    pp_evidence_l o;
    let {supporting = _; opposing = o} = t in
    Printf.printf "    Not a tuple due to:\n";
    pp_evidence_l o;
    let {supporting = _; opposing = o} = v in
    Printf.printf "    Not a vector due to:\n";
    pp_evidence_l o
    

  let pp_reasoning ic = 
    let {map = m; tuple = t; vector = v; guess = guess; confused = c;
      mixed_val_ty = _; key_t = _; value_t = _; declaration = _} = ic in
      match guess with
      | Map ->
        let {supporting = s; opposing = _} = m in
        pp_evidence_l s
      | Tuple -> 
        let {supporting = s; opposing = _} = t in 
        pp_evidence_l s
      | Vector _ -> 
        let {supporting = s; opposing = _} = v in
        pp_evidence_l s
      | _ -> ()

  let pp_array_guesses at =
    Printf.printf "inferred arrays: %d\n" (AMap.cardinal !(at.inferred));
    AMap.iter (fun id ic ->
      pp_arr_id id;
      let {map = _; tuple = _; vector = _; guess = guess; confused = c;
        mixed_val_ty = _; key_t = _; value_t = _; declaration = _} = ic in
      match c with 
      | true -> 
          Printf.printf "  conflicting data exists\n";
          pp_confused_reasoning ic
      | false -> 
          Printf.printf "  %s\n" (string_of_container guess);
          pp_reasoning ic
    ) !(at.inferred)

end
