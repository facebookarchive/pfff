open Env_typing_php 
module THP = Typing_helpers_php
module APS = Ast_php_simple
module PI = Parse_info

module Array_typer = struct

exception Fun_def_error

(*****************************************************************************)
(* Datastructure *)
(*****************************************************************************)

  type evidence = 
    | SingleLine of PI.info option(*When a single line of evidence is enough,
    ex a no index access indicating a vector*)
    | DoubleLine of PI.info option * Env_typing_php.t *
    PI.info option * Env_typing_php.t (*When two conflicting
    lines are needed, ex inserting two different types*)

  type container_evidence = {
    supporting: evidence list;
    opposing: evidence list
  }

  type container = 
    | Vector of Env_typing_php.t
    | Tuple (*of Env_typing_php.t * Env_typing_php.t*)
    | Map of Env_typing_php.t * Env_typing_php.t
    | NoData (*Indicates insufficient data*)
    | NoGuess (*indicates conflicting data*)
    | NotArray (* May be present in a return type*)

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
    declaration: declaration;
    parameter: bool;
    dec_loc: PI.info option list;
    arr_loc: PI.info option list;
    return_val: bool;
    fdef_loc: PI.info option; 
    tup_ty_l: Env_typing_php.t list;
    manual_patch_ty: string;
  }

  type val_evi = Env_typing_php.t * evidence

  type patch = int * string (* line number * new line text *)

  type t = {
    (* intermediate structure - contains list of value types associated with the
     * array, and a list of line numbers *)
    values: val_evi list AMap.t ref;
    (* Final inferred type confidences*)
    inferred: inferred_container AMap.t ref;
    (* Map to store patches for the file associated with the key *)
    patches: patch list SMap.t ref
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

  let compare_evidence e1 e2 = 
    match e1, e2 with 
    | SingleLine(Some(p1)), SingleLine(Some(p2)) 
        when (PI.file_of_info p1)=(PI.file_of_info p2)-> (
      let l1 = PI.line_of_info p1 in 
      let l2 = PI.line_of_info p2 in
      match l1, l2 with 
      | _, _ when l1 < l2 -> (-1)
      | _, _ when l1 > l2 -> 1
      | _, _ -> 0
    )
    | DoubleLine(Some(p11), _, Some(p12), _), DoubleLine(Some(p21), _, Some(p22), _)
        when (PI.file_of_info p11) = (PI.file_of_info p21) -> (
      let l11 = PI.line_of_info p11 in
      let l12 = PI.line_of_info p12 in
      let l21 = PI.line_of_info p21 in
      let l22 = PI.line_of_info p22 in
      match (l11, l12, l21, l22) with
      | _ when l11 = l21 && l12 = l22 -> 0
      | _ when l11 < l21 -> (-1) 
      | _ when l11 > l21 -> 1 
      | _ when l12 < l22 -> (-1) 
      | _ -> 1
    )
    | _ -> -1

  let rec prune_evidence l = 
   match l with 
   | [] -> []
   | x::xs -> 
       let pl = List.filter 
         (fun t -> if (compare_evidence x t) = 0 then false else true) xs in
       x::(prune_evidence pl)


  let rec prune_arr_loc l = 
    match l with 
    | [] -> []
    | x::xs -> 
        let pl = List.filter
          (fun t -> (x <> t)) xs in 
        x::(prune_arr_loc pl)

  let rec pp_arr_loc l = 
    match l with 
    | [] -> ()
    | Some(x)::xs -> 
        Printf.printf "  In %s on %d at %d\n" (PI.file_of_info x)
        (PI.line_of_info x) (PI.col_of_info x);
        pp_arr_loc xs
    | None::xs -> pp_arr_loc xs

  let make_inferred_container = {
    map = make_container_evidence;
    tuple = make_container_evidence;
    vector = make_container_evidence;
    guess = NotArray;
    confused = false;
    mixed_val_ty = false;
    key_t = None;
    value_t = None;
    declaration = NoDec;
    parameter = false;
    dec_loc = [];
    arr_loc = [];
    return_val = false;
    fdef_loc = None;
    tup_ty_l = [];
    manual_patch_ty = ""
  }

  let make_array_typer = {
    values = ref AMap.empty;
    inferred = ref AMap.empty;
    patches = ref SMap.empty;
  }

  let pi_of_evidence evi = 
    match evi with 
    | SingleLine(pi) -> pi
    | DoubleLine(pi, _, _, _) -> pi
  
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

  let pp_arr_id id = 
    match id with 
    | (e, f, c) -> 
      Printf.printf "In %s %s, " c f;
      let stmt = APS.Expr(e) in
      let v = Meta_ast_php_simple.vof_program [stmt] in
      let s = Ocaml.string_of_v v in
      Common.pr s

(*****************************************************************************)
(* Array Inference Helpers *)
(*****************************************************************************)

  let rec check_id_in_list id l = 
    match l with 
    | [] -> false
    | x::xs when x = id -> true
    | x::xs -> check_id_in_list id xs

  let is_parameter env id =
    let params = !(THP.AEnv.get_params env) in
    check_id_in_list id params

  let rec get_fun_pil f c fi l = 
    match l with 
    | [] -> None
    | (tf, tc, pi)::xs when f = tf && tc = c && (fi = PI.file_of_info pi) ->
      Some(pi)
    | x::xs -> get_fun_pil f c fi xs

  let get_fun_pi env f c fi = 
    let fids = THP.AEnv.get_funs env in
    get_fun_pil f c fi fids

  let set_fdef env id pi ic= 
    match id, pi with 
    | (_, f, c), Some p  -> (
      let fdef = get_fun_pi env f c (PI.file_of_info p) in
      let ic = {ic with fdef_loc = fdef} in 
      ic
    )
    | _ -> ic
  

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

(*****************************************************************************)
(* Array Inference Analysis *)
(*****************************************************************************)

  let analyze_noindex env at id pi v =
    let v = apply_subst env v in
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in
    let ic = set_fdef env id pi ic in
    let {map = m; tuple = t; vector = ve; key_t = key; value_t = va;
    mixed_val_ty = mt; arr_loc = al; _} = ic in
    let p = is_parameter env id in
    let e = SingleLine(pi) in
    let m = add_evidence m Opposing e in
    let t = add_evidence t Opposing e in
    let k = Tsum[Tabstr "int"] in
    let ic = {ic with arr_loc = pi::al; map = m; tuple = t; parameter = p} in
    match key, va with
    | Some(key_t), _ when key_t <> k && not (string_equ key_t k) ->
      let ic = {ic with confused = true; mixed_val_ty = true} in
      at.inferred := AMap.add id ic !(at.inferred)
    | _, _ when mt ->
      let ic = {ic with confused = true} in
      at.inferred := AMap.add id ic !(at.inferred)
    | _, Some(va) when (va = v || string_equ va v) -> 
      let ve = add_evidence ve Supporting e in
      let ic = {ic with vector = ve; guess = (Vector(v))} in
      at.inferred := AMap.add id ic !(at.inferred)
    | _, Some(va) -> 
      let ic = {ic with confused = true; mixed_val_ty = true} in
      at.inferred := AMap.add id ic !(at.inferred)
    | _, None -> 
      let ve = add_evidence ve Supporting (SingleLine(pi)) in
      if (v <> Tsum[]) && (k <> Tsum[]) then (
        let ic = {ic with guess = (Vector(v)); vector = ve; key_t = key; value_t = va} in
        at.inferred := AMap.add id ic !(at.inferred))
      else
        let ic = {ic with guess = (Vector(v)); vector = ve} in
        at.inferred := AMap.add id ic !(at.inferred)
    
  let analyze_value env at id pi v =
    let v = apply_subst env v in
    match prev_conflicting_val at id v pi with
    | None -> ()
    | Some(t, tpi) -> 
      let tpi = pi_of_evidence tpi in
      let evi = DoubleLine(pi, v, tpi, t) in
      let ic = try AMap.find id !(at.inferred) with Not_found ->
        make_inferred_container in 
      let {arr_loc = al; map = map; vector = vector; _ } = ic in
      let p = is_parameter env id in 
      let map = add_evidence map Opposing evi in 
      let vector = add_evidence vector Opposing evi in 
      let ic = {ic with parameter = p; map = map; vector = vector; mixed_val_ty
        = true; arr_loc = pi::al} in
      at.inferred := AMap.add id ic !(at.inferred)

  let analyze_declaration_value env at id pi v = 
    let v = apply_subst env v in
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in
    let ic = set_fdef env id pi ic in
    let {map = m; tuple = t; vector = ve; key_t = key; value_t = va; declaration 
    = d; arr_loc = al; _} = ic in
    let e = SingleLine(pi) in
    let p = is_parameter env id in
    let ic = {ic with arr_loc = pi::al; parameter = p} in
    let k = Tsum[Tabstr "int"] in
      match d with 
      | DKeyValue ->
        let ic = {ic with guess = NoGuess; confused = true; declaration =
          DValue} in
        at.inferred := AMap.add id ic !(at.inferred)

      | DValue -> (
        match key, va with 
        | None, None -> 
          let key = Some (k) in
          let va = Some(v) in
          let ve = add_evidence ve Supporting e in
          let m = add_evidence m Opposing e in 
          let t = add_evidence t Supporting e in
          if (v <> Tsum[]) && (k <> Tsum[]) then (
            let ic = {ic with key_t = key; value_t = va; vector = ve; map = m;
              tuple = t; declaration = DValue} in
            at.inferred := AMap.add id ic !(at.inferred))
          else
            let ic = {ic with vector = ve; map = m; tuple = t; declaration = DValue} in
            at.inferred := AMap.add id ic !(at.inferred)

        | Some(key_t), Some(ty) when (ty = v || string_equ ty v) && (key_t = k
          || string_equ key_t k) -> 
          let ve = add_evidence ve Supporting e in
          let m = add_evidence m Opposing e in 
          let t = add_evidence t Supporting e in
          let ic = {ic with vector = ve; map = m; tuple = t; declaration =
            DValue} in
          at.inferred := AMap.add id ic !(at.inferred)
      
        | Some(key_t), Some (va_t) when key_t = k || string_equ key_t k ->
          let m = add_evidence m Opposing e in
          let ve = add_evidence ve Opposing e in
          let t = add_evidence t Supporting e in
          let ic = {ic with map = m; vector = ve; tuple = t; guess = Tuple;
            mixed_val_ty = true; declaration = DValue} in
          at.inferred := AMap.add id ic !(at.inferred)

        | _, _ -> 
          let ic = {ic with confused = true} in
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
        if (v <> Tsum[]) && (k <> Tsum[]) then (
          let ic = {ic with key_t = key; value_t = va; guess = g; map = m; tuple =
            t; vector = ve; declaration = DValue} in
          at.inferred := AMap.add id ic !(at.inferred))
        else
          let ic = {ic with guess = g; map = m; tuple = t; vector = ve; declaration = DValue} in
          at.inferred := AMap.add id ic !(at.inferred)

  let analyze_declaration_kvalue env at id pi k v  = 
    let k = apply_subst env k in
    let v = apply_subst env v in
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in
    let ic = set_fdef env id pi ic in
    let {map = m; tuple = t; vector = ve; key_t = key; value_t = va; declaration
    = d; arr_loc = al; _}  = ic in
    let e = SingleLine(pi) in
    let p = is_parameter env id in
    let ic = {ic with arr_loc = pi::al; parameter = p} in
    match d with 
    | DValue -> 
      let ic = {ic with guess = NoGuess; confused = true; declaration =
        DKeyValue} in
      at.inferred := AMap.add id ic !(at.inferred)
    
    | DKeyValue
    | NoDec -> ( 
      match key, va with 
      | Some(key_t), Some(va_t) when (key_t = k || string_equ key_t k) &&
      (va_t = v || string_equ va_t v)-> 
        let m = add_evidence m Supporting e in
        let ve = add_evidence ve Opposing e in
        let t = add_evidence t Opposing e in
        let g = Map (key_t, va_t) in
        let ic = {ic with map = m; vector = ve; tuple = t; guess = g;
          declaration = DKeyValue} in
        at.inferred := AMap.add id ic !(at.inferred)
        
      | None, None ->  
        let key = Some(k) in
        let va = Some(v) in
        let m = add_evidence m Supporting e in
        let ve = add_evidence ve Opposing e in
        let t = add_evidence t Opposing e in
        let g = Map (k, v) in
        if (v <> Tsum[]) && (k <> Tsum[]) then (
          let ic = {ic with key_t = key; value_t = va; map = m; vector = ve;
            tuple = t; guess = g; declaration = DKeyValue} in
          at.inferred := AMap.add id ic !(at.inferred))
        else
          let ic = {ic with map = m; vector = ve; tuple = t; guess = g; declaration = DKeyValue} in
          at.inferred := AMap.add id ic !(at.inferred)
      
      | _, _ ->
        let ic = {ic with guess = NoGuess; confused = true; declaration =
          DKeyValue} in
        at.inferred := AMap.add id ic !(at.inferred)
      )

  let analyze_nonstring_access env at id pi k v = 
    let k = apply_subst env k in 
    let v = apply_subst env v in
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in 
    let ic = set_fdef env id pi ic in
    let {map = m; tuple = t; vector = ve; key_t = key; value_t = va; arr_loc =
      al; _} = ic in
    let e = SingleLine(pi) in
    let p = is_parameter env id in
    let ic = {ic with arr_loc = pi::al; parameter = p} in
    match key, va with 
    | Some(key_t), _ when (key_t <> k && not (string_equ key_t k))->
      let ic = {ic with guess = NoGuess; confused = true} in
      at.inferred := AMap.add id ic !(at.inferred)

    | Some(key_t), Some(va_t) when (key_t = k || string_equ key_t k) && k <>
      Tsum[Tabstr "int"] && (va_t = v || string_equ va_t v)-> 
      let m = add_evidence m Supporting e in 
      let ve = add_evidence ve Opposing e in
      let t = add_evidence t Opposing e in
      let g = Map (key_t, va_t) in
      let ic = {ic with map = m; vector = ve; tuple = t; guess = g} in
      at.inferred := AMap.add id ic !(at.inferred)
    
    | Some(key_t), Some(va_t) when (key_t = k || string_equ key_t k) && k =
      Tsum[Tabstr "int"] && (va_t = v || string_equ va_t v) -> 
      at.inferred := AMap.add id ic !(at.inferred)
    
    | Some(Tsum[Tabstr "int"]), Some(va_t) when (va_t <> v && not (string_equ
      va_t v)) ->
      let m = add_evidence m Opposing e in
      let ve = add_evidence ve Opposing e in 
      let t = add_evidence t Supporting e in
      let ic = {ic with map = m; vector = ve; tuple = t; guess = Tuple;
        mixed_val_ty = true} in
      at.inferred := AMap.add id ic !(at.inferred)
    
    | None, None -> (*also doesn't really tell me anything*)
        if (v <> Tsum[]) && (k <> Tsum[]) then 
        let key = Some(k) in 
        let va = Some(v) in
        let ic = {ic with key_t = key; value_t = va} in
        at.inferred := AMap.add id ic !(at.inferred)
    
    | _, _ ->
      at.inferred := AMap.add id ic !(at.inferred)

  let analyze_string_access env at id pi v = 
    let v = apply_subst env v in 
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in
    let ic = set_fdef env id pi ic in
    let {map = m; tuple = t; vector = ve; key_t = key; value_t = va; arr_loc =
      al; _} = ic in
    let e = SingleLine(pi) in
    let p = is_parameter env id in
    let t = add_evidence t Opposing e in 
    let ve = add_evidence ve Opposing e in
    let ic = {ic with arr_loc = pi::al; parameter = p; tuple = t; vector = ve} in
    let k = Tsum[Tabstr "string"] in 
    match key, va with 
    | None, None -> 
      let key = Some(k) in
      let va = Some(v) in 
      let m = add_evidence m Supporting e in
      let g = Map (k, v) in
      if (k <> Tsum[]) && (v <> Tsum[]) then
        let ic = {ic with key_t = key; value_t = va; map = m; guess = g} in
        at.inferred := AMap.add id ic !(at.inferred)
      else 
        let ic = {ic with map = m; guess = g} in 
        at.inferred := AMap.add id ic !(at.inferred)
    | Some(key_t),  Some(va_t) when (va_t = v || string_equ va_t v) && (key_t =
      k || string_equ key_t k) -> 
      let m = add_evidence m Supporting e in
      let g = Map (key_t, va_t) in
      let ic = {ic with map = m; guess = g} in
      at.inferred := AMap.add id ic !(at.inferred)
    | _, _ -> 
      let m = add_evidence m Opposing e in
      let ic = {ic with map = m; confused = true; mixed_val_ty = true} in
      at.inferred := AMap.add id ic !(at.inferred)

  let declared_array env at id pi tl=
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in
    let ic = set_fdef env id pi ic in
    let {arr_loc = al; dec_loc = dl; _} = ic in
    let ic = {ic with arr_loc = pi::al; dec_loc = pi::dl; tup_ty_l = tl} in
    at.inferred := AMap.add id ic !(at.inferred)

  let initialize_inferred_container env at id pi = 
    if not (AMap.mem id !(at.inferred)) then 
      let ic = make_inferred_container in 
      let ic = set_fdef env id pi ic in
      at.inferred := AMap.add id ic !(at.inferred)

  let string_of_container_t env = function
    | Vector t -> "vector of "^(THP.Type_string.ty env ISet.empty 0 t)
    | Tuple -> "tuple"
    | Map (Tsum[Tsstring _], t) 
    | Map (Tsum[Tabstr "string"], t) -> "strmap of "^(THP.Type_string.ty env
    ISet.empty 0 t)
    | Map (Tsum[Tabstr "int"], t) -> "intmap of "^(THP.Type_string.ty env
    ISet.empty 0 t)
    | _ -> "(container not available)"

  let string_of_container = function
    | Vector _ -> "vector"
    | Tuple -> "tuple"
    | Map _ -> "map"
    | NoData -> "no data"
    | NoGuess -> "conflicting data exists"
    | NotArray -> "not an array"
  
  let set_return env at id pi = 
    let ic = try AMap.find id !(at.inferred) with Not_found ->
      make_inferred_container in 
    let ic = set_fdef env id pi ic in
    let ic = {ic with return_val = true;} in 
    at.inferred := AMap.add id ic !(at.inferred)

  let analyze_access_info env at id ail = 
    List.iter (fun ai -> 
      match ai with
      | (pi, NoIndex (v)) -> analyze_noindex env at id pi v
      | (pi, VarOrInt (k, v)) -> analyze_nonstring_access env at id pi k v
      | (pi, Const _) -> initialize_inferred_container env at id pi
      | (pi, ConstantString v) -> analyze_string_access env at id pi v
      | (pi, DeclarationValue v) -> analyze_declaration_value env at id pi v
      | (pi, DeclarationKValue (k, v)) -> analyze_declaration_kvalue env at id pi k v
      | (pi, Declaration tl ) -> declared_array env at id pi tl
      | (pi, Value v) -> analyze_value env at id pi v
      | (pi, UnhandledAccess) -> initialize_inferred_container env at id pi
      | (_, Parameter) ->()
      | (pi, ReturnValue) -> set_return env at id pi
    ) ail

  let analyze_accesses_values env at = 
    THP.AEnv.iter env (fun id ail -> 
      analyze_access_info env at id ail;
    )
  
  let infer_arrays env at =
    Printf.printf "Inferring arrays ...\n";
    analyze_accesses_values env at
  
(*****************************************************************************)
(* Pretty Printing *)
(*****************************************************************************)

  let strip s = Str.replace_first (Str.regexp "^[ ]+") "" s
  
  let get_line_to_patch at pi lines = 
    let ln = PI.line_of_info pi in
    let file = PI.file_of_info pi in
    let file_patches = try SMap.find file !(at.patches) with Not_found -> [] in
    let l = try List.assoc ln file_patches with Not_found -> List.nth lines (ln
    -1 ) in 
    l
  
  let indent n = 
    String.make n ' '

  let pp_evidence at lines e n = 
    let sp = indent n in
    match e with 
    | SingleLine (Some pi) -> Printf.printf "%sSee %s on line %d at %d:\n"
      sp (PI.file_of_info pi) (PI.line_of_info pi) (PI.col_of_info pi); 
      Printf.printf "%s%s\n" sp (strip (get_line_to_patch at pi lines))
    | DoubleLine (Some pi1, e1, Some pi2, e2) -> 
      Printf.printf "%sConflicting types in %s on line %d at %d:\n"
      sp (PI.file_of_info pi1) (PI.line_of_info pi1) (PI.col_of_info pi1);
      Printf.printf "%s%s\n" sp (strip (get_line_to_patch at pi1 lines));
      Printf.printf "%sand in %s on line %d at %d:\n" sp
      (PI.file_of_info pi2) (PI.line_of_info pi2) (PI.col_of_info pi2);
      Printf.printf "%s%s\n" sp (strip (get_line_to_patch at pi2 lines))
    | _ -> Printf.printf "%sEvidence unavailable" sp

  let rec pp_evidence_l at lines cel n = 
    match cel with 
    | [] -> ()
    | x::xs -> pp_evidence at lines x n; pp_evidence_l at lines xs n

  let pp_confused_reasoning at lines ic = 
    let {map = m; tuple = t; vector = v; guess = guess; confused = c;
    _} = ic in
    let {supporting = _; opposing = o} = m in
    let o = prune_evidence o in
    Printf.printf "      Not a map due to:\n";
    pp_evidence_l at lines o 8;
    let {supporting = _; opposing = o} = t in
    let o = prune_evidence o in
    Printf.printf "      Not a tuple due to:\n";
    pp_evidence_l at lines o 8;
    let {supporting = _; opposing = o} = v in
    let o = prune_evidence o in
    Printf.printf "      Not a vector due to:\n";
    pp_evidence_l at lines o 8
    

  let pp_reasoning at lines ic = 
    let {map = m; tuple = t; vector = v; guess = guess; confused = c;
    _} = ic in
      match guess with
      | Map _ ->
        let {supporting = s; opposing = _} = m in
        let s = prune_evidence s in
        pp_evidence_l at lines s 6
      | Tuple -> 
        let {supporting = s; opposing = _} = t in 
        let s = prune_evidence s in
        pp_evidence_l at lines s 6
      | Vector _ -> 
        let {supporting = s; opposing = _} = v in
        let s = prune_evidence s in
        pp_evidence_l at lines s 6
      | _ -> ()

  let pp_param_arrays at =
    Printf.printf "Arrays that are parameters\n";
    AMap.iter (fun id ic -> 
      let {guess = g; parameter = p; _} = ic in
      if p then (Printf.printf "Guess of param is %s: "(string_of_container g); pp_arr_id id)
      ) !(at.inferred)

  let pp_all_arr_id at = 
    Printf.printf "Ids\n";
    AMap.iter (fun id ic -> 
      pp_arr_id id
      ) !(at.inferred)

(*****************************************************************************)
(* Patching *)
(*****************************************************************************)


  let file_lines f = 
    let lines = ref [] in
    let in_ch = open_in f in
    try while true; do
      lines := input_line in_ch ::!lines
    done; []
    with End_of_file -> 
      close_in in_ch;
      List.rev !lines

  let add_patch_to_patch_list at file ln line = 
    let file_patches = try SMap.find file !(at.patches) with Not_found -> [] in
    let file_patches = (ln, line)::file_patches in
    at.patches := SMap.add file file_patches !(at.patches)


  let write_patch f lines = 
    let f = f in
    let out_ch = open_out f in
    List.iter (fun l -> output_string out_ch (l^"\n")) lines;
    close_out out_ch;
    Printf.printf "Patch applied \n"

  let rec insert_patched_line_to_list ln newl lines c =
    match lines with 
    | [] -> []
    | x::xs when ln = c -> newl::xs
    | x::xs -> x:: (insert_patched_line_to_list ln newl xs (c+1))
    

  let prompt_evidence at lines ic = 
    Printf.printf "Would you like to see evidence supporting this? (y/n)\n";
    let e = read_line () in
    if e = "y" || e = "yes" then 
      pp_reasoning at lines ic


  let rec prompt_patched_line at file old_line line line_n ic = 
    Printf.printf "    Suggested patch: \n      %s\n" line;
    Printf.printf "      Would you like to apply this patch? (y/n)\n";
    let patch = read_line () in 
    if patch = "y" then
      (
        add_patch_to_patch_list at file line_n line
      )
    else Printf.printf "    Patch not selected\n"

  let get_param_name id = 
    match id with 
    | (APS.Id(n, _), _, _) -> n
    | _ -> "Error retrieving name"

  let rec unsplit_list delim l = 
    match l with 
    | [] -> ""
    | x::xs -> delim^x^(unsplit_list delim xs)
  
  let rec create_tup_str_l env str l = 
    match l with 
    | [] -> str
    | x::xs -> 
      let t = THP.Type_string.ty env ISet.empty 0 x in
      create_tup_str_l env (str^t^", ") xs

  let create_tup_str env l = 
    let rs = create_tup_str_l env "(" l in
    let str_len = String.length rs in
    (String.sub rs 0 (str_len - 2))^")"

  let insert_param_type line_to_patch param_type_str name = 
    let spl_line = Str.split (Str.regexp ("\\"^name)) line_to_patch in
    match spl_line with 
    | [] -> raise Fun_def_error
    | x::xs -> x^param_type_str^(unsplit_list name xs)

  let check_type_string g s = 
    match s with 
    | "_" -> Printf.printf "Sorry, the type of the %s can not be determined.\n"
      (string_of_container g); false
    | _ -> true

  let prompt_types p = 
    Printf.printf "%s\n" p;
    read_line()

  let prompt_manual_patch_r_p at pi id ic = 
    Printf.printf "  Would you like to enter a patch yourself? (y/n)\n";
    let patch = read_line() in
    if patch = "y" || patch = "yes" then (
      Printf.printf "    Please select container: (vector/tuple/strmap/intmap/q to cancel patch)\n";
      let patch = read_line () in 
      let patch = String.lowercase patch in
      match patch with 
      | "v" | "vector" -> (
        let ty = prompt_types "    Please enter the type of the vector: ex int \n" in
        match ty with 
        | "q" -> Printf.printf "    Patch cancelled\n"; ""
        | _ -> ("Vector<"^ty^">")
      )
      | "t" | "tuple" -> (
        let ty = prompt_types "    Please enter the types contained in the tuple in the following format: (type1, type2, ...) \n" in
        match ty with 
        | "q" -> Printf.printf "    Patch cancelled\n"; "" 
        | _ -> ty
      )
      | "s" | "strmap" -> (
        let ty = prompt_types "    Please enter the type of the strmap: ex int\n" in
        match ty with 
        | "q" -> Printf.printf "    Patch cancelled\n"; ""
        | _ -> ("SMap<"^ty^">")
      )
      | "i" | "intmap" -> (
        let ty = prompt_types "    Please enter the type of the intmap: ex string\n" in
        match ty with 
        | "q" -> Printf.printf "    Patch cancelled\n"; ""
        | _ -> ("IMap<"^ty^">")
      )
      | _ ->( 
        Printf.printf "    Patch cancelled\n";
        "")
    )
    else ( 
      Printf.printf "    Patch cancelled\n"; 
      "" 
    )


  let suggest_manually_patch_parameter at pi lines id ic n = 
    let file = PI.file_of_info pi in
    let ln = PI.line_of_info pi in
    match prompt_manual_patch_r_p at pi id ic with 
    | "" -> ()
    | p -> 
      let line_to_patch = get_line_to_patch at pi lines in
      let patched_line = insert_param_type line_to_patch p n in
      prompt_patched_line at file line_to_patch patched_line ln ic;
      let ic = {ic with manual_patch_ty = p} in
      at.inferred := AMap.add id ic !(at.inferred)

  let suggest_patch_parameter env at g pi lines ic n = 
    let ln = (PI.line_of_info pi) - 1 in
    let file = PI.file_of_info pi in
    let line_to_patch = get_line_to_patch at pi lines in
    match g with 
    | Vector t -> 
        let t_string = THP.Type_string.ty env ISet.empty 0 t in
        if check_type_string g t_string then (
        let t_string = "Vector<"^t_string^"> " in 
        let patched_line = insert_param_type line_to_patch t_string n in
        prompt_patched_line at file line_to_patch patched_line ln ic
        )
    | Tuple _ -> 
        let {tup_ty_l = tup_ty; _} = ic in
        let t_string = create_tup_str env tup_ty in
        let patched_line = insert_param_type line_to_patch t_string n in
        prompt_patched_line at file line_to_patch patched_line ln ic
    | Map (Tsum[Tsstring _], t)
    | Map (Tsum[Tabstr "string"], t) -> 
        let t_str = THP.Type_string.ty env ISet.empty 0 t in
        if check_type_string g t_str then (
        let t_string = "SMap<"^t_str^">" in
        let patched_line = insert_param_type line_to_patch t_string n in
        prompt_patched_line at file line_to_patch patched_line ln ic
        )
    | Map (Tsum[Tabstr "int"], t) -> 
        let t_str = THP.Type_string.ty env ISet.empty 0 t in
        if check_type_string g t_str then (
        let t_string = "IMap<"^t_str^">" in
        let patched_line = insert_param_type line_to_patch t_string n in
        prompt_patched_line at file line_to_patch patched_line ln ic
        )
    | _ -> Printf.printf "  Sorry, no patch can be applied\n"

  let patch_parameter env at id ic = 
    let name = get_param_name id in
    let {confused = c; guess = g; fdef_loc = fd; _} = ic in
    match fd with 
    | None ->()
    | Some(pi) -> 
      let lines = file_lines (PI.file_of_info pi) in 
      Printf.printf "  Parameter %s is an array in the following function in %s on %d: \n" 
        name (PI.file_of_info pi) (PI.line_of_info pi);
      Printf.printf "  %s\n" (strip (get_line_to_patch at pi lines));
      if (not c && not (g = NoData)) then (
        Printf.printf "    This array may be a %s. " (string_of_container g);
        prompt_evidence at lines ic;
        suggest_patch_parameter env at g pi lines ic name
        )
      else if c then (
        Printf.printf "    Confused about the array due to the following:\n";
        pp_confused_reasoning at lines ic;
        suggest_manually_patch_parameter at pi lines id ic name
      )
      else (* No data *)
        Printf.printf "    No data about this array is available.\n"
 
  let suggest_manually_patch_declaration at pi lines id ic = 
    let file = PI.file_of_info pi in
    let ln = PI.line_of_info pi in
    Printf.printf "  Would you like to enter a patch yourself? (y/n)\n";
    let patch = read_line() in
    if patch = "y" || patch = "yes" then (
      let line_to_patch = get_line_to_patch at pi lines in
      let arr_regex = Str.regexp "array" in
      Printf.printf "    Please select container: (vector/tuple/strmap/intmap/g to cancel patch\n";
      let patch = read_line() in
      let patch = String.lowercase patch in
      match patch with 
      | "v" | "vector" -> 
        let patched_line = Str.replace_first arr_regex "vector" line_to_patch in
        prompt_patched_line at file line_to_patch patched_line ln ic
      | "t" | "tuple" -> 
        let patched_line = Str.replace_first arr_regex "tuple" line_to_patch in
        prompt_patched_line at file line_to_patch patched_line ln ic
      | "s" | "strmap" -> 
        let patched_line = Str.replace_first arr_regex "strmap" line_to_patch in
        prompt_patched_line at file line_to_patch patched_line ln ic
      | "i" | "intmap" -> 
        let patched_line = Str.replace_first arr_regex "intmap" line_to_patch in
        prompt_patched_line at file line_to_patch patched_line ln ic
      | _ -> Printf.printf "    Patch cancelled\n"
    )

  let suggest_patch_declaration at g pi lines ic = 
    let ln = (PI.line_of_info pi) - 1 in
    let file = PI.file_of_info pi in
    let line_to_patch = get_line_to_patch at pi lines in
    let arr_regex = Str.regexp "array" in
    match g with 
    | Vector _ -> 
        let patched_line = Str.replace_first arr_regex "vector" line_to_patch in
        prompt_patched_line at file line_to_patch patched_line ln ic
    | Tuple -> 
        let patched_line = Str.replace_first arr_regex "tuple" line_to_patch in
        prompt_patched_line at file line_to_patch patched_line ln ic 
    | Map (Tsum[Tsstring _], _) 
    | Map (Tsum[Tabstr "string"], _) -> 
        let patched_line = Str.replace_first arr_regex "strmap" line_to_patch in 
        prompt_patched_line at file line_to_patch patched_line ln ic 
    | Map (Tsum[Tabstr "int"], _) -> 
        let patched_line = Str.replace_first arr_regex "intmap" line_to_patch in
        prompt_patched_line at file line_to_patch patched_line ln ic
    | _ -> 
        Printf.printf "  Sorry, no patch can be applied  \n"

  let patch_declaration env at id ic =
    let {dec_loc = dl; confused = c; guess = g; parameter = p; _} = ic in
    if p then patch_parameter env at id ic else (
      List.iter (fun x -> 
    match x with
    | None -> ()
    | Some(pi) ->
      Printf.printf "  Declared in %s on line %d at position %d:\n"
        (PI.file_of_info pi) (PI.line_of_info pi) (PI.col_of_info pi);
      let lines = file_lines (PI.file_of_info pi) in
      Printf.printf "  %s\n" (strip (get_line_to_patch at pi lines));
      if (not c && not (g = NoData)) then(
        Printf.printf "    This array may be a %s. " (string_of_container g);
        prompt_evidence at lines ic;
        suggest_patch_declaration at g pi lines ic 
        )
      else if c then (
        Printf.printf "    Confused about the array due to the following:\n";
        pp_confused_reasoning at lines ic;
        suggest_manually_patch_declaration at pi lines id ic
      )
      else (* No data *)
        Printf.printf "    No data about this array is available.\n"
      ) dl
    )

  let insert_return_value line_to_patch return_type_str = 
    let spl_line = Str.split (Str.regexp ")") line_to_patch in
    match spl_line with
    | [] -> raise Fun_def_error
    | x::xs -> x^"): "^return_type_str^(unsplit_list ")" xs)

  let suggest_manually_patch_return at pi lines id ic = 
    let file = PI.file_of_info pi in
    let ln = PI.line_of_info pi in
    match prompt_manual_patch_r_p at pi id ic with 
    | "" -> ()
    | p ->
      let line_to_patch = get_line_to_patch at pi lines in
      let patched_line = insert_return_value line_to_patch p in
      prompt_patched_line at file line_to_patch patched_line ln ic;
      let ic = {ic with manual_patch_ty = p} in
      at.inferred := AMap.add id ic !(at.inferred)

  let suggest_patch_return_val env at g pi lines ic = 
    let ln = (PI.line_of_info pi) - 1 in 
    let file = PI.file_of_info pi in 
    let line_to_patch = get_line_to_patch at pi lines in 
    match g with 
    | Vector t -> 
        let t_string = THP.Type_string.ty env ISet.empty 0 t in
        if check_type_string g t_string then (
        let t_string = "Vector <"^t_string^">" in
        let patched_line = insert_return_value line_to_patch t_string in
        prompt_patched_line at file line_to_patch patched_line ln ic
        )
    | Tuple _ -> 
        let {tup_ty_l = tup_ty; _} = ic in 
        let t_string = create_tup_str env tup_ty in 
        let patched_line = insert_return_value line_to_patch t_string in
        prompt_patched_line at file line_to_patch patched_line ln ic
    | Map (Tsum[Tsstring _], _) 
    | Map (Tsum[Tabstr "string"], _) -> 
        let t_string = "type" in 
        if check_type_string g t_string then (
        let t_string = "SMap <"^t_string^">" in
        let patched_line = insert_return_value line_to_patch t_string in
        prompt_patched_line at file line_to_patch patched_line ln ic
        )
    | Map (Tsum[Tabstr "int"], t) -> 
        let t_string = THP.Type_string.ty env ISet.empty 0 t in 
        if check_type_string g t_string then (
        let t_string = "IMap <"^t_string^">" in
        let patched_line = insert_return_value line_to_patch t_string in
        prompt_patched_line at file line_to_patch patched_line ln ic
        )
    | _ -> Printf.printf "  Sorry, no patch can be applied\n"

  let patch_return_value env at id ic =
    let {fdef_loc = fd; confused = c; guess = g; _} = ic in
    match fd with 
    | None -> () 
    | Some(pi) ->
      let lines = file_lines (PI.file_of_info pi) in
      if (not c && not (g = NoData)) then (
        Printf.printf "  Returned by the following function in %s on line %d: \n" 
        (PI.file_of_info pi) (PI.line_of_info pi);
        Printf.printf "  %s\n" (strip (get_line_to_patch at pi lines));
        Printf.printf "    This array may be a %s. " (string_of_container g);
        prompt_evidence at lines ic; 
        suggest_patch_return_val env at g pi lines ic
      )
      else if c then (
        Printf.printf "    Confused about the array due to the following:\n";
        pp_confused_reasoning at lines ic;
        suggest_manually_patch_return at pi lines id ic
      )
      else
        Printf.printf "    No data about this array is available.\n"

  let rec write_lines pl lines out_ch ln = 
    match pl, lines with
    | _, [] -> ()
    | (ln1, line1)::xs1, line2::xs2 when ln1 = ln ->
      output_string out_ch (line1^"\n");
      write_lines xs1 xs2 out_ch (ln+1)
    | _, line::xs -> 
      output_string out_ch (line^"\n");
      write_lines pl xs out_ch (ln+1)

  let write_file_patches file pl lines = 
    let file = file in
    let out_ch = open_out file in
    write_lines pl lines out_ch 0;
    close_out out_ch

  let compare_patches p1 p2 = 
    match p1, p2 with 
    | (l1, _), (l2, _) when l1 < l2 -> (-1)
    | (l1, _), (l2, _) when l1 > l2 -> 1
    | (l1, _), (l2, _) -> 0

  let apply_file_patches file pl = 
    let pl = List.sort compare_patches pl in
    Printf.printf "%s: %d change(s)\n" file (List.length pl);
    let lines = file_lines file in
    write_file_patches file pl lines
  
  let apply_all_patches at = 
    SMap.iter apply_file_patches !(at.patches)

  let rec get_file_of_dec_loc dl = 
    match dl with 
    | [] -> ""
    | Some(pi)::xs -> "in "^(PI.file_of_info pi)^" "
    | None::xs -> get_file_of_dec_loc xs

  let get_class_of_id id = 
    match id with 
    | (_, _, "") -> ""
    | (_, _, c) -> "in "^c^" "

  let get_function_of_id id = 
    match id with 
    | (_, "", _) -> ""
    | (_, f, _) -> "in "^f

  let patch_possible ic = 
    let {return_val = r; dec_loc = dl; fdef_loc = fd; parameter = p; _} = ic in
    let param = not (p && (fd = None)) in
    let dec = not (not p && (List.length dl = 0)) in
    let return = not (r && (fd = None)) in
    param || dec || return 

  let patch_suggestion env at = 
    Printf.printf "Preparing to patch files \n";
    AMap.iter (fun id ic ->
      if patch_possible ic then (
        let {confused = c; guess = g; return_val = r; dec_loc = dl; arr_loc = 
          al; _} = ic in
        let file = get_file_of_dec_loc dl in 
        let c = get_class_of_id id in
        let f = get_function_of_id id in
        if (g <> NotArray) && (g <> NoData) && r then (
          Printf.printf "\n** Array %s%s%s is a %s\n" file c f
          (string_of_container_t env g);
          let al = prune_arr_loc al in
          pp_arr_loc al;
          patch_declaration env at id ic;
          patch_return_value env at id ic
        )
        else if not (g = NotArray) && (g <> NoData) then(
          Printf.printf "\n** Array %s%s%s is a %s\n" file c f
          (string_of_container_t env g);
          let al = prune_arr_loc al in
          pp_arr_loc al;
          patch_declaration env at id ic;
        )
      )
    ) !(at.inferred);
    apply_all_patches at

end
