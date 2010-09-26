(* Copyright (C) 2008 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

open Common

open Ast_java

(*****************************************************************************)
(* Visitor, fold-like, the general one *)
(*****************************************************************************)
type 'a inout = 'a -> 'a 

(* Note that I don't visit necesserally in the order of the token
 * found in the original file. So don't assume such hypothesis!
 * But I try at least for big stuff like expression, or statement, 
 * to visit them in the same order by for instance introducing 
 * some extra let because of ocaml default right-from-left evaluation order.
 * It makes possible then to have only one visitor code and 
 * to make available a iter-like visitor by just wrapping it 
 * over the fold-like visitor.
 *)


(* case, var_decl_id *)
(*
  kdecl_s: (declaration  inout * visitor_c_s) -> declaration inout;
  kdef_s:  (definition   inout * visitor_c_s) -> definition  inout; 

  ktoplevel_s: (toplevel inout * visitor_c_s) -> toplevel inout;
  knode_s: (F.node inout * visitor_c_s) -> F.node inout;

  kdefineval_s: (define_val inout * visitor_c_s) -> define_val inout;
*)

type visitor_s = { 
  ktype_s:      (typ   inout * visitor_s) -> typ   inout;
  kexpr_s:      (expr inout * visitor_s) -> expr inout;
  kstatement_s: (stmt  inout * visitor_s) -> stmt  inout;

  kini_s:  (init  inout * visitor_s) -> init inout; 
  kcase_s: (case inout * visitor_s) -> case inout;


  kname_s: (name inout * visitor_s) -> name inout;

  kdecl_s: (decl inout * visitor_s) -> decl inout;
  kprogram_s: (compilation_unit inout * visitor_s) -> compilation_unit inout;

  kinfo_s: (info inout * visitor_s) -> info inout;

 } 

(*
    kdecl_s      = (fun (k,_) d  -> k d);
    kdef_s       = (fun (k,_) d  -> k d);
    ktoplevel_s  = (fun (k,_) p  -> k p);
    knode_s      = (fun (k,_) n  -> k n);
    kdefineval_s = (fun (k,_) x  -> k x);
*)
let default_visitor_s = 
  { 
    kexpr_s =      (fun (k,_) e  -> k e);
    kstatement_s = (fun (k,_) st -> k st);
    ktype_s      = (fun (k,_) t  -> k t);

    kini_s       = (fun (k,_) d  -> k d);
    kcase_s       = (fun (k,_) d  -> k d);


    kname_s      = (fun (k,_) i  -> k i);

    kdecl_s = (fun (k,_) i  -> k i);
    kprogram_s = (fun (k,_) i  -> k i);

    kinfo_s      = (fun (k,_) i  -> k i);
  } 




let rec compilation_unit = fun bigf cu -> 
  let  _iif ii = infoii bigf ii in
  let rec cuf st = bigf.kprogram_s (k, bigf) st 
  and k cu = 
    let package' = Common.fmap (name_wrap bigf) cu.package in
    let imports' = List.map (name bigf) cu.imports in
    let decls' = List.map (decl bigf) cu.decls in
    {
      package = package';
      imports = imports';
      decls = decls';
    }
  in
  cuf cu

and decls = fun bigf decls -> List.map (decl bigf) decls

and decl = fun bigf decl -> 
  let  _iif ii = infoii bigf ii in
  let rec declf st = bigf.kdecl_s (k, bigf) st 
  and k decl = 
    match decl with
    | Class cd        -> Class        (class_decl bigf cd)
    | Interface inter -> Interface    (interface bigf inter)
    | Field fld       -> Field        (field bigf fld)
    | Method md       -> Method       (method_decl bigf md)
    | InstanceInit st -> InstanceInit (stmt bigf st)
    | StaticInit st   -> StaticInit   (stmt bigf st)
    | Constructor md  -> Method       (method_decl bigf md)
  in
  declf decl

and class_decl = fun bigf cd -> 
  let  _iif ii = infoii bigf ii in

  let mods' = List.map (modifier bigf) cd.cl_mods in
  let name' = ident bigf cd.cl_name in
  let supers' = Common.fmap (name bigf) cd.cl_super in
  let impls' = List.map (name bigf) cd.cl_impls in
  let decls' = List.map (decl bigf) cd.cl_body in 
  
  { 
    cl_mods = mods';
    cl_name = name'; 
    cl_super = supers';
    cl_impls = impls';
    cl_body = decls';
  }

and interface = fun bigf inter -> 
  let  _iif ii = infoii bigf ii in

  let mods' = List.map (modifier bigf) inter.if_mods in
  let name' = ident bigf inter.if_name in
  let exts' = List.map (name bigf) inter.if_exts in
  let decls' = List.map (decl bigf) inter.if_body in 

  { 
    if_mods = mods';
    if_name = name';
    if_exts = exts';
    if_body = decls';
  }


and field = fun bigf fld -> 
  let  _iif ii = infoii bigf ii in
  let var' = var bigf fld.f_var in
  let init' = Common.fmap (init bigf) fld.f_init in
  { 
    f_var = var';
    f_init = init';
  }

and var = fun bigf avar -> 
  let  _iif ii = infoii bigf ii in

  let mods' = List.map (modifier bigf) avar.v_mods in
  let typ' = typ bigf avar.v_type in
  let name' = ident bigf avar.v_name in

  {
    v_mods = mods';
    v_type = typ';
    v_name = name';
  }



and method_decl = fun bigf md -> 
  let  _iif ii = infoii bigf ii in

  let var' = var bigf md.m_var in
  let formals' = List.map (var bigf) md.m_formals in

  let throws' = List.map (name bigf) md.m_throws in
  let stmt' = stmt bigf md.m_body in
  { 
    m_var = var';
    m_formals = formals';
    m_throws = throws'; 
    m_body = stmt';
  }


and init = fun bigf ini -> 
  let  iif ii = infoii bigf ii in
  let rec initf ini = bigf.kini_s (k, bigf) ini
  and k ini = 
   let (unwrap_ini, ii) = ini in
   let ini' = 
     match unwrap_ini with
     | ExprInit e ->   ExprInit (expr bigf e)
     | ArrayInit is -> ArrayInit (List.map initf is)
   in
   ini', iif ii
  in
  initf ini


and stmt = fun bigf stm -> 
  let  iif ii = infoii bigf ii in
  let rec statf st = bigf.kstatement_s (k, bigf) st 
  and k st = 
    let (unwrap_st, ii) = st in
    let st' = 
      match unwrap_st with

      | Empty -> Empty

      | Block stmts -> Block (List.map statf stmts)

      | Expr e -> Expr (expr bigf e)

      | If (e, st1, st2opt) -> 
          let e' = expr bigf e in
          let st1' = statf st1 in
          let st2opt' = Common.fmap statf st2opt in
          If (e', st1', st2opt')
          
      | Switch (e, cases_stmts_list) -> 
          let e' = expr bigf e in
          let cases_stmts_list' = List.map (cases_stmts bigf) cases_stmts_list
          in
          Switch (e', cases_stmts_list')


      | While (e, st) -> 
          let e' = expr bigf e in
          let st' = statf st in
          While (e', st')
      | Do (st,e) -> 
          let st' = statf st in
          let e' = expr bigf e in
          Do (st', e')
          
      | For (sts1, eopt, sts2, st) -> 
          let sts1' = List.map statf sts1 in
          let eopt' = Common.fmap (expr bigf) eopt in
          let sts2' = List.map statf sts2 in
          let st' = statf st in
          For (sts1', eopt', sts2', st')

      | Break idopt -> Break idopt
      | Continue idopt -> Continue  idopt

      | Return eopt -> Return (Common.fmap (expr bigf) eopt)
      | Label (id, st) -> 
          let st' = statf st in
          Label (id, st')

      | Sync (e, st) -> 
          let e' = expr bigf e in
          let st' = statf st in
          Sync (e', st')

      | Try (st, cats, stopt) -> 
          let st' = statf st in
          let cats' = List.map (catch bigf) cats in
          let stopt' = Common.fmap statf stopt in
          Try (st', cats', stopt')

      | Throw e -> Throw (expr bigf e)

      (* decl as statement *)
      | LocalVar fld -> LocalVar (field bigf fld)
      | LocalClass cd -> LocalClass (class_decl bigf cd)

      | Assert (e1, e2opt) -> 
          let e1' = expr bigf e1 in
          let e2opt' = Common.fmap (expr bigf) e2opt in
          Assert (e1', e2opt')
    in
    st', iif ii
  in
  statf stm


and cases_stmts = fun bigf cas_sts -> 
  let (cases, stmts) = cas_sts in

  let cases' = List.map (case bigf) cases in
  let stmts' = List.map (stmt bigf) stmts in

  cases', stmts'

and case = fun bigf cs -> 
  let  iif ii = infoii bigf ii in
  let rec casef ini = bigf.kcase_s (k, bigf) cs
  and k cs = 
   let (unwrap_cs, ii) = cs in
   let cs' = 
     match unwrap_cs with
     | Case e ->   Case (expr bigf e)
     | Default -> Default
   in
   cs', iif ii
  in
  casef cs
  


and catch = fun bigf ct -> 
  let  _iif ii = infoii bigf ii in
  let (avar, st) = ct in

  let var' = var bigf avar in
  let st' = stmt bigf st in

  var', st'



and expr = fun bigf exp -> 
  let  iif ii = infoii bigf ii in

  let rec exprf e = bigf.kexpr_s (k, bigf) e
  and k exp = 
    let (unwrap_e, ii) = exp  in
    let e' = 
      match unwrap_e with
      | Name n -> Name (name bigf n)
      | Literal s -> Literal s
      | ClassLiteral t -> ClassLiteral (typ bigf t)

      | NewClass (t, es, ds_opt) -> 
          let t' = typ bigf t in
          let es' = List.map exprf es in
          let ds_opt' = Common.fmap (class_body bigf) ds_opt in
          NewClass (t', es', ds_opt')

      | NewQualifiedClass (e, id, es, ds_opt) -> 
          let e' = exprf e in
          let es' = List.map exprf es in
          let ds_opt' = Common.fmap (class_body bigf) ds_opt in
          NewQualifiedClass (e', id, es', ds_opt')

      | NewArray (t, es, aint, iniopt) -> 
          let t' = typ bigf t in
          let es' = List.map exprf es in
          let iniopt' = Common.fmap (init bigf) iniopt in
          NewArray (t', es', aint, iniopt')


      | Dot (e, id) -> 
          let e' = exprf e in
          Dot (e', id)

      | Call (e, es) -> 
          let e' = exprf e in
          let es' = List.map exprf es in
          Call (e', es')
      | ArrayAccess (e1, e2)  -> 
          let e1' = exprf e1 in
          let e2' = exprf e2 in
          ArrayAccess (e1', e2')

      | Postfix (e, op) -> 
          Postfix (exprf e, op) 
      | Prefix (op, e) -> 
          Prefix (op, exprf e)
            
      | Cast (t, e) -> 
          let t' = typ bigf t in
          let e' = exprf e in
          Cast (t', e')

      | Infix (e1, op, e2) -> 
          let e1' = exprf e1 in
          let e2' = exprf e2 in
          Infix (e1', op, e2')

      | InstanceOf (e, t) -> 
          let e' = exprf e in
          let t' = typ bigf t in
          InstanceOf (e', t')

      | Conditional (e1, e2, e3) -> 
          let e1' = exprf e1 in
          let e2' = exprf e2 in
          let e3' = exprf e3 in
          Conditional (e1', e2', e3')

      | Assignment (e1, op, e2) -> 
          let e1' = exprf e1 in
          let e2' = exprf e2 in
          Assignment (e1', op, e2')
    in
    e', iif ii
  in
  exprf exp



and typ = fun bigf ty -> 
  let  iif ii = infoii bigf ii in
  let rec typf ty = bigf.ktype_s (k, bigf) ty
  and k ty = 
   let (unwrap_ty, ii) = ty in
   let ty' = 
     match unwrap_ty with
     | TypeName n -> TypeName (name bigf n)
     | ArrayType t -> ArrayType (typ bigf t)
   in
   ty', iif ii
  in
  typf ty



and name = fun bigf na -> 
  let  _iif ii = infoii bigf ii in
  let rec namef e = bigf.kname_s (k, bigf) e
  and k xs = List.map (ident bigf) xs
  in
  namef na

and ident = fun bigf id -> 
  let  iif ii = infoii bigf ii in
  let (anid, ii) = id in
  anid, iif ii

and name_wrap = fun bigf nw -> 
  let  iif ii = infoii bigf ii in
  let (aname, ii) = nw in
  let name' = name bigf aname in
  name', iif ii

and modifier = fun bigf modif -> 
  let  iif ii = infoii bigf ii in
  let (amod, ii) = modif in
  amod, iif ii
  

and info = fun bigf aninfo -> 
  let rec infof ii = bigf.kinfo_s (k, bigf) ii
  and k i = i
  in
  infof aninfo

and infoii = fun bigf ii -> 
  List.map (info bigf) ii


and class_body = fun bigf cb -> 
  let  iif ii = infoii bigf ii in
  let (decls, ii) = cb in
  let decls' = List.map (decl bigf) decls in
  decls', iif ii

and modifiers = fun bigf xs -> 
  List.map (modifier bigf) xs


(*****************************************************************************)
(* Visitor, iter-like *)
(*****************************************************************************)

type 'a effect = 'a -> unit
type visitor = { 
  ktype:      (typ   effect * visitor_s) -> typ   effect;
  kexpr:      (expr effect * visitor_s) -> expr effect;
  kstatement: (stmt  effect * visitor_s) -> stmt  effect;

  kini:  (init  effect * visitor_s) -> init effect; 

  kname: (name effect * visitor_s) -> name effect;
  kinfo: (info effect * visitor_s) -> info effect;
 } 

let default_visitor = 
  { 
    ktype      = (fun (k,_) t  -> k t);
    kexpr =      (fun (k,_) e  -> k e);
    kstatement = (fun (k,_) st -> k st);

    kini       = (fun (k,_) ie  -> k ie);


    kname      = (fun (k,_) ii  -> k ii);
    kinfo      = (fun (k,_) ii  -> k ii);
  } 
(*
    kdecl      = (fun (k,_) d  -> k d);
    kdef       = (fun (k,_) d  -> k d);
    knode      = (fun (k,_) n  -> k n);
    ktoplevel  = (fun (k,_) p  -> k p);
*)


let adapt_visitor_s bigf_iter = 
  {
    kexpr_s =      (fun (k,_) e  -> k e);
    kstatement_s = (fun (k,_) st -> k st);
    ktype_s      = (fun (k,_) t  -> k t);
    
    kini_s       = (fun (k,_) d  -> k d);
    kcase_s       = (fun (k,_) d  -> k d);


    kname_s      = (fun (k,_) i  -> k i);

    kdecl_s       = (fun (k,_) d  -> k d);
    kprogram_s       = (fun (k,_) d  -> k d);

    kinfo_s      = (fun (k,_) i  -> k i);
  }
  
