open Common

open Ast_cpp

module Ast = Ast_cpp
module Flag = Flag_parsing_cpp

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common2.mk_pr2_wrappers Flag.verbose_parsing 

let warning s v = 
  if !Flag.verbose_parsing 
  then Common2.warning ("PARSING: " ^ s) v
  else v

exception Semantic of string * Ast_cpp.tok

(*****************************************************************************)
(* Parse helpers functions *)
(*****************************************************************************)

(*-------------------------------------------------------------------------- *)
(* Type related *)
(*-------------------------------------------------------------------------- *)

type shortLong = Short | Long | LongLong
    
(* note: have a full_info: parse_info list; to remember ordering
 * between storage, qualifier, type? well this info is already in
 * the Ast_c.info, just have to sort them to get good order 
 *)
type decl = { 
  storageD: storage;
  typeD: (sign option * shortLong option * typeCbis option) wrap;
  qualifD: typeQualifier;
  inlineD: bool wrap;
} 

let nullDecl = {
  storageD = NoSto;
  typeD = (None, None, None), noii;
  qualifD = Ast.nQ;
  inlineD = false, noii;
}

let addStorageD x decl  =
  match decl with
  | {storageD = NoSto; _} -> { decl with storageD = x }
  | {storageD = (StoTypedef ii | Sto (_, ii)) as y; _} -> 
      if x = y 
      then decl +> warning "duplicate storage classes"
      else raise (Semantic ("multiple storage classes", ii))

let addInlineD ii decl =
  match decl with
  | {inlineD = (false,[]); _} -> { decl with inlineD=(true,[ii])}
  | {inlineD = (true, _ii2); _} -> decl +> warning "duplicate inline"
  | _ -> raise Impossible


let addTypeD ty decl =
  match ty, decl with
  | (Left3 Signed,_ii), {typeD = ((Some Signed,  _b,_c),_ii2); _} -> 
      decl +> warning "duplicate 'signed'"
  | (Left3 UnSigned,_ii), {typeD = ((Some UnSigned,_b,_c),_ii2); _} -> 
      decl +> warning "duplicate 'unsigned'"
  | (Left3 _,ii),        {typeD = ((Some _,_b,_c),_ii2); _} -> 
      raise (Semantic ("both signed and unsigned specified", List.hd ii))
  | (Left3 x,ii),        {typeD = ((None,b,c),ii2); _} -> 
      { decl with typeD = (Some x,b,c),ii @ ii2}
  | (Middle3 Short,_ii),  {typeD = ((_a,Some Short,_c),_ii2); _} -> 
      decl +> warning "duplicate 'short'"

      
  (* gccext: long long allowed *)
  | (Middle3 Long,ii),   {typeD = ((a,Some Long,c),ii2); _}-> 
      { decl with typeD = (a, Some LongLong, c),ii@ii2 }
  | (Middle3 Long,_ii),  {typeD = ((_a,Some LongLong,_c),_ii2); _} -> 
      decl +> warning "triplicate 'long'"

  | (Middle3 _,ii),     {typeD = ((_a,Some _,_c),_ii2); _} -> 
      raise (Semantic ("both long and short specified", List.hd ii))
  | (Middle3 x,ii),      {typeD = ((a,None,c),ii2); _} -> 
      { decl with typeD = (a, Some x,c),ii@ii2}

  | (Right3 _t,ii),     {typeD = ((_a,_b,Some _),_ii2); _} -> 
      raise (Semantic ("two or more data types", List.hd ii))
  | (Right3 t,ii),       {typeD = ((a,b,None),ii2); _}   -> 
      { decl with typeD = (a,b, Some t),ii@ii2}


let addQualif tq1 tq2 =
  match tq1, tq2 with
  | {const=Some _; _},   {const=Some _; _} -> 
      tq2 +> warning "duplicate 'const'"
  | {volatile=Some _; _}, {volatile=Some _; _} -> 
      tq2 +> warning "duplicate 'volatile'"
  | {const=Some x; _},   _ -> 
      { tq2 with const = Some x}
  | {volatile=Some x; _}, _ -> 
      { tq2 with volatile = Some x}
  | _ -> Common2.internal_error "there is no noconst or novolatile keyword"

let addQualifD qu qu2 = 
  { qu2 with qualifD = addQualif qu qu2.qualifD }


(*-------------------------------------------------------------------------- *)
(* Declaration/Function related *)
(*-------------------------------------------------------------------------- *)

(* stdC: type section, basic integer types (and ritchie)
 * To understand the code, just look at the result (right part of the PM) 
 * and go back.
 *)
let type_and_storage_from_decl  
  {storageD = st; 
   qualifD = qu; 
   typeD = (ty,iit); 
   inlineD = (inline,iinl);
  }  = 
 (qu,
   (match ty with 
 | (None, None, None)     -> 
   (* mine (originally default to int, but this looks like bad style) *)
   let decl = 
     { v_namei = None; v_type = qu, (BaseType Void, iit); v_storage = st } in
   raise (Semantic ("no type (could default to 'int')", 
                    List.hd (Lib_parsing_cpp.ii_of_any (OneDecl decl))))
 | (None, None, Some t)   -> (t, iit)
	 
 | (Some sign,   None, (None| Some (BaseType (IntType (Si (_,CInt))))))  -> 
     BaseType(IntType (Si (sign, CInt))), iit
 | ((None|Some Signed),Some x,(None|Some(BaseType(IntType (Si (_,CInt)))))) -> 
     BaseType(IntType (Si (Signed, [Short,CShort; Long, CLong; LongLong, CLongLong] +> List.assoc x))), iit
 | (Some UnSigned, Some x, (None| Some (BaseType (IntType (Si (_,CInt))))))-> 
     BaseType(IntType (Si (UnSigned, [Short,CShort; Long, CLong; LongLong, CLongLong] +> List.assoc x))), iit
 | (Some sign,   None, (Some (BaseType (IntType CChar))))   -> BaseType(IntType (Si (sign, CChar2))), iit
 | (None, Some Long,(Some(BaseType(FloatType CDouble))))    -> BaseType (FloatType (CLongDouble)), iit

 | (Some _,_, Some _) ->  
   raise (Semantic("signed, unsigned valid only for char and int", List.hd iit))
 | (_,Some _,(Some(BaseType(FloatType (CFloat|CLongDouble))))) -> 
   raise (Semantic ("long or short specified with floatint type", List.hd iit))
 | (_,Some Short,(Some(BaseType(FloatType CDouble)))) ->
     raise (Semantic ("the only valid combination is long double", List.hd iit))
       
 | (_, Some _, Some _) -> 
     (* mine *)
     raise (Semantic ("long, short valid only for int or float", List.hd iit)) 

     (* if do short uint i, then gcc say parse error, strange ? it is
      * not a parse error, it is just that we dont allow with typedef
      * either short/long or signed/unsigned. In fact, with
      * parse_typedef_fix2 (with et() and dt()) now I say too parse
      * error so this code is executed only when do short struct
      * {....} and never with a typedef cos now we parse short uint i
      * as short ident ident => parse error (cos after first short i
      * pass in dt() mode) *)
   )), st, (inline, iinl)
  

let type_and_register_from_decl decl = 
  let {storageD = st; _} = decl in 
  let (t,_storage, _inline) = type_and_storage_from_decl decl in
  match st with
  | NoSto -> t, None
  | Sto (Register, ii) -> t, Some ii
  | StoTypedef ii | Sto (_, ii) -> 
      raise (Semantic ("storage class specified for parameter of function", ii))

let fixNameForParam (name, ftyp) =
  match name with
  | None, [], IdIdent id -> id, ftyp
  | _ -> 
    let ii =  Lib_parsing_cpp.ii_of_any (Name name) +> List.hd in
    raise (Semantic ("parameter have qualifier", ii))

let type_and_storage_for_funcdef_from_decl decl =
  let (returnType, storage, _inline) = type_and_storage_from_decl decl in
  (match storage with
  | StoTypedef tok -> 
      raise (Semantic ("function definition declared 'typedef'", tok))
  | _x -> (returnType, storage)
  )

(* 
 * this function is used for func definitions (not declarations).
 * In that case we must have a name for the parameter.
 * This function ensures that we give only parameterTypeDecl with well
 * formed Classic constructor.
 * 
 * todo?: do we accept other declaration in ? 
 * so I must add them to the compound of the deffunc. I dont
 * have to handle typedef pb here cos C forbid to do VF f { ... }
 * with VF a typedef of func cos here we dont see the name of the
 * argument (in the typedef)
 *)
let (fixOldCDecl: fullType -> fullType) = fun ty ->
  match snd ty with
  | FunctionType ({ft_params=params;_}),_iifunc -> 
      (* stdC: If the prototype declaration declares a parameter for a
       * function that you are defining (it is part of a function
       * definition), then you must write a name within the declarator.
       * Otherwise, you can omit the name. *)
      (match Ast.unparen params with
      | [{p_name = None; p_type = ty2;_},_] -> 
          (match Ast.unwrap_typeC ty2 with
          | BaseType Void -> ty
          | _ ->
            (* less: there is some valid case actually, when use interfaces
             * and generic callbacks where specific instances do not
             * need the extra parameter (happens a lot in plan9).
             * Maybe this check is better done in a scheck for C.
              let info = Lib_parsing_cpp.ii_of_any (Type ty2) +> List.hd in
              pr2 (spf "SEMANTIC: parameter name omitted (but I continue) at %s"
                     (Parse_info.string_of_info info)
              );
            *)
              ty
          )
      | params ->
          (params +> List.iter (fun (param,_) ->
            match param with
            | {p_name = None; p_type = _ty2; _} -> 
              (* see above
              let info = Lib_parsing_cpp.ii_of_any (Type ty2) +> List.hd in
              (* if majuscule, then certainly macro-parameter *)
              pr2 (spf "SEMANTIC: parameter name omitted (but I continue) at %s"
                     (Parse_info.string_of_info info)
              );
              *)
              ()
	    | _ -> ()
           ));
          ty
      )
     (* todo? can we declare prototype in the decl or structdef,
      *  ... => length <> but good kan meme 
      *)
  | _ -> 
      (* gcc says parse error but I dont see why *)
      let ii = Lib_parsing_cpp.ii_of_any (Type ty) +> List.hd in
      raise (Semantic ("seems this is not a function", ii))

(* TODO: this is ugly ... use record! *)
let fixFunc ((name, ty, sto), cp) =
  match ty with
  | (aQ,(FunctionType ({ft_params=params; _} as ftyp),_iifunc)) ->
      (* it must be nullQualif, cos parser construct only this *)
      assert (aQ =*= nQ);

      (match Ast.unparen params with
      [{p_name= None; p_type = ty2;_}, _] ->
          (match Ast.unwrap_typeC ty2 with
          | BaseType Void -> ()
          (* failwith "internal errror: fixOldCDecl not good" *)
          | _ -> ()
          )
      | params -> 
          params +> List.iter (function 
          | ({p_name = Some _s;_}, _) -> ()
          (* failwith "internal errror: fixOldCDecl not good" *)
          | _ -> ()
          )
      ); 
      { f_name = name; f_type = ftyp; f_storage = sto; f_body = cp; }
  | _ -> 
      let ii = Lib_parsing_cpp.ii_of_any (Type ty) +> List.hd in
      raise (Semantic ("function definition without parameters", ii))

let fixFieldOrMethodDecl (xs, semicolon) =
  match xs with
  | [FieldDecl({
      v_namei = Some (name, ini_opt);
      v_type = (q, (FunctionType ft, ii_ft));
      v_storage = sto;
    }), _noiicomma] ->
      (* todo? define another type instead of onedecl? *)
      MemberDecl (MethodDecl ({
        v_namei = Some (name, None);
        v_type = (q, (FunctionType ft, ii_ft));
        v_storage = sto;
      }, 
      (match ini_opt with
      | None -> None
      | Some (EqInit(tokeq, InitExpr(C(Int "0"), iizero))) ->
          Some (tokeq, List.hd iizero)
      | _ ->
          raise (Semantic ("can't assign expression to method decl", semicolon))
      ), semicolon
      ))

  | _ -> MemberField (xs, semicolon)

(*-------------------------------------------------------------------------- *)
(* shortcuts *)
(*-------------------------------------------------------------------------- *)
let mk_e e ii = (e, ii)

let mk_funcall e1 args = 
  Call (e1, args)

let mk_constructor id (lp, params, rp) cp =
  let params, _hasdots = 
    match params with
    | Some (params, ellipsis) ->
        params, ellipsis
    | None -> [], None
  in
  let ftyp = {
    ft_ret = nQ, (BaseType Void, noii);
    ft_params= (lp, params, rp);
    ft_dots = None;
    (* TODO *)
    ft_const = None;
    ft_throw = None;
  }
  in
  { f_name = (None, noQscope, IdIdent id); f_type = ftyp; 
    f_storage = NoSto; f_body = cp
  }

let mk_destructor tilde id (lp, _voidopt, rp) exnopt cp =
  let ftyp = {
    ft_ret = nQ, (BaseType Void, noii);
    ft_params= (lp,  [], rp);
    ft_dots = None;
    ft_const = None;
    ft_throw = exnopt;
  }
  in
  { f_name = (None, noQscope, IdDestructor (tilde, id)); f_type = ftyp;
    f_storage = NoSto; f_body = cp;
  }

let opt_to_list_params params =
  match params with
  | Some (params, _ellipsis) ->
      (* todo? raise a warning that should not have ellipsis? *)
      params
  | None -> []
