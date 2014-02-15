open Common

open Ast_cpp

module Ast = Ast_cpp
module Flag = Flag_parsing_cpp

open Semantic_cpp

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common2.mk_pr2_wrappers Flag.verbose_parsing 

let warning s v = 
  if !Flag.verbose_parsing 
  then Common2.warning ("PARSING: " ^ s) v
  else v

(*****************************************************************************)
(* Parse helpers functions *)
(*****************************************************************************)

(*-------------------------------------------------------------------------- *)
(* Type related *)
(*-------------------------------------------------------------------------- *)

type shortLong = Short | Long | LongLong

type decl = { 
  storageD: storagebis wrap;
  typeD: ((sign option) * (shortLong option) * (typeCbis option)) wrap;
  qualifD: typeQualifier;
  inlineD: bool             wrap;
  (* note: have a full_info: parse_info list; to remember ordering
   * between storage, qualifier, type ? well this info is already in
   * the Ast_c.info, just have to sort them to get good order *)
} 

let nullDecl = {
  storageD = NoSto, [];
  typeD = (None, None, None), [];
  qualifD = Ast.nQ;
  inlineD = false, [];
}
let fake_pi = Parse_info.fake_token_location

let addStorageD  = function 
  | ((x,ii), ({storageD = (NoSto,[]); _} as v)) -> 
      { v with storageD = (x, [ii]) }
  | ((x,_ii), ({storageD = (y, _ii2); _} as v)) ->  
      if x = y then warning "duplicate storage classes" v
      else raise (Semantic ("multiple storage classes", fake_pi))

let addInlineD  = function 
  | ((true,ii), ({inlineD = (false,[]); _} as v)) -> 
      { v with inlineD=(true,[ii])}
  | ((true,_ii), ({inlineD = (true, _ii2); _} as v)) -> 
      warning "duplicate inline" v
  | _ -> raise Impossible


let addTypeD     = function 
  | ((Left3 Signed,_ii)   ,({typeD = ((Some Signed,  _b,_c),_ii2); _} as v)) -> 
      warning "duplicate 'signed'"   v
  | ((Left3 UnSigned,_ii) ,({typeD = ((Some UnSigned,_b,_c),_ii2); _} as v)) -> 
      warning "duplicate 'unsigned'" v
  | ((Left3 _,_ii),        ({typeD = ((Some _,_b,_c),_ii2); _} as _v)) -> 
      raise (Semantic ("both signed and unsigned specified", fake_pi))
  | ((Left3 x,ii),        ({typeD = ((None,b,c),ii2); _} as v))   -> 
      {v with typeD = (Some x,b,c),ii ++ ii2}

  | ((Middle3 Short,_ii),  ({typeD = ((_a,Some Short,_c),_ii2); _} as v)) -> 
      warning "duplicate 'short'" v

      
  (* gccext: long long allowed *)
  | ((Middle3 Long,ii),   ({typeD = ((a,Some Long ,c),ii2); _} as v)) -> 
      { v with typeD = (a, Some LongLong, c),ii++ii2 }
  | ((Middle3 Long,_ii),   ({typeD = ((_a,Some LongLong ,_c),_ii2); _} as v)) -> 
      warning "triplicate 'long'" v

  | ((Middle3 _,_ii),      ({typeD = ((_a,Some _,_c),_ii2); _} as _v)) -> 
      raise (Semantic ("both long and short specified", fake_pi))
  | ((Middle3 x,ii),      ({typeD = ((a,None,c),ii2); _} as v))  -> 
      {v with typeD = (a, Some x,c),ii++ii2}

  | ((Right3 _t,_ii),       ({typeD = ((_a,_b,Some _),_ii2); _} as _v)) -> 
      raise (Semantic ("two or more data types", fake_pi))
  | ((Right3 t,ii),       ({typeD = ((a,b,None),ii2); _} as v))   -> 
      {v with typeD = (a,b, Some t),ii++ii2}


let addQualif = function
  | ({const=Some _; _},   ({const=Some _; _} as x)) -> 
      warning "duplicate 'const'" x
  | ({volatile=Some _; _},({volatile=Some _; _} as x))-> 
      warning "duplicate 'volatile'" x
  | ({const=Some x; _},    v) -> 
      {v with const=Some x}
  | ({volatile=Some x; _}, v) -> 
      {v with volatile=Some x}
  | _ -> Common2.internal_error "there is no noconst or novolatile keyword"

let addQualifD (qu, ({qualifD = v; _} as x)) =
  { x with qualifD = addQualif (qu, v) }


(*-------------------------------------------------------------------------- *)
(* Declaration/Function related *)
(*-------------------------------------------------------------------------- *)

(* stdC: type section, basic integer types (and ritchie)
 * To understand the code, just look at the result (right part of the PM) 
 * and go back.
 *)
let (fixDeclSpecForDecl: decl -> (fullType * (storage wrap)))  = function
 {storageD = (st,iist); 
  qualifD = qu; 
  typeD = (ty,iit); 
  inlineD = (inline,iinl);
  } -> 
  (
   (qu,
   (match ty with 
 | (None,None,None)       -> warning "type defaults to 'int'" (defaultInt, [])
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
     (*mine*)
     raise (Semantic ("signed, unsigned valid only for char and int", fake_pi))
 | (_,Some _,(Some(BaseType(FloatType (CFloat|CLongDouble))))) -> 
     raise (Semantic ("long or short specified with floatint type", fake_pi))
 | (_,Some Short,(Some(BaseType(FloatType CDouble)))) ->
     raise (Semantic ("the only valid combination is long double", fake_pi))
       
 | (_, Some _, Some _) -> 
     (* mine *)
     raise (Semantic ("long, short valid only for int or float", fake_pi)) 

     (* if do short uint i, then gcc say parse error, strange ? it is
      * not a parse error, it is just that we dont allow with typedef
      * either short/long or signed/unsigned. In fact, with
      * parse_typedef_fix2 (with et() and dt()) now I say too parse
      * error so this code is executed only when do short struct
      * {....} and never with a typedef cos now we parse short uint i
      * as short ident ident => parse error (cos after first short i
      * pass in dt() mode) *)
   ))
     ,((st, inline),iist++iinl)
  )

let fixDeclSpecForParam = function ({storageD = (st,iist); _} as r) -> 
  let ((_qu,_ty) as v,_st) = fixDeclSpecForDecl r in
  match st with
  | (Sto Register) -> v, Some (List.hd iist)
  | NoSto -> v, None
  | _ -> 
      raise 
        (Semantic ("storage class specified for parameter of function", 
                  fake_pi))

let fixNameForParam (name, ftyp) =
  match name with
  | None, [], IdIdent id -> id, ftyp
  | _ -> 
      raise (Semantic ("parameter have qualifier", fake_pi))

let fixDeclSpecForFuncDef x =
  let (returnType,storage) = fixDeclSpecForDecl x in
  (match fst (unwrap storage) with
  | StoTypedef -> 
      raise (Semantic ("function definition declared 'typedef'", fake_pi))
  | _x -> (returnType, storage)
  )

(* parameter: this function is used where we give parameters only when
 * in func DEFINITION not in func DECLARATION. We must have a name.
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
          | BaseType Void ->
              ty
          | _ -> 
              pr2 ("SEMANTIC:parameter name omitted, but I continue");
              ty
          )

      | params -> 
          (params +> List.iter (fun (param,_) ->
            match param with
            | {p_name = None;_} -> 
              (* if majuscule, then certainly macro-parameter *)
              pr2 ("SEMANTIC:parameter name omitted, but I continue"); 
	  | _ -> ()
          ));
          ty
      )
        (* todo? can we declare prototype in the decl or structdef,
           ... => length <> but good kan meme *)
  | _ -> 
      (* gcc say parse error but dont see why *)
      raise (Semantic ("seems this is not a function", fake_pi)) 

(* TODO: this is ugly ... use record! *)
let fixFunc = function
  | (name, (aQ, (FunctionType ({ft_params=params; _} as ftyp),_iifunc)),sto),cp->
      (* it must be nullQualif, cos parser construct only this *)
      assert (aQ =*= nQ);

      (match Ast.unparen params with
      [{p_name= None; p_type = ty2;_}, _] ->
          (match Ast.unwrap_typeC ty2 with
          | BaseType Void -> ()
          | _ ->
                (* failwith "internal errror: fixOldCDecl not good" *)
              ()
          )
      | params -> 
          params +> List.iter (function 
          | ({p_name = Some _s;_}, _) -> ()
	  | _ -> ()
                (* failwith "internal errror: fixOldCDecl not good" *)
          )
      ); 
      { f_name = name; f_type = ftyp; f_storage = sto; f_body = cp; }
  | _ -> 
      raise 
        (Semantic 
            ("you are trying to do a function definition but you dont give " ^
             "any parameter", fake_pi))

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
      | Some (EqInit(tokeq, InitExpr( ((C(Int("0"))), iizero)) )) ->
          Some (tokeq, List.hd iizero)
      | _ ->
          raise (Semantic ("can't assign expression to method decl", fake_pi))
      ), semicolon
      ))

  | _ -> MemberField (xs, semicolon)

(*-------------------------------------------------------------------------- *)
(* shortcuts *)
(*-------------------------------------------------------------------------- *)
let mk_e e ii = (e, ii)

let mk_funcall e1 args = 
  match e1 with
  | (Ident (name, _idinfo)), _ii_empty ->
      FunCallSimple (name, args)
  | _ -> 
      FunCallExpr (e1, args)

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
    f_storage = (NoSto, false), noii; f_body = cp
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
    f_storage = (NoSto, false), noii; f_body = cp;
  }

let opt_to_list_params params =
  match params with
  | Some (params, _ellipsis) ->
      (* todo? raise a warning that should not have ellipsis? *)
      params
  | None -> []
