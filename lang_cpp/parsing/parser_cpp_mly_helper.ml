open Common

open Ast_cpp

module Ast = Ast_cpp
module Flag = Flag_parsing_cpp

open Semantic_cpp

module LP = Lexer_parser_cpp

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag.verbose_parsing 

let warning s v = 
  if !Flag.verbose_parsing 
  then Common.warning ("PARSING: " ^ s) v
  else v

(*****************************************************************************)
(* Parse helpers functions *)
(*****************************************************************************)

(*-------------------------------------------------------------------------- *)
(* Type related *)
(*-------------------------------------------------------------------------- *)

type shortLong      = Short  | Long | LongLong

type decl = { 
  storageD: storagebis wrap;
  typeD: ((sign option) * (shortLong option) * (typeCbis option)) wrap;
  qualifD: typeQualifierbis wrap;
  inlineD: bool             wrap;
  (* note: have a full_info: parse_info list; to remember ordering
   * between storage, qualifier, type ? well this info is already in
   * the Ast_c.info, just have to sort them to get good order *)
} 

let nullDecl = {
  storageD = NoSto, [];
  typeD = (None, None, None), [];
  qualifD = Ast.nullQualif;
  inlineD = false, [];
}
let fake_pi = Parse_info.fake_parse_info

let addStorageD  = function 
  | ((x,ii), ({storageD = (NoSto,[]); _} as v)) -> 
      { v with storageD = (x, [ii]) }
  | ((x,ii), ({storageD = (y, ii2); _} as v)) ->  
      if x = y then warning "duplicate storage classes" v
      else raise (Semantic ("multiple storage classes", fake_pi))

let addInlineD  = function 
  | ((true,ii), ({inlineD = (false,[]); _} as v)) -> 
      { v with inlineD=(true,[ii])}
  | ((true,ii), ({inlineD = (true, ii2); _} as v)) -> 
      warning "duplicate inline" v
  | _ -> raise Impossible


let addTypeD     = function 
  | ((Left3 Signed,ii)   ,({typeD = ((Some Signed,  b,c),ii2); _} as v)) -> 
      warning "duplicate 'signed'"   v
  | ((Left3 UnSigned,ii) ,({typeD = ((Some UnSigned,b,c),ii2); _} as v)) -> 
      warning "duplicate 'unsigned'" v
  | ((Left3 _,ii),        ({typeD = ((Some _,b,c),ii2); _} as _v)) -> 
      raise (Semantic ("both signed and unsigned specified", fake_pi))
  | ((Left3 x,ii),        ({typeD = ((None,b,c),ii2); _} as v))   -> 
      {v with typeD = (Some x,b,c),ii ++ ii2}

  | ((Middle3 Short,ii),  ({typeD = ((a,Some Short,c),ii2); _} as v)) -> 
      warning "duplicate 'short'" v

      
  (* gccext: long long allowed *)
  | ((Middle3 Long,ii),   ({typeD = ((a,Some Long ,c),ii2); _} as v)) -> 
      { v with typeD = (a, Some LongLong, c),ii++ii2 }
  | ((Middle3 Long,ii),   ({typeD = ((a,Some LongLong ,c),ii2); _} as v)) -> 
      warning "triplicate 'long'" v

  | ((Middle3 _,ii),      ({typeD = ((a,Some _,c),ii2); _} as _v)) -> 
      raise (Semantic ("both long and short specified", fake_pi))
  | ((Middle3 x,ii),      ({typeD = ((a,None,c),ii2); _} as v))  -> 
      {v with typeD = (a, Some x,c),ii++ii2}

  | ((Right3 t,ii),       ({typeD = ((a,b,Some _),ii2); _} as _v)) -> 
      raise (Semantic ("two or more data types", fake_pi))
  | ((Right3 t,ii),       ({typeD = ((a,b,None),ii2); _} as v))   -> 
      {v with typeD = (a,b, Some t),ii++ii2}


let addQualif = function
  | ({const=true; _},   ({const=true; _} as x)) -> 
      warning "duplicate 'const'" x
  | ({volatile=true; _},({volatile=true; _} as x))-> 
      warning "duplicate 'volatile'" x
  | ({const=true; _},    v) -> 
      {v with const=true}
  | ({volatile=true; _}, v) -> 
      {v with volatile=true}
  | _ -> internal_error "there is no noconst or novolatile keyword"

let addQualifD ((qu,ii), ({qualifD = (v,ii2); _} as x)) =
  { x with qualifD = (addQualif (qu, v),ii::ii2) }


(*-------------------------------------------------------------------------- *)
(* Declaration/Function related *)
(*-------------------------------------------------------------------------- *)


(* stdC: type section, basic integer types (and ritchie)
 * To understand the code, just look at the result (right part of the PM) 
 * and go back.
 *)
let (fixDeclSpecForDecl: decl -> (fullType * (storage wrap)))  = function
 {storageD = (st,iist); 
  qualifD = (qu,iiq); 
  typeD = (ty,iit); 
  inlineD = (inline,iinl);
  } -> 
  (
   ((qu, iiq),
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
  let ((qu,ty) as v,_st) = fixDeclSpecForDecl r in
  match st with
  | (Sto Register) -> (v, true), iist
  | NoSto -> (v, false), iist
  | _ -> 
      raise 
        (Semantic ("storage class specified for parameter of function", 
                  fake_pi))

let fixDeclSpecForFuncDef x =
  let (returnType,storage) = fixDeclSpecForDecl x in
  (match fst (unwrap storage) with
  | StoTypedef -> 
      raise (Semantic ("function definition declared 'typedef'", fake_pi))
  | x -> (returnType, storage)
  )

(* parameter: (this is the context where we give parameter only when
 * in func DEFINITION not in funct DECLARATION) We must have a name.
 * This function ensure that we give only parameterTypeDecl with well
 * formed Classic constructor todo?: do we accept other declaration
 * in ? so I must add them to the compound of the deffunc. I dont
 * have to handle typedef pb here cos C forbid to do VF f { ... }
 * with VF a typedef of func cos here we dont see the name of the
 * argument (in the typedef)
 *)
let (fixOldCDecl: fullType -> fullType) = fun ty ->
  match snd ty with
  | ((FunctionType (fullt, (params, (b, iib)))),iifunc) -> 

      (* stdC: If the prototype declaration declares a parameter for a
       * function that you are defining (it is part of a function
       * definition), then you must write a name within the declarator.
       * Otherwise, you can omit the name. *)
      (match params with
      | [((reg, None, ((_qua, (BaseType Void,_)))),_), _] ->  
          ty
      | params -> 
          (params +> List.iter (function 
          | (((b, None, _),  ii1),ii2) -> 
              (* if majuscule, then certainly macro-parameter *)
              pr2 ("SEMANTIC:parameter name omitted, but I continue"); 
	  | _ -> ()
          );
           ty)
      )
        (* todo? can we declare prototype in the decl or structdef,
           ... => length <> but good kan meme *)
  | _ -> 
      (* gcc say parse error but dont see why *)
      raise (Semantic ("seems this is not a function", fake_pi)) 


let fixFunc = function
  | ((
      (s,iis), 
      (nQ, (FunctionType (fullt, (params,bool)),iifunc)), 
      (st,iist)
    ), 
    (cp,iicp)) -> 
      let iistart = Ast.fakeInfo () in
      assert (nQ =*= nullQualif);
      (match params with
      | [((reg, None, ((_qua, (BaseType Void,_)))),_), _] ->  ()
      | params -> 
          params +> List.iter (function 
          | (((bool, Some s, fullt), _), _) -> ()
	  | _ -> ()
                (* failwith "internal errror: fixOldCDecl not good" *)
          ));
      (* it must be nullQualif,cos parser construct only this*)
      (s, (fullt, (params, bool)), st, cp), 
      ([iis]++iifunc++iicp++[iistart]++iist) 
  | _ -> 
      raise 
        (Semantic 
            ("you are trying to do a function definition but you dont give " ^
             "any parameter", fake_pi))

(*-------------------------------------------------------------------------- *)
(* shortcuts *)
(*-------------------------------------------------------------------------- *)

let mk_e e ii = ((e, Ast.noType()), ii)
(* todo: let mk_e e ii = Ast_c.mk_e e ii *)

let mk_funcall e1 args = 
  match e1 with
  | (Ident (name, _idinfo), _t), ii_empty ->
      FunCallSimple (name, args)
  | _ -> 
      FunCallExpr (e1, args)

(*let mk_string_wrap (s,info) = (s, [info])*)
