(* Yoann Padioleau
 * 
 * Copyright (C) 2002 Yoann Padioleau
 * Copyright (C) 2006-2007 Ecole des Mines de Nantes
 * Copyright (C) 2008-2009 University of Urbana Champaign
 * Copyright (C) 2010 Facebook
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

(*****************************************************************************)
(* The AST C++ related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)

type pinfo = 
  (* Present both in ast and list of tokens *)
  | OriginTok of Common.parse_info
  (* Present only in ast and generated after parsing. Used mainly
   * by Julia, to add stuff at virtual places, beginning of func or decl *)
  | FakeTok of string * virtual_position
  (* Present both in ast and list of tokens.  *)
  | ExpandedTok of Common.parse_info * virtual_position
  (* Present neither in ast nor in list of tokens
   * but only in the '+' of the mcode of some tokens. Those kind of tokens
   * are used to be able to use '=' to compare big ast portions.
   *)
  | Ab
  and virtual_position = Common.parse_info * int (* character offset *)

type info = { 
  (* contains among other things the position of the token through
   * the Common.parse_info embedded inside the pinfo type.
   *)
  pinfo : pinfo;
  (* todo? token_info : sometimes useful to know what token it was *)
  }
and tok = info

(* a shortcut to annotate some information with token/position information *)
and 'a wrap  = 'a * info list (* TODO CHANGE *)

and 'a paren   = tok * 'a * tok
and 'a brace   = tok * 'a * tok
and 'a bracket = tok * 'a * tok 
and 'a comma_list = 'a wrap list


(* ------------------------------------------------------------------------- *)
(* Ident, name, scope qualifier *)
(* ------------------------------------------------------------------------- *)
(* c++ext: in C 'name' and 'ident' are equivalent and equal to just string.
 * In C++ ident can have a complex form like A::B::list<int>::size.
 * I use Q for qualified. I also have a special type to make the difference
 * between intermediate idents (the classname or template_id) and final ident, 
 * but note that sometimes final ident are also classname and can have final
 * template_id.
 * Sometimes some elements are not allowed at certain places, for 
 * instance converters can not have an associated Qtop. But I prefer
 * to simplify the type again.
 *)
and name = qtop option * qualifier list * ident  

 and ident = identbis wrap 
   and identbis = 
     (* function name, macro name, variable, classname, enumname, namespace *)
     | IdIdent of string 
     | IdOperator of operator
     | IdConverter of fullType
     | IdDestructor of string (* todo: ident or template_id here too *)
     | IdTemplateId of string * template_arguments

 and qualifier = qualifierbis wrap (* s :: *)
   and qualifierbis = 
     | QClassname of string (* classname or namespacename *)
     | QTemplateId of string * template_arguments

 and qtop = qtobis wrap (* :: *)
   and qtobis = QTop

 and template_argument = (fullType, expression) Common.either
 and template_arguments = template_argument comma_list

 (* special cases *)
 and class_name     = name (* only IdIdent or IdTemplateId *)
 and namespace_name = name (* only IdIdent *)
 and typedef_name   = name (* only IdIdent *)
 and enum_name      = name (* only IdIdent *)

 and ident_name = name (* only IdIdent *)

(* ------------------------------------------------------------------------- *)
(* C Type *)
(* ------------------------------------------------------------------------- *)
(* Could have more precise type in fullType, in expression, etc, but
 * it requires to do too much things in parsing such as checking no
 * conflicting structname, computing value, etc. Better to separate
 * concern, so I put '=>' to mean what we would really like. In fact
 * what we really like is defining another fullType, expression, etc
 * from scratch, because many stuff are just sugar.
 * 
 * invariant: Array and FunctionType have also typeQualifier but they
 * dont have sense. I put this to factorise some code. If you look in
 * grammar, you see that we can never specify const for the array
 * himself (but we can do it for pointer).
 * 
 * 
 * Because of ExprStatement, we can have more 'new scope' events, but
 * rare I think. For instance with 'array of constExpression' there can
 * have an exprStatement and a new (local) struct defined. Same for
 * Constructor.
 * 
 * Some stuff are tagged semantic: which means that they are computed
 * after parsing. *)


and fullType = typeQualifier * typeC
and  typeC = typeCbis wrap

and typeCbis =
  | BaseType        of baseType

  | Pointer         of fullType
  | Reference       of fullType (* c++ext: *)

  | Array           of constExpression option * fullType
  | FunctionType    of functionType

  | Enum            of string option * enumType    
  | StructUnion     of class_definition (* c++ext: bigger type now *)

  | EnumName        of string (*enum_name*)
  | StructUnionName of structUnion * string (*ident_name*)

  (* c++note: TypeName can also correspond in fact to a classname or enumname *)
  | TypeName   of string(*typedef_name*) * fullType option (* semantic: filled later *)
  (* c++ext: *)
  | TypeTemplate of string(*ident_name*) * template_arguments

  (* gccext: TypeOfType may seems useless, why declare a __typeof__(int)
   * x; ? But when used with macro, it allows to fix a problem of C which
   * is that type declaration can be spread around the ident. Indeed it
   * may be difficult to have a macro such as '#define macro(type,
   * ident) type ident;' because when you want to do a macro(char[256],
   * x), then it will generate invalid code, but with a '#define
   * macro(type, ident) __typeof(type) ident;' it will work. *)
  | TypeOfExpr of expression  
  | TypeOfType of fullType    

  (* c++ext: only to disambiguate I think *)
  | TypenameKwd of fullType (* in practice either TypeName of TemplateName *)
  (* forunparser: *)
  | ParenType of fullType 

      
(* -------------------------------------- *)    
     and  baseType = Void 
                   | IntType   of intType 
		   | FloatType of floatType


	  (* stdC: type section 
           * add  a | SizeT ?
           * note: char and signed char are semantically different!! 
           *)
          and intType   = CChar (* obsolete? | CWchar  *)
	                | Si of signed
                        (* c++ext: maybe could be put in baseType instead ? *)
                        | CBool
                        | WChar_t 

           and signed = sign * base
            and base = CChar2 | CShort | CInt | CLong | CLongLong (* gccext: *)
            and sign = Signed | UnSigned

          and floatType = CFloat | CDouble | CLongDouble


     (* -------------------------------------- *)    
     (* c++ext: and structType, cf now below *)

     (* -------------------------------------- *)    
     and enumType = (string * constExpression option) wrap (* s = *) 
                    comma_list
                   (* => string * int list *)


     (* -------------------------------------- *)    
     (* return * (params * has "...") 
      * c++ext: todo now const, throw spec, etc
     *)
     and functionType = fullType * (parameterType comma_list * bool wrap)
        and parameterType = (bool * string option * fullType) wrap (* reg s *)
              (* => (bool (register) * fullType) list * bool *)


and typeQualifier = typeQualifierbis wrap 
and typeQualifierbis = {const: bool; volatile: bool}


(* ------------------------------------------------------------------------- *)
(* C expression *)
(* ------------------------------------------------------------------------- *)
and expression = (expressionbis * fullType option ref (* semantic: *)) wrap
and expressionbis = 

  (* Ident can be a enumeration constant, a simple variable, a name of a func.
   * With cppext, Ident can also be the name of a macro. Sparse says
   * that "an identifier with a meaning is a symbol". 
   * With c++ Ident is now a 'name' instead of a 'string' and can correspond 
   * to an operator name.
   *)
  | Ident          of name  (* todo? more semantic info such as LocalFunc *)
  | Constant       of constant                                  
  (* c++ext: *)
  | This 

  | FunCall        of expression * argument comma_list

  (* gccext: x ? /* empty */ : y <=> x ? x : y; *)
  | CondExpr       of expression * expression option * expression

  (* should be considered as statements, bad C langage *)
  | Sequence       of expression * expression                   
  | Assignment     of expression * assignOp * expression        

  | Postfix        of expression * fixOp                        
  | Infix          of expression * fixOp                        
  | Unary          of expression * unaryOp                      
  | Binary         of expression * binaryOp * expression        

  | ArrayAccess    of expression * expression                   

   (* The Pt is redundant normally, could replace it by DeRef RecordAccess *)
  | RecordAccess   of expression * name
  | RecordPtAccess of expression * name

  (* c++ext: note that second param is an expression, not a name *)
  | RecordStarAccess   of expression * expression
  | RecordPtStarAccess of expression * expression

  | SizeOfExpr     of expression
  | SizeOfType     of fullType

  | Cast          of fullType * expression
  (* c++ext: *)
  | CplusplusCast of cast_operator * fullType * expression   (* c++ext: *)

  (* gccext: *)        
  | StatementExpr of compound wrap (* ( )     new scope *) 
  | GccConstructor  of fullType * initialiser comma_list

  (* c++ext: *)
  | ConstructedObject of fullType * argument comma_list
  | TypeIdOfExpr     of expression
  | TypeIdOfType     of fullType

  | New (* todo: of placement, init, etc *)
  | Delete of expression * qtop option
  | DeleteArray of expression * qtop option

  | Throw of expression option 

  (* forunparser: *)
  | ParenExpr of expression 



  (* cppext: normmally just expression *)
  and argument = (expression, wierd_argument) Common.either
   and wierd_argument = 
       | ArgType of parameterType
       | ArgAction of action_macro
      and action_macro = 
         | ActMisc of tok list


  (* I put string for Int and Float because int would not be enough because
   * OCaml int are 31 bits. So simpler to do string. Same reason to have
   * string instead of int list for the String case.
   * 
   * note: that -2 is not a constant, it is the unary operator '-'
   * applied to constant 2. So the string must represent a positive
   * integer only. *)

  and constant = 
    | String of (string * isWchar) 
    | MultiString  (* can contain MacroString *)
    | Char   of (string * isWchar) (* normally it is equivalent to Int *)
    | Int    of (string  (* * intType*)) 
    | Float  of (string * floatType)
    (* c++ext: *)
    | Bool of bool

    and isWchar = IsWchar | IsChar

  (* gccext: GetRefLabel, via &&label notation *)
  and unaryOp  = GetRef | DeRef | UnPlus |  UnMinus | Tilde | Not | GetRefLabel
  and assignOp = SimpleAssign | OpAssign of arithOp
  and fixOp    = Dec | Inc

  and binaryOp = Arith of arithOp | Logical of logicalOp

       and arithOp   = 
         | Plus | Minus | Mul | Div | Mod
         | DecLeft | DecRight 
         | And | Or | Xor

       and logicalOp = 
         | Inf | Sup | InfEq | SupEq 
         | Eq | NotEq 
         | AndLog | OrLog

  (* c++ext: used elsewhere but prefer to define it close to other operators *)
  and ptrOp = PtrStarOp | PtrOp
  and allocOp = NewOp | DeleteOp | NewArrayOp | DeleteArrayOp
  and accessop = ParenOp | ArrayOp
  and operator = 
    | BinaryOp of binaryOp
    | AssignOp of assignOp
    | FixOp of fixOp
    | PtrOpOp of ptrOp
    | AccessOp of accessop
    | AllocOp of allocOp
    | UnaryTildeOp
    | UnaryNotOp
    | CommaOp


 (* c++ext: *)
  and cast_operator =
    | Static_cast
    | Dynamic_cast
    | Const_cast
    | Reinterpret_cast

 and constExpression = expression (* => int *)


(* ------------------------------------------------------------------------- *)
(* C statement *)
(* ------------------------------------------------------------------------- *)
(* note: that assignement is not a statement but an expression;
 * wonderful C langage.
 * 
 * note: I use 'and' for type definition cos gccext allow statement as
 * expression, so need mutual recursive type definition. *)

and statement = statementbis wrap 
and statementbis = 
  | Compound      of compound   (* new scope *)
  | ExprStatement of exprStatement
  | Labeled       of labeled
  | Selection     of selection (* have fakeend *)
  | Iteration     of iteration (* have fakeend *)
  | Jump          of jump

  (* c++ext: old: simplify cocci: only at beginning of a compound normally *)
  | DeclStmt  of block_declaration 
  (* gccext: *)
  | NestedFunc of definition
  (* cppext: *)
  | MacroStmt
  (* c++ext: *)
  | Try of compound wrap * handler list


  (* cppext: c++ext:
   * old: compound = (declaration list * statement list) 
   * old: (declaration, statement) either list 
   * Simplify cocci to just have statement list, by integrating Decl in stmt.
   *)
  and compound = statement_sequencable list 

  (* cppext: easier to put at statement_list level than statement level *)
  and statement_sequencable = 
    | StmtElem of statement
    (* cppext: *) 
    | CppDirectiveStmt of cpp_directive
    | IfdefStmt of ifdef_directive (* * statement list *)

  and exprStatement = expression option



  and labeled = Label   of string * statement
              | Case    of expression * statement 
              | CaseRange of expression * expression * statement (* gccext: *)
	      |	Default of statement

 (* for Switch, need check that all elements in the compound start 
  * with a case:, otherwise unreachable code.
  *)
  and selection     = 
   | If     of expression * statement * statement
   | Switch of expression * statement 

  and iteration     = 
    | While   of expression * statement
    | DoWhile of statement * expression
    | For     of exprStatement wrap * exprStatement wrap * exprStatement wrap *
                 statement
    | MacroIteration of string * argument comma_list * statement

  and jump  = Goto of string
            | Continue | Break 
            | Return   | ReturnExpr of expression
            | GotoComputed of expression (* gccext: goto *exp ';' *)


  (* c++ext: *)
  and handler = exception_declaration wrap (* catch () *) * compound wrap
   and exception_declaration = 
     | ExnDeclEllipsis of info
     | ExnDecl of parameterType

(* ------------------------------------------------------------------------- *)
(* Block Declaration *)
(* ------------------------------------------------------------------------- *)

(* a.k.a declaration_statement *)
and block_declaration = block_declarationbis wrap
 and block_declarationbis =
  | SimpleDecl of var_declaration (* include class_declaration *)

  (* c++ext: using namespace *)
  | UsingDecl of name
  | UsingDirective of namespace_name
  | NameSpaceAlias of string * namespace_name

  (* gccext: *)
  | Asm of asmbody



  (* gccext: *)
  and asmbody = tok list (* string list *) * colon wrap (* : *) list
      and colon = Colon of colon_option comma_list
      and colon_option = colon_optionbis wrap
          and colon_optionbis = ColonMisc | ColonExpr of expression




(* ------------------------------------------------------------------------- *)
(* Simple Declaration *)
(* ------------------------------------------------------------------------- *)
(* (string * ...) option cos can have empty declaration or struct tag 
 * declaration.
 *   
 * Before I had Typedef constructor, but why make this special case and not 
 * have StructDef, EnumDef, ... so that 'struct t {...} v' will generate 2 
 * declarations ? So I try to generalise and not have not Typedef too. This
 * requires more work in parsing. Better to separate concern.
 * 
 * Before the need for unparser, I didn't have a DeclList but just a Decl.
 *
 * I am not sure what it means to declare a prototype inline, but gcc
 * accepts it. 
 * 
 * note: var_declaration include prototype declaration.
 *)
  
and var_declaration = 
  | DeclList of onedecl comma_list wrap (* ; fakestart sto *)
  (* cppext: todo? now factorize with MacroTop ?  *)
  | MacroDecl of (string * argument comma_list) wrap

     and onedecl = 
       ((string(*name*) * initialiser option) wrap (* = *) option) * 
         fullType * 
         storage

     and storage       = storagebis * bool (* inline or not, gccext: *)
     and storagebis    = NoSto | StoTypedef | Sto of storageClass
     and storageClass  = Auto  | Static | Register | Extern (* Mutable? *)
     (* Friend ???? *)

     (*c++ext: *)
     and func_specifier =
       | Inline
       | Virtual



     and initialiser = initialiserbis wrap
       and initialiserbis = 
          | InitExpr of expression 
          | InitList of initialiser comma_list 
          (* gccext: *)
          | InitDesignators of designator list * initialiser
          | InitFieldOld  of string * initialiser
          | InitIndexOld  of expression * initialiser

       (* ex: [2].y = x,  or .y[2]  or .y.x. They can be nested *)
       and designator = designatorbis wrap 
        and designatorbis = 
            | DesignatorField of string 
            | DesignatorIndex of expression
            | DesignatorRange of expression * expression
        
(* ------------------------------------------------------------------------- *)
(* Function definition *)
(* ------------------------------------------------------------------------- *)
(* Normally we should define another type functionType2 because there 
 * are more restrictions on what can define a function than a pointer 
 * function. For instance a function declaration can omit the name of the
 * parameter wheras a function definition can not. But, in some cases such
 * as 'f(void) {', there is no name too, so I simplified and reused the 
 * same functionType type for both declaration and function definition.
 *)
and definition = (string(*name*) * functionType * storage * compound) 
                 wrap (* s ( ) { } fakestart sto *)


(* ------------------------------------------------------------------------- *)
(* Struct/Class *)
(* ------------------------------------------------------------------------- *)

(* c++ext: the ident can be a template_id when do template specialization *)
and class_definition = 
 (structUnion * ident_name(*class_name??*) option * base_clause comma_list option * 
 class_member_sequencable list (* new scope *))
 wrap (* struct { } ':' of bopt *)

  and structUnion =
    | Struct
    | Union
    (* c++ext: *)
    | Class

  (* used in inheritance spec (base_clause) and class_member *)
  and access_spec =
    | Public
    | Private
    | Protected

  (* was called field wrap before *)
  and class_member = class_memberbis wrap
   and class_memberbis =
     | Access of access_spec (* could put outside and take class_member list *)

     | DeclarationField of field_declaration
    
         
     | Method of definition
     (* MethodDecl is inside field_declaration *)

     | Constructor of definition * bool (* explicit *) (* * TODO chain_call*)
     | Destructor of definition
     | ConstructorDecl of parameterType comma_list * bool (* explicit *)
     | DestructorDecl of name(*IdDestructor*) * bool (* virtual*) (* ( ) void_opt *)
         
     | QualifiedIdInClass of name
         
     | TemplateDeclInClass of (template_parameters * declaration)
     | UsingDeclInClass of name

     | EmptyField  (* gccext: and maybe c++ext: ';' *)



      (* before unparser, I didn't have a FieldDeclList but just a Field. *)
      and field_declaration = 
       | FieldDeclList of fieldkind comma_list (* , *) wrap (* ';' sto *)

      (* At first I thought that a bitfield could be only Signed/Unsigned.
       * But it seems that gcc allow char i:4. C rule must say that you
       * can cast into int so enum too, ... 
       * c++ext: FieldDecl was before Simple of string option * fullType
       * but in c++ field can also have storage so now reuse ondecl.
       *)
        and fieldkind = fieldkindbis wrap (* s :  or pure spec *)
          and fieldkindbis = 
            | FieldDecl of onedecl
            (* = 0 at end before the ';' *)
            | MethodDecl of onedecl * bool (* pure virtual method *)
            | BitField of string option * fullType * constExpression
               (* fullType => BitFieldInt | BitFieldUnsigned *) 
   
  and class_member_sequencable = 
    | ClassElem of class_member
    (* cppext: *)
    | CppDirectiveStruct of cpp_directive
    | IfdefStruct of ifdef_directive (* * field list *)


  and base_clause = base_clausebis wrap (* virtual and access spec *)
    and base_clausebis = 
         class_name * bool (* virtual inheritance *) * access_spec option



  
(* ------------------------------------------------------------------------- *)
(* Declaration, in c++ sense *)
(* ------------------------------------------------------------------------- *)

and declaration = declarationbis wrap
 and declarationbis = 
  | Declaration of block_declaration (* include class definition *)

  | Definition of definition   (* include method definition *)
  (* c++ext: *)
  | ConstructorTop of definition * bool (* explicit *) (* * chain_call*)
  | DestructorTop of definition

  | TemplateDecl of (template_parameters * declaration)
  | TemplateSpecialization of declaration

  (* the list can be empty *)
  | ExternC of declaration
  | ExternCList of declaration_sequencable list

  (* the list can be empty *)
  | NameSpace       of string * declaration_sequencable list
  | NameSpaceExtend of string * declaration_sequencable list (* after have semantic info *)
  | NameSpaceAnon   of          declaration_sequencable list



 and template_parameters = template_parameter comma_list
  and template_parameter = parameterType (* todo more *)

  (* TODO *)
  (* cppext: easier to put at statement_list level than statement level *)
  and declaration_sequencable = 
    | DeclElem of declaration
    (* cppext: *) 
    | CppDirectiveDecl of cpp_directive
    | IfdefDecl of ifdef_directive (* * statement list *)


(* ------------------------------------------------------------------------- *)
(* cpp, #define and #include body *)
(* ------------------------------------------------------------------------- *)
and cpp_directive =
  | Include of includ 
  | Define of define 
  | Undef of string * tok list
  | PragmaAndCo of tok list

(* to specialize if someone need more info *)
and ifdef_directive = 
  | IfdefDirective of tok list
(* or and 'a ifdefed = 'a list wrap (* ifdef elsif else endif *) *)


(* cppext *) 
and define = string wrap * define_body   (* #define s *)
 and define_body = define_kind * define_val
   and define_kind =
   | DefineVar
   | DefineFunc   of ((string wrap) comma_list) wrap
   and define_val = 
     | DefineExpr of expression
     | DefineStmt of statement
     | DefineType of fullType
     | DefineDoWhileZero of statement wrap (* do { } while(0) *)
     | DefineFunction of definition
     | DefineInit of initialiser (* in practice only { } with possible ',' *)
     | DefineText of string wrap
     | DefineEmpty

     | DefineTodo



and includ = inc_file wrap (* #include s *) * 
  (unit (* old: include_rel_pos option ref *) * bool (* is in ifdef, cf -test incl *) )
 and inc_file = 
  | Local    of inc_elem list
  | NonLocal of inc_elem list
  | Wierd of string (* ex: #include SYSTEM_H *)
  and inc_elem = string

(* Cocci: to tag the first of #include <xx/> and last of #include <yy/>
 * 
 * The first_of and last_of store the list of prefixes that was
 * introduced by the include. On #include <a/b/x>, if the include was
 * the first in the file, it would give in first_of the following
 * prefixes a/b/c; a/b/; a/ ; <empty> 
 * 
 * This is set after parsing, in cocci.ml, in update_rel_pos.
 and include_rel_pos = { 
  first_of : string list list;
  last_of :  string list list;
 }
 *)

(* ------------------------------------------------------------------------- *)
(* The toplevels elements *)
(* ------------------------------------------------------------------------- *)

and toplevel =

  | TopDecl of declaration
         
  (* cppext: *)
  | CppTop of cpp_directive
  | IfdefTop of ifdef_directive (* * toplevel list *)

  (* cppext: *)
  | MacroTop of string * argument comma_list * tok list
         
  (* gccext: allow redundant ';' *)
  | EmptyDef of tok list

  | NotParsedCorrectly of tok list

  | FinalDef of info (* EOF *)

(* ------------------------------------------------------------------------- *)
and program = toplevel list

(*****************************************************************************)
(* Cpp constructs, put it comments in lexer *)
(*****************************************************************************)

(* This type is not in the Ast but is associated with the TCommentCpp token.
 * I put this enum here because parser_c.mly need it. I could have put
 * it also in lexer_parser.
 *)
type cppcommentkind = 
  | CppDirective | CppAttr | CppMacro 
  | CppPassingNormal (* ifdef 0, cplusplus, etc *) 
  | CppPassingCosWouldGetError (* expr passsing *)
  | CppOther



(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)
let nullQualif = ({const=false; volatile= false}, [])
let nQ = nullQualif 

let defaultInt = (BaseType (IntType (Si (Signed, CInt))))

(* When want add some info in ast that does not correspond to 
 * an existing C element.
 * old: or when don't want 'synchronize' on it in unparse_c.ml
 * (now have other mark for tha matter).
 *)
let no_virt_pos = ({str="";charpos=0;line=0;column=0;file=""},-1)


let fakeInfo pi  = 
  { pinfo = FakeTok ("",no_virt_pos);
  }

let noType () = ref None (* old: None, old: [] *)

let noii = []

let noQtop = None
let noQscope = []

let noTypedefDef () = None

(* for include, some meta information needed by cocci *)
let noRelPos () = 
  ()
  (* old: ref (None: include_rel_pos option) *)

let noInIfdef () = 
  ref false


(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let unwrap = fst


let rewrap_str s ii =  
  {ii with pinfo =
    (match ii.pinfo with
    | OriginTok pi -> OriginTok { pi with Common.str = s;}
    | ExpandedTok (pi,vpi) -> ExpandedTok ({ pi with Common.str = s;},vpi)
    | FakeTok (_,vpi) -> FakeTok (s,vpi)
    | Ab -> failwith "should not be rewrapped")}


let str_of_info ii =
  match ii.pinfo with
  | OriginTok pi -> pi.Common.str
  | ExpandedTok (pi,_) -> pi.Common.str
  | FakeTok (s,_) -> s
  | Ab -> failwith "Ab"


(* info about the current location *)
let get_pi = function
    OriginTok pi -> pi
  | ExpandedTok (_,(pi,_)) -> pi
  | FakeTok (_,(pi,_)) -> pi
  | Ab -> failwith "Ab"

(* original info *)
let get_opi = function
    OriginTok pi -> pi
  | ExpandedTok (pi,_) -> pi
  | FakeTok (_,_) -> failwith "no position information"
  | Ab -> failwith "Ab"

(* used by parsing hacks *)
let make_expanded ii =
  {ii with pinfo = ExpandedTok (get_opi ii.pinfo,no_virt_pos)}

(* used by token_helpers *)
let get_info f ii =
  match ii.pinfo with
  | OriginTok pi -> f pi
  | ExpandedTok (_,(pi,_)) -> f pi
  | FakeTok (_,(pi,_)) -> f pi
  | Ab -> failwith "Ab"

let get_orig_info f ii =
  match ii.pinfo with
  | OriginTok pi -> f pi
  | ExpandedTok (pi,_) -> f pi
  | FakeTok (_,(pi,_)) -> f pi
  | Ab -> failwith "Ab"

let line_of_info  ii = get_orig_info (function x -> x.Common.line)    ii
let col_of_info   ii = get_orig_info (function x -> x.Common.column)  ii
let file_of_info  ii = get_orig_info (function x -> x.Common.file)    ii

let pos_of_info   ii = get_info      (function x -> x.Common.charpos) ii
let opos_of_info  ii = get_orig_info (function x -> x.Common.charpos) ii

let pinfo_of_info ii = ii.pinfo
let parse_info_of_info ii = get_pi ii.pinfo

let is_origintok ii = 
  match ii.pinfo with
  | OriginTok pi -> true
  | _ -> false

(* used by parsing hacks *)
let rewrap_pinfo pi ii =  
  {ii with pinfo = pi}
