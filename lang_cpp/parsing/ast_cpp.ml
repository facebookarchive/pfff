(* Yoann Padioleau
 * 
 * Copyright (C) 2002 Yoann Padioleau
 * Copyright (C) 2006-2007 Ecole des Mines de Nantes
 * Copyright (C) 2008-2009 University of Urbana Champaign
 * Copyright (C) 2010-2011 Facebook
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

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This is a big file ... C++ is a quite complicated language ... 
 *
 * Like most other ASTs in pfff, it's actually more a Concrete Syntax Tree.
 * Some stuff are tagged 'semantic:' which means that they are computed
 * after parsing. 
 *)
(*****************************************************************************)
(* The AST C++ related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type info = Parse_info.info
and tok = info

(* a shortcut to annotate some information with token/position information *)
and 'a wrap  = 'a * info list (* TODO CHANGE to 'a * info *)
and 'a wrap2  = 'a * info

and 'a paren   = tok * 'a * tok
and 'a brace   = tok * 'a * tok
and 'a bracket = tok * 'a * tok 
and 'a angle = tok * 'a * tok 

and 'a comma_list = 'a wrap list

(* ------------------------------------------------------------------------- *)
(* Ident, name, scope qualifier *)
(* ------------------------------------------------------------------------- *)

(* c++ext: in C 'name' and 'ident' are equivalent and equal to just 'string'.
 * In C++ 'ident' can have a complex form like 'A::B::list<int>::size'.
 * I use Q for qualified. I also have a special type to make the difference
 * between intermediate idents (the classname or template_id) and final idents.
 * Note that sometimes final idents are also classnames and can have final
 * template_id.
 * 
 * Sometimes some elements are not allowed at certain places, for instance
 * converters can not have an associated Qtop. But I prefered to simplify
 * and have a unique type for all those different kinds of ident.
 *)
type name = qtop option * qualifier list * ident  

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

 and qtop = qtobis wrap2 (* :: *)
   and qtobis = QTop

 and template_argument = (fullType, expression) Common.either
 and template_arguments = template_argument comma_list

 (* special cases *)
 and class_name     = name (* only IdIdent or IdTemplateId *)
 and namespace_name = name (* only IdIdent *)
 and typedef_name   = name (* only IdIdent *)
 and enum_name      = name (* only IdIdent *)

 and ident_name = name (* only IdIdent *)

(* TODO: do like in parsing_c/
 * and ??? ident_string ??? = 
 *  | RegularName of string wrap
 *
 *  (* cppext: *)
 *  | CppConcatenatedName of (string wrap) wrap2 (* the ## separators *) list
 *  (* normally only used inside list of things, as in parameters or arguments
 *   * in which case, cf cpp-manual, it has a special meaning *)
 *  | CppVariadicName of string wrap (* ## s *)
 *  | CppIdentBuilder of string wrap (* s ( ) *) * 
 *                      ((string wrap) wrap2 list) (* arguments *)
 *)

(* ------------------------------------------------------------------------- *)
(* Types *)
(* ------------------------------------------------------------------------- *)
(* We could have a more precise type in fullType, in expression, etc, but
 * it would require too much things at parsing time such as checking there
 * is no conflicting structname, computing value, etc. It's better to separate
 * concerns, so I put '=>' to mean what we would really like. In fact
 * what we really like is defining another fullType, expression, etc
 * from scratch, because many stuff are just sugar.
 * 
 * invariant: Array and FunctionType have also typeQualifier but they
 * dont have sense. I put this to factorise some code. If you look in
 * grammar, you see that we can never specify const for the array
 * himself (but we can do it for pointer).
 *)

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

  (* c++note: TypeName can now correspond also to a classname or enumname *)
  | TypeName   of string(*typedef_name*)  * fullType option (* semantic: *)
  (* c++ext: *)
  | TypeTemplate of string(*ident_name*) * template_arguments

  | TypeOfExpr of expression  
  (* gccext: TypeOfType may seems useless, why declare a __typeof__(int)
   * x; ? But when used with macro, it allows to fix a problem of C which
   * is that type declaration can be spread around the ident. Indeed it
   * may be difficult to have a macro such as '#define macro(type,
   * ident) type ident;' because when you want to do a macro(char[256],
   * x), then it will generate invalid code, but with a '#define
   * macro(type, ident) __typeof(type) ident;' it will work. *)
  | TypeOfType of fullType    

  (* c++ext: only to disambiguate I think *)
  | TypenameKwd of fullType (* in practice either type_name of template_name *)

  (* forunparser: *)
  | ParenType of fullType 

  and  baseType = 
    | Void 
    | IntType   of intType 
    | FloatType of floatType

     (* stdC: type section. todo? add  a SizeT ?
      * note: 'char' and 'signed char' are semantically different!! 
      *)
      and intType   = 
        | CChar (* obsolete? | CWchar  *)
        | Si of signed
         (* c++ext: maybe could be put in baseType instead ? *)
        | CBool | WChar_t 

        and signed = sign * base
         and base = 
           | CChar2 | CShort | CInt | CLong 
           (* gccext: *)
           | CLongLong 
         and sign = Signed | UnSigned

      and floatType = CFloat | CDouble | CLongDouble

   and enumType = enum_elem comma_list (* => string * int list *)
    and enum_elem = {
      e_name: string wrap2;
      e_val: (tok (*=*) * constExpression) option;
    }

   (* c++ext: TODO const, throw spec, etc *) 
   and functionType = { 
     ft_ret: fullType;
     ft_params: parameter comma_list; (* TODO: paren *)
     ft_has_dots: bool wrap; (* (info * info) option? *)
   }
     and parameter = {
        p_name: string wrap2 option;
        p_type: fullType;
        p_register: bool wrap;
      }

  (* c++ext: for class_definition (was structType) see below *)

and typeQualifier = typeQualifierbis wrap 
and typeQualifierbis = { const: bool; volatile: bool; }

(* TODO: like in parsing_c/
 * (* gccext: cppext: *)
 * and attribute = attributebis wrap
 *  and attributebis =
 *   | Attribute of string 
 *)

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)

(* Because of StatementExpr, we can have more 'new scope', but it's
 * rare I think. For instance with 'array of constExpression' we could
 * have an StatementExpr and a new (local) struct defined. Same for
 * Constructor.
 *)
and expression = expressionbis wrap
 and expressionbis = 

  (* Ident can be a enumeration constant, a simple variable, a name of a func.
   * cppext: Ident can also be the name of a macro. Sparse says
   *  "an identifier with a meaning is a symbol". 
   * c++ext: Ident is now a 'name' instead of a 'string' and can be
   *  also an operator name for example.
   *)
  | Ident of name * (* semantic: *) ident_info
  | C of constant                                  
  (* c++ext: *)
  | This of tok

  (* specialized version of FunCallExpr that makes it easier to write
   * certain analysis. Note that because 'name' can be qualified,
   * FunCallSimple is also a StaticMethodCallSimple
   *)
  | FunCallSimple  of name * argument comma_list
  (* todo? MethodCallSimple, MethodCallExpr *)
  | FunCallExpr    of expression * argument comma_list

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

   (* The Pt is redundant normally, could be replace by DeRef RecordAccess *)
  | RecordAccess   of expression * name
  | RecordPtAccess of expression * name

  (* c++ext: note that second paramater is an expression, not a name *)
  | RecordStarAccess   of expression * expression
  | RecordPtStarAccess of expression * expression

  | SizeOfExpr     of expression
  | SizeOfType     of fullType

  | Cast          of fullType * expression

  (* gccext: *)        
  | StatementExpr of compound wrap (* ( )     new scope *) 
  | GccConstructor  of fullType * initialiser comma_list

  (* c++ext: *)
  | ConstructedObject of fullType * argument comma_list
  | TypeIdOfExpr     of expression
  | TypeIdOfType     of fullType
  | CplusplusCast of cast_operator * fullType * expression

  | New (* todo: of placement, init, etc *)
  | Delete      of qtop option * expression
  | DeleteArray of qtop option * expression

  | Throw of expression option 

  (* forunparser: *)
  | ParenExpr of expression 

  and ident_info = {
    mutable i_scope: Scope_code.scope;
  }

  (* cppext: normmally just expression *)
  and argument = (expression, weird_argument) Common.either
   and weird_argument = 
       | ArgType of parameter
       | ArgAction of action_macro
      and action_macro = 
         | ActMisc of tok list

  (* I put 'string' for Int and Float because 'int' would not be enough.
   * Indeed OCaml int are 31 bits. So it's simpler to use 'string'. 
   * Same reason to have 'string' instead of 'int list' for the String case.
   * 
   * note: '-2' is not a constant; it is the unary operator '-'
   * applied to the constant '2'. So the string must represent a positive
   * integer only. 
   *)
  and constant = 
    | String of (string * isWchar) 
    | MultiString  (* can contain MacroString *)
    | Char   of (string * isWchar) (* normally it is equivalent to Int *)
    | Int    of (string  (* * intType*)) 
    | Float  of (string * floatType)
    (* c++ext: *)
    | Bool of bool
    and isWchar = IsWchar | IsChar

  and unaryOp  = 
    | GetRef | DeRef | UnPlus |  UnMinus | Tilde | Not 
    (* gccext: via &&label notation *)
    | GetRefLabel
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
    | UnaryTildeOp | UnaryNotOp | CommaOp

 (* c++ext: *)
  and cast_operator = 
    | Static_cast | Dynamic_cast | Const_cast | Reinterpret_cast

 and constExpression = expression (* => int *)

(* ------------------------------------------------------------------------- *)
(* Statements *)
(* ------------------------------------------------------------------------- *)
(* note: assignement is not a statement, it's an expression
 * (wonderful C language).
 * note: I use 'and' for type definition because gccext allows statements as
 * expressions, so we need mutual recursive type definition now.
 *)
and statement = statementbis wrap 
 and statementbis = 
  | Compound      of compound   (* new scope *)
  | ExprStatement of exprStatement
  | Labeled       of labeled
  | Selection     of selection
  | Iteration     of iteration
  | Jump          of jump

  (* c++ext: in C this constructor could be outside the statement type, in a
   * decl type, because declarations are only at the beginning of a compound
   * normally. But in C++ we can freely mix declarations and statements.
   *)
  | Try of compound wrap * handler list
  | DeclStmt  of block_declaration 
  (* gccext: *)
  | NestedFunc of definition
  (* cppext: *)
  | MacroStmt

  (* cppext: c++ext:
   * old: compound = (declaration list * statement list)
   * old: (declaration, statement) either list 
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

  and selection     = 
   | If of expression * statement * statement
   (* need to check that all elements in the compound start
    * with a case:, otherwise it's unreachable code.
    *)
   | Switch of expression * statement 

  and iteration     = 
    | While   of expression * statement
    | DoWhile of statement * expression
    | For     of exprStatement wrap * exprStatement wrap * exprStatement wrap *
                 statement
    (* cppext: *)
    | MacroIteration of string * argument comma_list * statement

  and jump  = 
    | Goto of string
    | Continue | Break 
    | Return   | ReturnExpr of expression
    (* gccext: goto *exp ';' *)
    | GotoComputed of expression

  (* c++ext: *)
  and handler = exception_declaration wrap (* catch () *) * compound wrap
   and exception_declaration = 
     | ExnDeclEllipsis of tok
     | ExnDecl of parameter

(* ------------------------------------------------------------------------- *)
(* Block Declaration *)
(* ------------------------------------------------------------------------- *)
(* a.k.a declaration_statement *)
and block_declaration = block_declarationbis wrap
 and block_declarationbis =
 (* (name * ...) option cos can have empty declaration or struct tag 
  * declaration.
  *   
  * Before I had a Typedef constructor, but why make this special case and not
  * have StructDef, EnumDef, so that 'struct t {...} v' will generate 2 
  * declarations? So I try to generalise and not have not Typedef. This
  * requires more work in parsing. Better to separate concern.
  * Before the need for unparser, I didn't have a DeclList but just a Decl.
  *
  * I am not sure what it means to declare a prototype inline, but gcc
  * accepts it. 
  * 
  * note: var_declaration include prototype declaration,
  * and class_declaration.
  *)
  | DeclList of onedecl comma_list wrap (* ; *)
  (* cppext: todo? now factorize with MacroTop ?  *)
  | MacroDecl of (string * argument comma_list) wrap

  (* c++ext: using namespace *)
  | UsingDecl of name
  | UsingDirective of namespace_name
  | NameSpaceAlias of string * namespace_name

  (* gccext: *)
  | Asm of asmbody

  and onedecl = {
    v_namei: (name *  (info (*=*) * initialiser) option)  option;
    v_type: fullType;
    v_storage: storage wrap;
    (* v_attr: attribute list; *) (* gccext: *)
  }
   and storage       = storagebis * bool (* gccext: inline or not: *)
   and storagebis    = NoSto | StoTypedef | Sto of storageClass
   and storageClass  = Auto  | Static | Register | Extern (* Mutable? *)
   (* Friend ???? *)

   (*c++ext: TODO *)
   and func_specifier = Inline | Virtual

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
              
  (* gccext: *)
  and asmbody = tok list (* string list *) * colon wrap (* : *) list
      and colon = Colon of colon_option comma_list
      and colon_option = colon_optionbis wrap
          and colon_optionbis = ColonMisc | ColonExpr of expression
        
(* ------------------------------------------------------------------------- *)
(* Function definition *)
(* ------------------------------------------------------------------------- *)
(* Normally we should define another type functionType2 because there 
 * are more restrictions on what can define a function than a pointer 
 * function. For instance a function declaration can omit the name of the
 * parameter wheras a function definition can not. But, in some cases such
 * as 'f(void) {', there is no name too, so I simplified and reused the 
 * same functionType type for both declarations and function definitions.
 *)
and definition = definitionbis wrap (* ( ) { } *)
 and definitionbis = {
   f_name: name;
   f_type: functionType;
   f_storage: storage wrap;
   f_body: compound;
   (*f_attr: attribute list;*) (* gccext: *)
 }

(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
and class_definition = {
  c_kind: structUnion wrap2; 
  (* the ident can be a template_id when do template specialization. *)
  c_name: ident_name(*class_name??*) option;
  c_inherit: (tok (*:*) * base_clause comma_list) option;
  c_members: class_member_sequencable list (* new scope *); (* braces? *)
  }
  and structUnion =
    | Struct | Union
    (* c++ext: *)
    | Class

  (* used in inheritance spec (base_clause) and class_member *)
  and access_spec = Public | Private | Protected

  (* was called field wrap before *)
  and class_member = class_memberbis wrap
   and class_memberbis =
     | Access of access_spec (* could put outside and take class_member list *)

     (* before unparser, I didn't have a FieldDeclList but just a Field. *)
     | DeclarationField of fieldkind comma_list wrap (* ';' *)
    
     | Method of definition
     (* MethodDecl is inside field_declaration *)
     | Constructor of definition * bool (* explicit *) (* * TODO chain_call*)
     | Destructor of definition

     | ConstructorDecl of parameter comma_list * bool (* explicit *)
     | DestructorDecl of name(*IdDestructor*) * bool (* virtual*) 
         (* ( ) void_opt *)
         
     | QualifiedIdInClass of name (* ?? *)
         
     | TemplateDeclInClass of (template_parameters * declaration)
     | UsingDeclInClass of name

     | EmptyField  (* gccext: and maybe c++ext: ';' *)

     (* At first I thought that a bitfield could be only Signed/Unsigned.
      * But it seems that gcc allow char i:4. C rule must say that you
      * can cast into int so enum too, ... 
      * c++ext: FieldDecl was before Simple of string option * fullType
      * but in c++ field can also have storage so now reuse ondecl.
      *)
      and fieldkind = fieldkindbis wrap (* :  or pure spec *)
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
    | IfdefStruct of ifdef_directive (*  * field list *)

  and base_clause = base_clausebis wrap (* virtual and access spec *)
    and base_clausebis = 
         class_name * bool (* virtual inheritance *) * access_spec option

(* ------------------------------------------------------------------------- *)
(* Declaration, in c++ sense *)
(* ------------------------------------------------------------------------- *)

and declaration = declarationbis wrap
 and declarationbis = 
  | BlockDecl of block_declaration (* include class definition *)

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
  (* after have some semantic info *)
  | NameSpaceExtend of string * declaration_sequencable list 
  | NameSpaceAnon   of          declaration_sequencable list

  (* gccext: allow redundant ';' *)
  | EmptyDef of tok

 and template_parameter = parameter (* todo more *)
  and template_parameters = template_parameter comma_list

  (* TODO *)
  (* cppext: easier to put at statement_list level than statement level *)
  and declaration_sequencable = 
    | DeclElem of declaration
    (* cppext: *) 
    | CppDirectiveDecl of cpp_directive
    | IfdefDecl of ifdef_directive (* * statement list *)

 and method_def = definition (* TODO: agglomerate method and ctor_dtor *)

(* ------------------------------------------------------------------------- *)
(* cppext: cpp directives, #ifdef, #define and #include body *)
(* ------------------------------------------------------------------------- *)
and cpp_directive =
  | Define of tok (* #define*) * string wrap2 * (define_kind * define_val)
  | Include of tok (* #include s *) * inc_file
  | Undef of string wrap2 (* #undef xxx *)
  | PragmaAndCo of tok

  and define_kind =
   | DefineVar
   | DefineFunc   of (string wrap comma_list) wrap
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

  and inc_file = 
    | Local    of inc_elem list
    | Standard of inc_elem list
    | Wierd of string (* ex: #include SYSTEM_H *)
   and inc_elem = string

  (* to specialize if someone need more info *)
  and ifdef_directive = 
    | IfdefDirective of tok list
  (* or and 'a ifdefed = 'a list wrap (* ifdef elsif else endif *) *)

(* TODO: like in parsing_c/
 * (* todo? to specialize if someone need more info *)
 * and ifdef_directive = (* or and 'a ifdefed = 'a list wrap *)
 *   | IfdefDirective of (ifdefkind * matching_tag) wrap
 *   and ifdefkind = 
 *     | Ifdef (* todo? of string ? of formula_cpp ? *)
 *     | IfdefElseif (* same *)
 *     | IfdefElse (* same *)
 *     | IfdefEndif 
 *   (* set in Parsing_hacks.set_ifdef_parenthize_info. It internally use 
 *    * a global so it means if you parse the same file twice you may get
 *    * different id. I try now to avoid this pb by resetting it each 
 *    * time I parse a file.
 *    *)
 *   and matching_tag = 
 *     IfdefTag of (int (* tag *) * int (* total with this tag *))
 *)

(* ------------------------------------------------------------------------- *)
(* The toplevel elements *)
(* ------------------------------------------------------------------------- *)
and toplevel =
  | TopDecl of declaration
         
  (* cppext: *)
  | CppTop of cpp_directive
  | IfdefTop of ifdef_directive (* * toplevel list *)
  | MacroTop of string * argument comma_list * tok list
         
  | NotParsedCorrectly of tok list

  | FinalDef of info (* EOF *)

and program = toplevel list

(* ------------------------------------------------------------------------- *)
(* Any *)
(* ------------------------------------------------------------------------- *)
and any = 
  | Program of program
  | Toplevel of toplevel
  | Decl of declaration
  | BlockDecl2 of block_declaration
  | Stmt of statement
  | Expr of expression
  | Type of fullType
  | Name of name

  | Cpp of cpp_directive
  | ClassDef of class_definition
  | FuncDef of definition
  | MethodDef of method_def

  | Constant of constant
  | Argument of argument
  | Parameter of parameter

  | Body of compound (* brace *)

  | Info of info
  | InfoList of info list

 (* with tarzan *)

(*****************************************************************************)
(* Some constructors *)
(*****************************************************************************)
let nQ = ({const=false; volatile= false}, [])
let defaultInt = (BaseType (IntType (Si (Signed, CInt))))

let fakeInfo pi  = 
  { PI.token = PI.FakeTokStr ("",None); comments = ();transfo = PI.NoTransfo;}

let noIdInfo () = { i_scope = Scope_code.NoScope; }
let noii = []
let noQscope = []
let noTypedefDef () = None

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let unwrap = fst
let untype = fst
let uncomma xs = List.map fst xs

let unwrap_typeC (qu, (typeC, ii)) = typeC

let rewrap_str = PI.rewrap_str
let str_of_info = PI.str_of_info

(* When want add some info in ast that does not correspond to 
 * an existing C element.
 * old: or when don't want 'synchronize' on it in unparse_c.ml
 * (now have other mark for tha matter).
 * used by parsing hacks
 *)
let make_expanded ii =
  let noVirtPos = ({PI.str="";charpos=0;line=0;column=0;file=""},-1) in
  let (a, b) = noVirtPos in
  { ii with PI.token = PI.ExpandedTok (PI.get_opi ii.PI.token, a, b) }

(* used by token_helpers *)
let get_info = PI.get_info

let line_of_info       = PI.line_of_info
let col_of_info        = PI.col_of_info
let file_of_info       = PI.file_of_info
let pos_of_info        = PI.pos_of_info
let pinfo_of_info      = PI.pinfo_of_info
let parse_info_of_info = PI.parse_info_of_info
let is_origintok       = PI.is_origintok

let opos_of_info ii = 
  PI.get_orig_info (function x -> x.PI.charpos) ii

(* used by parsing hacks *)
let rewrap_pinfo pi ii =  
  {ii with PI.token = pi}

(* for error reporting *) 
let string_of_info ii = 
  Parse_info.string_of_parse_info (parse_info_of_info ii)

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

(* used while migrating the use of 'string' to 'name' *)
let (string_of_name_tmp: name -> string) = fun name ->
  let (_opt, _qu, id) = name in
  match id with
  | IdIdent s, _ -> s
  | _ ->
      "TODO_string_of_name_tmp"
      (* raise Todo *)

let (info_of_name_tmp: name -> info) = fun name ->
  let (_opt, _qu, id) = name in
  match id with
  | IdIdent s, ii -> List.hd ii
  | _ -> raise Todo

let (semi_fake_name: (string * info) -> name) = fun (s, iis) ->
  None, [], (IdIdent s, [iis])
