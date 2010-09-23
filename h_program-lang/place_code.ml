
(*****************************************************************************)
(* The place, for comment it's the C/C++/Java entity next to it *)
(*****************************************************************************)


(* We also associate to the place the entity it refers too. It can be useful
 * for instance to see if a comment is referencing the same entity multiple
 * times.
 *
 * todo? make difference between macroVar that expand to constant and
 * the one that expands to complex code ? 
 * 
 * history: was in of ccomment/comments/comments_extraction.ml
 *
 * From ocamldoc manual, the ocaml entities:
 *  "In this chapter, we use the word element to refer to any of the
 *  following parts of an OCaml source file": 
 *  - SEMI a module
 *  - DONE a class
 *  - DONE a type
 *  - DONE a value (function or variable)
 *  - an exception
 *  - SEMI a type constructor
 *  - DONE a record field
 *  - a module type
 *  - a class type
 *  - DONE a class method
 *  - DONE a class value or 
 *  - a class inheritance clause
 *)

type place = 

  | Header (* file/module/package comment *)

  (* ------------------- *)
  (* big entities *)

  | Function of string
  | Variable of string

  | Prototype of string

  (* ------------------- *)
  (* c++ext: javaext: *)
  | Class of string (* javeext: or interface *)

  | Method of string
  | MethodDecl of string

  | Constructor of string
  | Destructor of string

  | ThrowErrorSpec 
  | InheritanceSpec 

  (* ------------------- *)
  (* cppext: *)

  (* #define *)
  | MacroFunction of string
  | MacroVariable of string

  (* #ifdef, make difference between the different kind ? cst, version, etc*)
  | IfDef 

  (* #include, import *)
  | Include of string

  (* cppext: also a end of xxx, cf below *)
  | IfdefElse (* or elsif *)
  | IfdefEndif


  (* ------------------- *)
  (* type related *)

  | Field of string

  (* add type ? maybe can detect that put lots of comments on int *)
  | Struct of string 
  | StructIdent (* as in struct /* xxx */ { }, often seen in sparse code *)

  | Enum of string
  | EnumValue of string


  (* ------------------- *)
  | Parameter (* type ? *)

  | Type

  | Initializer

  (* ------------------- *)
  (* less? associate the statement ? *)
  | Statement of statement_place
  | Expression of expression_place


  (* ------------------- *)
  (* end of stuff *)

  | EndCompound
  | Else 


  | EndOfFile

  (* ------------------- *)
  (* fallthrough *)
  | InInitializerUnknown 
  | InStructUnknown
  | InEnumUnknown
  | InFuncUnknown

  (* c++ext: *)
  | InClassUnknown
  | InInterfaceUnknown
  | InCtorDtorUnknown


  (* ------------------- *)
  | Unknown
  | UnknownParsePB

  | Fake (* fake comments but to have opportunity to have objects other
          * than comments in the LFS database
          *)

 and statement_place = 
   | If
   | Loop (* for, while, iterator *)

   | SimpleFunCall of string
   | SimpleAssign of string
   | SimpleFieldAssign of string

   | MethodCall of string (* xxx.foo(); pointer function call =~ method call *)

   | SimpleAssignOp of Ast_generic.arithOp
   | SimpleIncDec of Ast_generic.fixOp

   | Label of string (* often used as exception handler in Linux *)
   | Case of string option

   | Goto of string
   | Break
   | Continue
   | Return

   | Try
   | Throw

   | Sync

   | Compound

   | StatementUnknown

 and expression_place = 
   | Binary of Ast_generic.binaryOp

   | ExpressionUnknown


(*****************************************************************************)
let s_of_arithop op = 
  match op with
  | Ast_generic.Plus | Ast_generic.Minus -> "plusminus"
  | Ast_generic.Mul | Ast_generic.Div | Ast_generic.Mod -> "muldiv"
  | Ast_generic.DecLeft | Ast_generic.DecRight  -> "shift"
  | Ast_generic.And | Ast_generic.Or | Ast_generic.Xor -> "logic"


(* the string option is the optional C entity the comments is next to *)
let (s_of_place : place -> string * string option) = function
  | Header -> "header", None

  | Function s  -> "function", Some s
  | Variable s -> "variable", Some s
  | Prototype s  -> "prototype", Some s

  | Class s  -> "class", Some s
  | Method s  -> "method", Some s
  | MethodDecl s  -> "method_decl", Some s

  | Constructor s -> "constructor", Some s
  | Destructor s -> "destructor", Some s

  | ThrowErrorSpec -> "throwspec", None
  | InheritanceSpec -> "inheritancespec", None

  | MacroFunction s -> "macro_function", Some s
  | MacroVariable s -> "macro_variable", Some s

  | IfDef -> "ifdef", None
  | Include s -> "include", Some s

  | Parameter -> "parameter", None

  | Type -> "type", None

  | Initializer -> "initializer", None

  | Struct s -> "struct", Some s
  | Field s -> "field", Some s
  | StructIdent -> "structident", None

  | Enum s -> "enum", Some s
  | EnumValue s -> "enum_value", Some s

  | EndCompound -> "end_compound", None
  | IfdefElse -> "ifdef_else", None
  | IfdefEndif -> "ifdef_endif", None
  | EndOfFile -> "end_eof", None
  | Else -> "end_else", None

  | Statement st -> 
      (match st with
      | If -> "if", None
      | Loop -> "loop", None

      | SimpleFunCall s -> "funcall", Some s
      | MethodCall s -> "methodcall", Some s

      | SimpleAssign s -> "assign", Some s
      | SimpleFieldAssign s -> "assignfield", Some s
      | SimpleAssignOp op -> ("assignop_" ^ s_of_arithop op), None
      | SimpleIncDec op -> "incdec", None

      | Label s -> "label", Some s
      | Case sopt -> "case", sopt

      | Goto s -> "goto", Some s
      | Continue -> "continue", None  | Break -> "break", None
      | Return -> "return", None

      | Try -> "try", None
      | Throw -> "throw", None

      | Sync -> "throw", None


      | Compound -> "compound", None

      | StatementUnknown -> "statement_misc", None
      )

  | Expression e -> 
      (match e with
      | Binary op -> "op", None
      | ExpressionUnknown -> "exp_misc", None
      )



  | InInitializerUnknown -> "initalizer_misc", None
  | InStructUnknown -> "struct_misc", None
  | InEnumUnknown -> "enum_misc", None
  | InFuncUnknown -> "function_misc", None
  | InClassUnknown -> "class_misc", None
  | InCtorDtorUnknown -> "ctordtor_misc", None
  | InInterfaceUnknown -> "interface_misc", None

  | Unknown -> "unknown", None
  | UnknownParsePB -> "unknown_parsepb", None
  | Fake -> "fake", None


(* unify back some difference place like struct/class, function/method *)
let simplify_and_group_place place = 
  raise Common.Todo
