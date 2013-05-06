
open Ast_clang
open Parser_clang

let vof_token =
  function
  | TUpperIdent v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("TUpperIdent", [ v1 ]))
  | TLowerIdent v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("TLowerIdent", [ v1 ]))
  | TInt v1 -> let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("TInt", [ v1 ]))
  | THexInt v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("THexInt", [ v1 ]))
  | TFloat v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("TFloat", [ v1 ]))
  | TString v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("TString", [ v1 ]))
  | TPath v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("TPath", [ v1 ]))
  | TMisc v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("TMisc", [ v1 ]))
  | TOPar v1 -> let v1 = Ocaml.vof_int v1 in Ocaml.VSum (("TOPar", [ v1 ]))
  | TOBrace v1 -> let v1 = Ocaml.vof_int v1 in Ocaml.VSum (("TOBrace", [ v1 ]))
  | TInf v1 -> let v1 = Ocaml.vof_int v1 in Ocaml.VSum (("TInf", [ v1 ]))
  | TOBracket v1 ->
      let v1 = Ocaml.vof_int v1 in Ocaml.VSum (("TOBracket", [ v1 ]))
  | TCPar -> Ocaml.VSum (("TCPar", []))
  | TSup -> Ocaml.VSum (("TSup", []))
  | TCBrace -> Ocaml.VSum (("TCBrace", []))
  | TCBracket -> Ocaml.VSum (("TCBracket", []))
  | TColon -> Ocaml.VSum (("TColon", []))
  | TComma -> Ocaml.VSum (("TComma", []))
  | TArrow -> Ocaml.VSum (("TArrow", []))
  | TDot -> Ocaml.VSum (("TDot", []))
  | TDots -> Ocaml.VSum (("TDots", []))
  | TEq -> Ocaml.VSum (("TEq", []))
  | TPlus -> Ocaml.VSum (("TPlus", []))
  | TMinus -> Ocaml.VSum (("TMinus", []))
  | TTilde -> Ocaml.VSum (("TTilde", []))
  | TStar -> Ocaml.VSum (("TStar", []))
  | TAnd -> Ocaml.VSum (("TAnd", []))
  | Error -> Ocaml.VSum (("Error", []))
  | TUnknown v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("TUnknown", [ v1 ]))
  | EOF -> Ocaml.VSum (("EOF", []))

let vof_enum =
  function
  | TodoAst v1 -> let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("TodoAst", [ v1 ]))
  | LinkageSpecDecl -> Ocaml.VSum (("LinkageSpecDecl", []))
  | FullComment -> Ocaml.VSum (("FullComment", []))
  | TextComment -> Ocaml.VSum (("TextComment", []))
  | ParagraphComment -> Ocaml.VSum (("ParagraphComment", []))
  | InlineCommandComment -> Ocaml.VSum (("InlineCommandComment", []))
  | VerbatimLineComment -> Ocaml.VSum (("VerbatimLineComment", []))
  | BlockCommandComment -> Ocaml.VSum (("BlockCommandComment", []))
  | VisibilityAttr -> Ocaml.VSum (("VisibilityAttr", []))
  | DeprecatedAttr -> Ocaml.VSum (("DeprecatedAttr", []))
  | MaxFieldAlignmentAttr -> Ocaml.VSum (("MaxFieldAlignmentAttr", []))
  | AlwaysInlineAttr -> Ocaml.VSum (("AlwaysInlineAttr", []))
  | NoDebugAttr -> Ocaml.VSum (("NoDebugAttr", []))
  | ConstAttr -> Ocaml.VSum (("ConstAttr", []))
  | NoThrowAttr -> Ocaml.VSum (("NoThrowAttr", []))
  | NonNullAttr -> Ocaml.VSum (("NonNullAttr", []))
  | AsmLabelAttr -> Ocaml.VSum (("AsmLabelAttr", []))
  | PackedAttr -> Ocaml.VSum (("PackedAttr", []))
  | FormatAttr -> Ocaml.VSum (("FormatAttr", []))
  | AlignedAttr -> Ocaml.VSum (("AlignedAttr", []))
  | WarnUnusedResultAttr -> Ocaml.VSum (("WarnUnusedResultAttr", []))
  | MayAliasAttr -> Ocaml.VSum (("MayAliasAttr", []))
  | PureAttr -> Ocaml.VSum (("PureAttr", []))
  | MallocAttr -> Ocaml.VSum (("MallocAttr", []))
  | ReturnsTwiceAttr -> Ocaml.VSum (("ReturnsTwiceAttr", []))
  | UnusedAttr -> Ocaml.VSum (("UnusedAttr", []))
  | FormatArgAttr -> Ocaml.VSum (("FormatArgAttr", []))
  | UnavailableAttr -> Ocaml.VSum (("UnavailableAttr", []))
  | TransparentUnionAttr -> Ocaml.VSum (("TransparentUnionAttr", []))
  | BlocksAttr -> Ocaml.VSum (("BlocksAttr", []))
  | Misc__Null__ -> Ocaml.VSum (("Misc__Null__", []))
  | Misc__Capture__ -> Ocaml.VSum (("Misc__Capture__", []))
  | Misc__Cleanup__Block -> Ocaml.VSum (("Misc__Cleanup__Block", []))
  | IntegerLiteral -> Ocaml.VSum (("IntegerLiteral", []))
  | StringLiteral -> Ocaml.VSum (("StringLiteral", []))
  | FloatingLiteral -> Ocaml.VSum (("FloatingLiteral", []))
  | CharacterLiteral -> Ocaml.VSum (("CharacterLiteral", []))
  | UnaryOperator -> Ocaml.VSum (("UnaryOperator", []))
  | BinaryOperator -> Ocaml.VSum (("BinaryOperator", []))
  | ConditionalOperator -> Ocaml.VSum (("ConditionalOperator", []))
  | CompoundAssignOperator -> Ocaml.VSum (("CompoundAssignOperator", []))
  | DeclRefExpr -> Ocaml.VSum (("DeclRefExpr", []))
  | ImplicitCastExpr -> Ocaml.VSum (("ImplicitCastExpr", []))
  | CStyleCastExpr -> Ocaml.VSum (("CStyleCastExpr", []))
  | CallExpr -> Ocaml.VSum (("CallExpr", []))
  | MemberExpr -> Ocaml.VSum (("MemberExpr", []))
  | ArraySubscriptExpr -> Ocaml.VSum (("ArraySubscriptExpr", []))
  | InitListExpr -> Ocaml.VSum (("InitListExpr", []))
  | CompoundLiteralExpr -> Ocaml.VSum (("CompoundLiteralExpr", []))
  | ShuffleVectorExpr -> Ocaml.VSum (("ShuffleVectorExpr", []))
  | UnaryExprOrTypeTraitExpr -> Ocaml.VSum (("UnaryExprOrTypeTraitExpr", []))
  | BlockExpr -> Ocaml.VSum (("BlockExpr", []))
  | ParenExpr -> Ocaml.VSum (("ParenExpr", []))
  | ExprWithCleanups -> Ocaml.VSum (("ExprWithCleanups", []))
  | VAArgExpr -> Ocaml.VSum (("VAArgExpr", []))
  | PredefinedExpr -> Ocaml.VSum (("PredefinedExpr", []))
  | CompoundStmt -> Ocaml.VSum (("CompoundStmt", []))
  | NullStmt -> Ocaml.VSum (("NullStmt", []))
  | DeclStmt -> Ocaml.VSum (("DeclStmt", []))
  | IfStmt -> Ocaml.VSum (("IfStmt", []))
  | ForStmt -> Ocaml.VSum (("ForStmt", []))
  | WhileStmt -> Ocaml.VSum (("WhileStmt", []))
  | DoStmt -> Ocaml.VSum (("DoStmt", []))
  | BreakStmt -> Ocaml.VSum (("BreakStmt", []))
  | ContinueStmt -> Ocaml.VSum (("ContinueStmt", []))
  | SwitchStmt -> Ocaml.VSum (("SwitchStmt", []))
  | CaseStmt -> Ocaml.VSum (("CaseStmt", []))
  | DefaultStmt -> Ocaml.VSum (("DefaultStmt", []))
  | ReturnStmt -> Ocaml.VSum (("ReturnStmt", []))
  | GotoStmt -> Ocaml.VSum (("GotoStmt", []))
  | LabelStmt -> Ocaml.VSum (("LabelStmt", []))
  | GCCAsmStmt -> Ocaml.VSum (("GCCAsmStmt", []))
  | FunctionDecl -> Ocaml.VSum (("FunctionDecl", []))
  | EnumDecl -> Ocaml.VSum (("EnumDecl", []))
  | EnumConstantDecl -> Ocaml.VSum (("EnumConstantDecl", []))
  | RecordDecl -> Ocaml.VSum (("RecordDecl", []))
  | FieldDecl -> Ocaml.VSum (("FieldDecl", []))
  | IndirectFieldDecl -> Ocaml.VSum (("IndirectFieldDecl", []))
  | Field -> Ocaml.VSum (("Field", []))
  | TypedefDecl -> Ocaml.VSum (("TypedefDecl", []))
  | VarDecl -> Ocaml.VSum (("VarDecl", []))
  | BlockDecl -> Ocaml.VSum (("BlockDecl", []))
  | ParmVarDecl -> Ocaml.VSum (("ParmVarDecl", []))
  | TranslationUnitDecl -> Ocaml.VSum (("TranslationUnitDecl", []))
  
let rec vof_sexp =
  function
  | Paren ((v1, v2, v3)) ->
      let v1 = vof_enum v1
      and v2 = Ocaml.vof_int v2
      and v3 = Ocaml.vof_list vof_sexp v3
      in Ocaml.VSum (("Paren", [ v1; v2; v3 ]))
  | Angle v1 ->
      let v1 = Ocaml.vof_list vof_sexp v1 in Ocaml.VSum (("Angle", [ v1 ]))
  | Anchor v1 ->
      let v1 = Ocaml.vof_list vof_sexp v1 in Ocaml.VSum (("Anchor", [ v1 ]))
  | Bracket v1 ->
      let v1 = Ocaml.vof_list vof_sexp v1 in Ocaml.VSum (("Bracket", [ v1 ]))
  | Brace ((v1, v2)) ->
      let v1 = Ocaml.vof_list vof_token v1
      and v2 = Ocaml.vof_option (Ocaml.vof_list vof_token) v2
      in Ocaml.VSum (("Brace", [ v1; v2 ]))
  | T v1 -> let v1 = vof_token v1 in Ocaml.VSum (("T", [ v1 ]))
  
let vof_program v = vof_sexp v
  

