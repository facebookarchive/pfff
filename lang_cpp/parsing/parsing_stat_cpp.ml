open Common 

(*****************************************************************************)
(* Stat *)
(*****************************************************************************)

(* coupling: if you add a new var, modify also assoc_stat_number below *)

let nTypedefInfer = ref 0

let nIncludeGrammar = ref 0
let nIncludeHack = ref 0

let nIteratorGrammar = ref 0 
let nIteratorHeuristic = ref 0 

let nMacroTopDecl = ref 0
let nMacroStructDecl = ref 0
let nMacroDecl = ref 0
let nMacroStmt = ref 0
let nMacroString = ref 0
let nMacroHigherOrder = ref 0 (* actions *)
let nMacrohigherTypeGrammar = ref 0

let nIfdefTop = ref 0
let nIfdefStmt = ref 0
let nIfdefStruct = ref 0
let nIfdefInitializer = ref 0
(* nIfdefExpr, nIfdefType *)

let nIfdefFunheader = ref 0

let nIfdefExprPassing = ref 0

let nIfdefZero = ref 0
let nIfdefVersion = ref 0



let nGccTypeof = ref 0
let nGccLongLong = ref 0
let nGccAsm = ref 0
let nGccInline = ref 0
let nGccAttribute = ref 0
let nGccCaseRange = ref 0
let nGccMixDecl = ref 0
let nGccDesignator = ref 0
let nGccStmtExpr = ref 0
let nGccConstructor = ref 0
let nGccEmptyStruct = ref 0
let nGccNestedFunc = ref 0

let nGccMisc = ref 0



let nDefineHack = ref 0

let nDefineConstant = ref 0
let nDefineStmt = ref 0
let nDefineExpr = ref 0
(* both below require some heuristic support *)
let nDefineWhile0 = ref 0
let nDefineInit = ref 0

let nDefineOther = ref 0

let nUndef = ref 0
let nPragmaAndCo = ref 0

(* let nDirectiveTop = ref 0 *)
let nDirectiveStmt = ref 0
let nDirectiveStruct = ref 0
let nDirectiveInitializer = ref 0


(* from standard.h *)
let nMacroHint = ref 0
let nMacroExpand = ref 0

let nNotParsedCorrectly = ref 0

let assoc_stat_number = 
  [
    "nTypedefInfer", nTypedefInfer;

    "nIteratorHeuristic", nIteratorHeuristic;

    "nMacroTopDecl", nMacroTopDecl;
    "nMacroStructDecl", nMacroStructDecl;
    "nMacroDecl", nMacroDecl;
    "nMacroStmt", nMacroStmt;
    "nMacroString", nMacroString;
    "nMacroHigherOrder", nMacroHigherOrder;

    "nMacrohigherTypeGrammar", nMacrohigherTypeGrammar;

    "nIfdefTop", nIfdefTop;
    "nIfdefStmt", nIfdefStmt;
    "nIfdefStruct", nIfdefStruct;
    "nIfdefInitializer", nIfdefInitializer;

    "nIfdefFunheader", nIfdefFunheader;
    "nIfdefZero", nIfdefZero;
    "nIfdefVersion", nIfdefVersion;
    "nIfdefExprPassing", nIfdefExprPassing;

    "nMacroExpand", nMacroExpand;
    "nMacroHint", nMacroHint;


    "nGccTypeof", nGccTypeof;
    "nGccLongLong", nGccLongLong;
    "nGccAsm", nGccAsm;
    "nGccInline", nGccInline;
    "nGccAttribute", nGccAttribute;
    "nGccCaseRange", nGccCaseRange;
    "nGccMixDecl", nGccMixDecl;
    "nGccDesignator", nGccDesignator;
    "nGccStmtExpr", nGccStmtExpr;
    "nGccConstructor", nGccConstructor;
    "nGccEmptyStruct", nGccEmptyStruct;
    "nGccNestedFunc", nGccNestedFunc;

    "nGccMisc", nGccMisc;


    "nDefineHack", nDefineHack;

    "nDefineConstant", nDefineConstant;
    "nDefineStmt", nDefineStmt;
    "nDefineExpr", nDefineExpr;
    "nDefineInit", nDefineInit;
    "nDefineOther", nDefineOther;

    "nUndef", nUndef;
    "nPragmaAndCo", nPragmaAndCo;

    "nDirectiveStmt", nDirectiveStmt;
    "nDirectiveStruct", nDirectiveStruct;
    "nDirectiveInitializer", nDirectiveInitializer;

    "nNotParsedCorrectly", nNotParsedCorrectly;


    (* less *)
    "nIncludeGrammar", nIncludeGrammar;
    "nIncludeHack", nIncludeHack;

    "nIteratorGrammar", nIteratorGrammar;
  ]

let print_stat_numbers () = 
  assoc_stat_number +> List.iter (fun (k, vref) -> 
    pr2 (spf "%-30s -> %d" k !vref);
  )
