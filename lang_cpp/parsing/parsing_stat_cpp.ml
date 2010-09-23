open Common 

(*****************************************************************************)
(* Stat *)
(*****************************************************************************)
type parsing_stat = {
    filename: filename;
    mutable have_timeout: bool;

    mutable correct: int;  
    mutable bad: int;

    mutable commentized: int; (* by our cpp commentizer *)

    (* if want to know exactly what was passed through, uncomment:
     *  
     * mutable passing_through_lines: int;
     * 
     * it differs from bad by starting from the error to
     * the synchro point instead of starting from start of
     * function to end of function.
     *)

  } 

let default_stat file =  { 
    filename = file;
    have_timeout = false;
    correct = 0; bad = 0;
    commentized = 0;
  }

(* todo: stat per dir ?  give in terms of func_or_decl numbers:   
 * nbfunc_or_decl pbs / nbfunc_or_decl total ?/ 
 *
 * note: cela dit si y'a des fichiers avec des #ifdef dont on connait pas les 
 * valeurs alors on parsera correctement tout le fichier et pourtant y'aura 
 * aucune def  et donc aucune couverture en fait.   
 * ==> TODO evaluer les parties non parsé ? 
 *)

let print_parsing_stat_list ?(verbose=false) = fun statxs -> 
  let total = List.length statxs in
  let perfect = 
    statxs 
      +> List.filter (function 
          {have_timeout = false; bad = 0} -> true | _ -> false)
      +> List.length 
  in

  if verbose then begin
  pr "\n\n\n---------------------------------------------------------------";
  pr "pbs with files:";
  statxs 
    +> List.filter (function 
      | {have_timeout = true} -> true 
      | {bad = n} when n > 0 -> true 
      | _ -> false)
    +> List.iter (function 
        {filename = file; have_timeout = timeout; bad = n} -> 
          pr (file ^ "  " ^ (if timeout then "TIMEOUT" else i_to_s n));
        );

  pr "\n\n\n";
  pr "files with lots of tokens passed/commentized:";
  let threshold_passed = 100 in
  statxs 
    +> List.filter (function 
      | {commentized = n} when n > threshold_passed -> true
      | _ -> false)
    +> List.iter (function 
        {filename = file; commentized = n} -> 
          pr (file ^ "  " ^ (i_to_s n));
        );

  pr "\n\n\n---------------------------------------------------------------";
  end;

  pr (
  (sprintf "NB total files = %d; " total) ^
  (sprintf "perfect = %d; " perfect) ^
  (sprintf "pbs = %d; "     (statxs +> List.filter (function 
      {have_timeout = b; bad = n} when n > 0 -> true | _ -> false) 
                               +> List.length)) ^
  (sprintf "timeout = %d; " (statxs +> List.filter (function 
      {have_timeout = true; bad = n} -> true | _ -> false) 
                               +> List.length)) ^
  (sprintf "=========> %d" ((100 * perfect) / total)) ^ "%"
                                                          
  );
  let good = statxs +> List.fold_left (fun acc {correct = x} -> acc+x) 0 in
  let bad  = statxs +> List.fold_left (fun acc {bad = x} -> acc+x) 0  in
  let passed = statxs +> List.fold_left (fun acc {commentized = x} -> acc+x) 0
  in
  let gf, badf = float_of_int good, float_of_int bad in
  let passedf = float_of_int passed in
  pr (
  (sprintf "nb good = %d,  nb passed = %d " good passed) ^
  (sprintf "=========> %f"  (100.0 *. (passedf /. gf)) ^ "%")
   );
  pr (
  (sprintf "nb good = %d,  nb bad = %d " good bad) ^
  (sprintf "=========> %f"  (100.0 *. (gf /. (gf +. badf))) ^ "%"
   )
  )

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
