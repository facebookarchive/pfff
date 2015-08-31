
type category =
  | Comment 
  | Null | Boolean | Number | String | Regexp

  | Keyword 
  | KeywordConditional | KeywordLoop 
  | KeywordExn | KeywordObject | KeywordModule
  | Builtin | BuiltinCommentColor | BuiltinBoolean
  | Operator | Punctuation

  | Entity of Entity_code.entity_kind * usedef2

  | Local of usedef
  | Parameter of usedef

  | FunctionDecl of def_info
  | ConstructorMatch of use_info

  | StaticMethod of usedef2

  | StructName of usedef
  | EnumName of usedef

  | TypeVoid | TypeInt

  | FunctionEquation
  | Label of usedef

  (* semantic visual feedback! highlight more! *)
  | BadSmell
  | UseOfRef
  | PointerCall
  | CallByRef
  | ParameterRef
  | IdentUnknown

  | Ifdef | Include | IncludeFilePath | Define | CppOther

  | EmbededCode (* e.g. javascript *)
  | EmbededUrl (* e.g. xhp *)
  | EmbededHtml (* e.g. xhp *) | EmbededHtmlAttr
  | EmbededStyle (* e.g. css *)
  | Verbatim (* for latex, noweb, html pre *)

  | GrammarRule

  | CommentWordImportantNotion | CommentWordImportantModal
  | CommentSection0 | CommentSection1 | CommentSection2 
  | CommentSection3 | CommentSection4
  | CommentEstet | CommentCopyright | CommentSyncweb

  | MatchGlimpse | MatchSmPL
  | MatchParent
  | MatchSmPLPositif | MatchSmPLNegatif

  | BackGround | ForeGround

  (* tools limitations *)
  | NotParsed | Passed | Expanded | Error
  | NoType

  | Normal

and usedef = Use | Def

and usedef2 = Use2 of use_info | Def2 of def_info

 (* semantic visual feedback! *)
 and def_info = use_arity
  and use_arity = NoUse | UniqueUse | SomeUse | MultiUse | LotsOfUse | HugeUse

 (* semantic visual feedback! *)
 and use_info = place * def_arity * use_arity
  and place = PlaceLocal | PlaceSameDir | PlaceExternal | NoInfoPlace
  and def_arity = UniqueDef | DoubleDef | MultiDef | NoDef

type highlighter_preferences = {
  mutable show_type_error : bool;
  mutable show_local_global : bool;
}
val default_highlighter_preferences: highlighter_preferences
val legend_color_codes : string

(* main entry point *)
val info_of_category :
  category ->
  [> `BACKGROUND of string
   | `FOREGROUND of string
   | `SCALE of [> `LARGE | `MEDIUM | `XX_LARGE | `X_LARGE ]
   | `STRIKETHROUGH of bool
   | `STYLE of [> `ITALIC ]
   | `UNDERLINE of [> `DOUBLE | `SINGLE ]
   | `WEIGHT of [> `BOLD ] ]
  list
(* use the same polymorphic variants than in ocamlgtk *)
val info_of_usedef : 
  usedef -> 
  [> `STYLE of [> `ITALIC ] ] list
val info_of_def_arity :
  def_arity ->
  [> `STRIKETHROUGH of bool | `UNDERLINE of [> `DOUBLE | `SINGLE ] ] list
val info_of_place : 'a -> 'b

val arity_ids : 'a list -> def_arity
val rewrap_arity_def2_category: def_info -> category -> category

