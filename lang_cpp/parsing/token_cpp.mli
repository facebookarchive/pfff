
type cppcommentkind = 
  | CppDirective 
  | CppAttr 
  | CppMacro 
  | CppPassingNormal (* ifdef 0, cplusplus, etc *) 
  | CppPassingCosWouldGetError (* expr passsing *)
  | CppOther

type cpluspluscommentkind =
  | CplusplusTemplate
  | CplusplusQualifier
