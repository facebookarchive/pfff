open Ast_java

type 'a inout = 'a -> 'a

type visitor_s = {

  ktype_s      : typ  inout * visitor_s -> typ  inout;
  kexpr_s      : expr inout * visitor_s -> expr inout;
  kstatement_s : stmt inout * visitor_s -> stmt inout;

  kini_s       : init inout * visitor_s -> init inout;
  kcase_s      : case inout * visitor_s -> case inout;

  kname_s      : name inout * visitor_s -> name inout;

  kdecl_s: (decl inout * visitor_s) -> decl inout;
  kprogram_s: (compilation_unit inout * visitor_s) -> compilation_unit inout;

  kinfo_s      : info inout * visitor_s -> info inout;
}

val default_visitor_s : visitor_s

val compilation_unit : visitor_s -> compilation_unit -> compilation_unit
val decl             : visitor_s -> decl             -> decl            
val class_decl       : visitor_s -> class_decl       -> class_decl      
val interface        : visitor_s -> interface        -> interface  
val method_decl      : visitor_s -> method_decl      -> method_decl     
val init             : visitor_s -> init             -> init            
val stmt             : visitor_s -> stmt             -> stmt            
val expr             : visitor_s -> expr             -> expr            
val typ              : visitor_s -> typ              -> typ             
val name             : visitor_s -> name            -> name
val info             : visitor_s -> info            -> info
val infoii           : visitor_s -> info list -> info list

val decls             : visitor_s -> decls             -> decls            
val modifiers         : visitor_s -> modifiers -> modifiers 


type 'a effect = 'a -> unit

type visitor = { 
   ktype      : (typ   effect  * visitor_s) -> typ   effect;
   kexpr      : (expr  effect  * visitor_s) -> expr  effect;
   kstatement : (stmt  effect  * visitor_s) -> stmt  effect;

   kini       : (init effect   * visitor_s) -> init  effect; 

   kname      : (name effect   * visitor_s) -> name  effect;
   kinfo      : (info effect   * visitor_s) -> info  effect;
 } 
