(*
   OClosure Project - 2010
   Class goog.ui.FormPost
   
   @author Cardoso Gabriel 
   @version 0.2
*)

#ifndef UI
open Component
open Js
#endif

class type formPost = object
  inherit component 

  method createDom : unit meth

(*  method post : param -> js_string t opt -> js_string t opt -> unit meth*)
end

val formPost : (formPost t) constr
