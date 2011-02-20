(*
   OClosure Project - 2010
   Class goog.ui.tree.treeControl
   
   A single node in the tree
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef TREE
open Js
open BaseNode
#endif
open Gdom

class type ['a] treeNode = object
  inherit ['a] baseNode
(**
   Returns the source for the icon.
   @return Src for the icon.
*)
  method getCalculatedIconClass : js_string t meth
  
(**
   Returns the tree
   @return The tree.
*)
  method getTree : 'a baseNode t opt meth
end

val treeNode : (js_string t -> config opt -> domHelper t opt -> 'a treeNode t) constr
