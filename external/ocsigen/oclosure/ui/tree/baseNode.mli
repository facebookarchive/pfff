(*
   OClosure Project - 2010
   Class goog.ui.tree.BaseNode
   
   @author : Cardoso Gabriel
   @version 0.2
*)

#ifndef TREE
open Js
#endif
#ifndef UI
open Component
#endif
open Gdom

type config 

class type ['a] baseNode = object
  inherit component
  
(**
   Adds a node as a child to the current node
   @param child The child to add.
   @param opt_before If specified, the new child is
   added as a child before this one. If not specified, it's appended to the
   end.
   @return The added child.
*)
  method add : 'a baseNode t -> 'a baseNode t opt -> 'a baseNode t meth
   
  method collapse : unit meth

  method collapseAll : unit meth

(** 
   Returns true if the node is a descendant of this node
   @param node The node to check.
   @return True if the node is a descendant of this node, false
   otherwise.
*)
  method contains : 'a baseNode t -> bool t meth

(**
   Creates the element.
   @return The element.
*)
  method createDom : unit meth

  method deselect : unit meth

  method disposeInternal : unit meth

  method enterDocument : unit meth

  method expand : unit meth

  method expandAll : unit meth

  method expandChildren : unit meth

(**
   @return The element after the label.
*)
  method getAfterLabelElement : Dom_html.element t meth

(**
   Returns the html that appears after the label. This is useful if you want to
   put extra UI on the row of the label but not inside the anchor tag.
   @return The html.
*)
  method getAfterLabelHtml : js_string t meth

(* Abstract method... *)
(*  method getCalculatedIconClass : unit -> unit*)

(**
   Returns the children of this node. The caller must not modify the returned
   collection.
   @return The children.
*)
  method getChildren : 'a baseNode t js_array t meth

(**
   @return The div containing the children.
*)
  method getChildrenElement : Dom_html.element t meth

(**
   @return Data set by the client.
*)
  method getClientData : 'a opt meth

(**
   @return The configuration for the tree.
*)
  method getConfig : config meth

(**
   Returns the depth of the node in the tree.
   Should no longer be overridden; override computeDepth instead.
   @return The non-negative depth of this node (the root is zero).
*)
  method getDepth : number t meth

(**
   @return The element for the tree node.
*)
  method getElement : Dom_html.element t meth

(**
   @return The src for the icon used for expanding the node.
*)
  method getExpandIconClass : js_string t meth

(**
   @return The expanded icon element.
 *)
  method getExpandIconElement : Dom_html.element t meth

(**
   @return The source for the icon.
 *)
  method getExpandIconHtml : js_string t meth

(**
   @return Whether the node is expanded.
 *)
  method getExpanded : bool t meth

(**
   Gets the icon class for when the node is expanded.
   @return The class.
 *)
  method getExpandedIconClass : js_string t meth

(**
   @return The first child of this node.
 *)
  method getFirstChild : 'a baseNode t opt meth

(**
   Returns the html of the label.
   @return The html string of the label.
 *)
  method getHtml : js_string t meth

(**
   Gets the icon class for the node.
   @return s The icon source.
 *)
  method getIconClass : js_string t meth

  method setClientData : 'a opt -> unit meth      
end

val baseNode : (js_string t -> config opt -> domHelper t opt -> 'a baseNode t) constr

module BaseNode : sig
val add : 'a #baseNode t -> 'a #baseNode t -> 'a #baseNode t opt -> unit
end
