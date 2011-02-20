(*
   OClosure Project - 2010
   Class goog.ui.DrilldownRow
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Component
open Js
#endif

class type drilldownRow = object
  inherit component
  
(**
   Child drilldowns are rendered when needed.
 *
   @param child New child to be added.
   @param index position to be occupied by the child.
   @param opt_render true to force immediate rendering.
 *)
  method addChildAt : component t -> int -> bool t opt -> unit meth

(**
   A top-level DrilldownRow decorates a TR element.

   @param node The element to test for decorability.
   @return true iff the node is a TR.
 *)
  method canDecorate : #Dom_html.element t -> bool t meth

(** @inheritDoc *)
  method createDom : unit meth

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
   The base class method calls its superclass method and this
   drilldown's 'decorator' method as defined in the constructor.
 *)
  method enterDocument : unit meth

(**
   Finds the numeric index of this child within its parent Component.
   Throws an exception if it has no parent.

   @return index of this within the children of the parent Component.
 *)
  method findIndex : int meth

(**
   Returns this DrilldownRow's level in the tree.  Top level is 1.

   @return depth of this DrilldownRow in its tree of drilldowns.
 *)
  method getDepth : int meth

(**
   Returns the expanded state of the DrilldownRow.

   @return true iff this is expanded.
 *)
  method isExpanded : bool t meth

(** @inheritDoc *)
  method removeChild : (js_string t, component t) Tools.Union.t opt ->
    bool t opt -> component t meth

(**
   Rendering of DrilldownRow's is on need, do not call this directly
   from application code.

   Rendering a DrilldownRow places it according to its position in its
   tree of DrilldownRows.  DrilldownRows cannot be placed any other
   way so this method does not use any arguments.  This does not call
   the base class method and does not modify any of this
   DrilldownRow's children.
 *)
  method render_ : unit meth

(**
   Sets the expanded state of this DrilldownRow: makes all children
   displayable or not displayable corresponding to the expanded state.

   @param expanded whether this should be expanded or not.
 *)
  method setExpanded : bool t -> unit meth
end

(**
   Builds a DrilldownRow component, which can overlay a tree
   structure onto sections of an HTML table.

*)
val drilldownRow : (drilldownRow t) constr 
