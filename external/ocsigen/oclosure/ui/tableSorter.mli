(* 
    OClosure Project - 2010
    Class goog.ui.TableSorter
    
    @author Gabriel Cardoso 
    @version 0.2
*)

#ifndef UI
open Js
open Component
#endif

class type tableSorter = object
  inherit component

(** @inheritDoc *)
  method canDecorate : #Dom_html.element t -> bool t meth

(** @inheritDoc *)
  method enterDocument : unit meth

(**
   @return The default sort function to be used by
       all columns.
 *)
  method getDefaultSortFunction : 
    (js_string t -> js_string t -> int) callback meth

(**
   @return The current sort column of the table, or -1 if none.
 *)
  method getSortColumn : int meth

(**
   Gets the sort function to be used by the given column.  Returns the default
   sort function if no sort function is explicitly set for this column.
   @param column The column index.
   @return The sort function used by the column.
 *)
  method getSortFunction : 
    (js_string t -> js_string t -> int) callback meth

(**
   @return Whether the last sort was in reverse.
 *)
  method isSortReversed : bool t meth

(**
   Sets the default sort function to be used by all columns.  If not set
   explicitly, this defaults to numeric sorting.
   @param sortFunction The new default sort function.
 *)
  method setDefaultSortFunction : (js_string t -> js_string t -> int) callback 
    -> unit meth

(**
   Set the sort function for the given column, overriding the default sort
   function.
   @param column The column index.
   @param sortFunction The new sort function.
 *)
  method setSortFunction : int -> 
    (js_string t -> js_string t -> int) callback -> unit meth

(**
   Sort the table contents by the values in the given column.
   @param column The column to sort by.
   @param opt_reverse Whether to sort in reverse.
 *)
  method sort : int -> bool t opt -> unit meth
end

(**
   A table sorter allows for sorting of a table by column.  This component can
   be used to decorate an already existing TABLE element with sorting
   features.
   The TABLE should use a THEAD containing TH elements for the table column
   headers.
   @param opt_domHelper Optional DOM helper, used for
       document interaction.
 *)
val tableSorter : (Gdom.domHelper t opt -> tableSorter t) constr

