(*
   OClosure Project - 2010
   Class goog.math.Rect
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef MATH
open Js
open Coordinate
open Size
open Box
#endif

class type rect = object
(**
   Expand this rectangle to also include the area of the given rectangle.
   @param rect The other rectangle.
 *)
  method boundingRect : rect t -> unit meth

(**
   Returns a new copy of the rectangle.
   @return A clone of this Rectangle.
 *)
  method clone : rect t meth

(**
   Tests whether this rectangle entirely contains another rectangle or
   coordinate.

   @param another The rectangle or
       coordinate to test for containment.
   @return Whether this rectangle contains given rectangle or
       coordinate.
 *)
  method contains : (rect t, coordinate t) Tools.Union.t 
    -> bool t meth

(**
   Computes the difference regions between this rectangle and [rect]. The
   return value is an array of 0 to 4 rectangles defining the remaining regions
   of this rectangle after the other has been subtracted.
   @param rect A Rectangle.
   @return An array with 0 to 4 rectangles which
       together define the difference area of rectangle a minus rectangle b.
 *)
  method difference : rect t -> rect t js_array t meth

(**
   Returns the size of this rectangle.
   @return The size of this rectangle.
 *)
  method getSize : size t meth

(**
   Computes the intersection of this rectangle and the rectangle parameter.  If
   there is no intersection, returns false and leaves this rectangle as is.
   @param rect A Rectangle.
   @return True iff this rectangle intersects with the parameter.
 *)
  method intersection : rect t -> bool t meth

(**
   Returns whether a rectangle intersects this rectangle.
   @param rect A rectangle.
   @return Whether rect intersects this rectangle.
 *)
  method intersects : rect t -> bool t meth

(**
   Returns a new Box object with the same position and dimensions as this
   rectangle.
   @return A new Box representation of this Rectangle.
*)
  method toBox : box t meth
      
 (**
    Returns a nice string representing size and dimensions of rectangle.
    @return In the form (50, 73 - 75w x 25h).
 *)
  method toString : js_string t meth
end

(**
   Class for representing rectangular regions.
   @param x Left.
   @param y Top.
   @param w Width.
   @param h Height.
 *)
val rect : (int -> int -> int -> int -> rect t) constr
