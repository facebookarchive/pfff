(* OClosure Project - 2010
   Class goog.math.Coordinate
     
   @author : Emmanuel CRESPIN
   @version 0.1
 *)
#ifndef MATH
open Js
#endif

class type coordinate = object
  method clone : coordinate t meth
  
  (** Returns a nice js_string t representing the coordinate.*)
  method toString : js_string t meth
  
  (** Compares coordinates for equality.*)
  method equals : coordinate t -> coordinate t -> bool meth
  
  (** Returns the distance between two coordinates.*)
  method distance : coordinate t -> coordinate t -> float meth
  
  (** Returns the squared distance between two coordinates.*)
  method squaredDistance : coordinate t -> coordinate t -> float t meth
  
  (** Returns the difference between two coordinates as a new goog.math.Coordinate.*)
  method difference : coordinate t -> coordinate t -> coordinate t meth
  
  (** Returns the sum of two coordinates as a new goog.math.Coordinate.*)
  method sum : coordinate t -> coordinate t -> coordinate t meth
end

(**
   Class for representing coordinates and positions.
   @param opt_x Left, defaults to 0.
   @param opt_y Top, defaults to 0.
 *)
val coordinate : (float opt -> float opt -> coordinate t) constr 
