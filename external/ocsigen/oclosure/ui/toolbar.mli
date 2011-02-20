(*
   OClosure Project - 2010
   Class goog.ui.Toolbar
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Js
open Container
open Control
#endif

class type toolbar = object
  inherit container
end

class type ['cont] toolbarRenderer = object
  inherit ['cont] containerRenderer

  (**
     Returns the ARIA role to be applied to toolbar/menubar.
     @return  ARIA role.
  *)
  method getAriaRole : Gdom.A11y.role_pre optdef meth

  (**
     Inspects the element, and creates an instance of or
     an appropriate subclass best suited to decorate it.  Overrides the superclass implementation by recognizing HR elements as separators.
     @param  element Element to decorate.
     @return  A new control suitable to decorate the element (null if none).
  *)
  method getDecoratorForChild : #Dom_html.element t -> control t opt meth

  (**
     Returns the CSS class to be applied to the root element of containers
     rendered using this renderer.
     @return  Renderer-specific CSS class.
  *)
  method getCssClass : js_string t meth

  (**
     Returns the default orientation of containers rendered or decorated by this renderer.  This implementation returns .
     @return  Default orientation for containers created or decorated by this renderer.
  *)
  method getDefaultOrientation : Container.orientation_pre meth

end

(**
   Default renderer for goog.ui.Toolbar, based on goog.ui.ContainerRenderer.
*)
val toolbarRenderer : (toolbar #toolbarRenderer t) constr

val toolbar : (toolbar #toolbarRenderer t opt -> Container.orientation_pre opt 
  -> Gdom.domHelper t opt -> toolbar t) constr
