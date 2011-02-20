(*
   OClosure Project - 2010
   Class goog.ui.Zippy

   @author : Oran Charles, Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open EventTarget
open Js
#endif

class type zippy = object
  inherit Events.eventTarget

  (** Collapses content pane. *)
  method collapse : unit meth

  (** Destroys widget and removes all event listeners. *)
  method disposeInternal : unit  meth

  (** Expands content pane. *)
  method expand : unit  meth

  (** Whether the zippy is in the process of being expanded or collapsed. *)
  method isBusy : bool t meth

  (** Whether the zippy is expanded. *)
  method isExpanded : bool t  meth

  (** Sets expanded state. *)
  method setExpanded : bool t -> unit meth

  (** Toggles expanded state. *)
  method toggle : unit meth
end

val zippy : ((#Dom_html.element t, js_string t) Tools.Union.t opt 
	    -> (#Dom_html.element t, js_string t) Tools.Union.t opt -> bool t opt 
	    -> (#Dom_html.element t, js_string t) Tools.Union.t opt -> zippy t) constr

val zippy_lazy : ((#Dom_html.element t, js_string t) Tools.Union.t opt 
	    -> (unit -> Dom_html.element t) callback -> bool t opt 
	    -> (#Dom_html.element t, js_string t) Tools.Union.t opt -> zippy t) constr
