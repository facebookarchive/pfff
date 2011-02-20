(*
   OClosure Project - 2010
   Class goog.ui.ToolbarMenuButton
   
   @author : Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Js
open ControlContent
open Button
open MenuButton
open Menu
#endif

class type toolbarMenuButton = object
  inherit menuButton
end

val toolbarMenuButton : (controlContent -> menu t opt -> 
  toolbarMenuButton #buttonRenderer t opt -> Gdom.domHelper t opt -> 
    toolbarMenuButton t) constr
