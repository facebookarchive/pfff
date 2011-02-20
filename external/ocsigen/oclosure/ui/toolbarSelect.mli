(*
   OClosure Project - 2010

   Class goog.ui.ToolBarSelect
   
   @author : Oran Charles
   @version 0.2
*)
#ifndef UI
open Js
open Select
open Menu
open MenuButton
open MenuButtonRenderer
open ControlContent
#endif
open Gdom

class type toolbarSelect = object
  inherit select
end

(**
   A select control for a toolbar.
   @param caption Default caption or existing DOM structure to display as the button's caption when nothing is selected.
   @param opt_menu Menu containing selection options.
   @param opt_renderer Renderer used to render or decorate the control; defaults to goog.ui.ToolbarMenuButtonRenderer.
   @param opt_domHelper Optional DOM hepler, used for document interaction.
*)
val toolbarSelect : (controlContent opt -> menu t -> toolbarSelect #menuButtonRenderer t opt -> domHelper t opt -> toolbarSelect t) constr
