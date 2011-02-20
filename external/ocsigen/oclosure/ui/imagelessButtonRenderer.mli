(*
   OClosure Project - 2010
   Class goog.ui.ImagelessButtonRenderer
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open CustomButtonRenderer
open Button
open ControlContent
#endif

class type ['but] imagelessButtonRenderer = object
  inherit ['but] customButtonRenderer

(**
   Takes a text caption or existing DOM structure, and returns the content
   wrapped in a pseudo-rounded-corner box.  Creates the following DOM structure:
    <div class="goog-inline-block goog-imageless-button-outer-box">
      <div class="goog-inline-block goog-imageless-button-inner-box">
        <div class="goog-imageless-button-pos">
          <div class="goog-imageless-button-top-shadow">&nbsp;</div>
          <div class="goog-imageless-button-content">Contents...</div>
        </div>
      </div>
    </div>
   Used by both #createDom and #decorate.  To be overridden
   by subclasses.
   @param content Text caption or DOM structure to wrap
       in a box.
   @param dom DOM helper, used for document interaction.
   @return Pseudo-rounded-corner box containing the content.
   @override
 *)
  method createButton : controlContent -> Gdom.domHelper t 
    -> Dom_html.element t meth

(**
   Returns the button's contents wrapped in the following DOM structure:
      <div class="goog-inline-block goog-imageless-button">
        <div class="goog-inline-block goog-imageless-button-outer-box">
          <div class="goog-imageless-button-inner-box">
            <div class="goog-imageless-button-pos-box">
              <div class="goog-imageless-button-top-shadow">&nbsp;</div>
              <div class="goog-imageless-button-content">Contents...</div>
            </div>
          </div>
        </div>
      </div>
   Overrides goog.ui.ButtonRenderer#createDom.
   @param button Button to render.
   @return Root element for the button.
   @override
 *)
  method createDom : 'but t -> Dom_html.element t meth

(** @inheritDoc *)
  method getContentElement : #Dom_html.element t -> Dom_html.element t meth

(**
   Returns the CSS class to be applied to the root element of components
   rendered using this renderer.
   @return Renderer-specific CSS class.
   @override
 *)
  method getCssClass : js_string t meth
end

(**
   Custom renderer for goog.ui.Buttons. Imageless buttons can contain
   almost arbitrary HTML content, will flow like inline elements, but can be
   styled like block-level elements.
 *)
val imagelessButtonRenderer : (#button imagelessButtonRenderer t) constr

module ImagelessButtonRenderer : sig
  val getInstance : unit -> 'a
end
