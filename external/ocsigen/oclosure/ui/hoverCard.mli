(*
   OClosure Project - 2010
   Class goog.ui.HoverCard
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef UI
open Js
open AdvancedTooltip
#endif

class type hoverCard = object
  inherit advancedTooltip

(**
   Abort pending hovercard showing, if any.
*)
  method cancelTrigger : unit meth

(**
   Destroys widget and remove listeners.
 *)
  method disposeInternal : unit meth

(**
   Gets the DOM element that triggered the current hovercard.  Note that in
   the TRIGGER or CANCEL_TRIGGER events, the current hovercard's anchor may not
   be the one that caused the event, so use the event's anchor property instead.
   @return Object that caused the currently displayed hovercard (or
       pending hovercard if none is displayed) to be triggered.
 *)
  method getAnchorElement : Dom_html.element t meth

(**
   Called by timer from mouse over handler. If this is called and the hovercard
   is not shown for whatever reason, then send a cancel trigger event.
   @param el Element to show tooltip for.
   @param opt_pos Position to display popup
       at.
 *)
  method maybeShow : #Dom_html.element t -> 
    Positioning.abstractPosition t opt -> unit meth

(**
   Sets the max number of levels to search up the dom if checking descendants.
   @param maxSearchSteps Maximum number of levels to search up the
       dom if checking descendants.
 *)
  method setMaxSearchSteps : int -> unit meth

(**
   Triggers the hovercard to show after a delay.
   @param anchorElement Element that is triggering the hovercard.
   @param opt_pos Position to display
       hovercard.
 *)
  method triggerForElement : #Dom_html.element t -> 
    Positioning.abstractPosition t opt -> unit meth
end

(**
   Create a hover card object.  Hover cards extend tooltips in that they don't
   have to be manually attached to each element that can cause them to display.
   Instead, you can create a function that gets called when the mouse goes over
   any element on your page, and returns whether or not the hovercard should be
   shown for that element.
   Alternatively, you can define a map of tag names to the attribute name each
   tag should have for that tag to trigger the hover card.  See example below.
   Hovercards can also be triggered manually by calling
  , shown without a delay by calling
  , or triggered over other elements by calling
  .  For the latter two cases, the application is responsible
   for calling when finished.
   HoverCard objects fire a TRIGGER event when the mouse moves over an element
   that can trigger a hovercard, and BEFORE_SHOW when the hovercard is
   about to be shown.  Clients can respond to these events and can prevent the
   hovercard from being triggered or shown.
   @param isAnchor Function that returns true if a given
       element should trigger the hovercard.  Alternatively, it can be a map of
       tag names to the attribute that the tag should have in order to trigger
       the hovercard, e.g., {A: 'href'} for all links.  Tag names must be all
       upper case; attribute names are case insensitive.
   @param opt_checkDescendants Use false for a performance gain if
       you are sure that none of your triggering elements have child elements.
       Default is true.
   @param opt_domHelper Optional DOM helper.
 *)
val hoverCard : ((#Dom_html.element t -> bool t) -> bool t opt -> 
  Gdom.domHelper t opt -> hoverCard t) constr 
