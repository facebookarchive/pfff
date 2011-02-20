(*
   OClosure Project - 2010
   Class goog.events.EventHandler
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef EVENTS
open Js
open Event
open EventTarget
#endif

class type eventHandler = object
  inherit Disposable.disposable

(**
   Disposes of this EventHandler and remove all listeners that it registered.
 *)
  method disposeInternal : unit meth

(**
   Default event handler
   @param e Event object.
 *)
  method handleEvent : event t -> unit meth

(**
   Listen to an event on a DOM node or EventTarget.  If the function is omitted
   then the EventHandler's handleEvent method will be used.
   @param src Event source.
   @param type Event type to listen for or array of
       event types.
   @param opt_fn Optional callback function to be used as the
      listener or an object with handleEvent function.
   @param opt_capture Optional whether to use capture phase.
   @return This object, allowing for chaining of
       calls.
 *)
  method listen : 
      (eventTarget t, Dom_html.eventTarget t) Tools.Union.t 
      -> (js_string t, js_string t js_array t) Tools.Union.t
      -> (unit -> unit) opt -> bool t opt -> eventHandler t meth

(**
   Listen to an event on a DOM node or EventTarget.  If the function is omitted
   then the EventHandler's handleEvent method will be used. After the event has
   fired the event listener is removed from the target. If an array of event
   types is provided, each event type will be listened to once.
   @param src Event source.
   @param type Event type to listen for or array of
       event types.
   @param opt_fn Optional callback function to be used as the
      listener or an object with handleEvent function.
   @param opt_capture Optional whether to use capture phase.
   @return This object, allowing for chaining of
       calls.
 *)
  method listenOnce : 
      (eventTarget t, Dom_html.eventTarget t) Tools.Union.t 
      -> (js_string t, js_string t js_array t) Tools.Union.t
      -> (unit -> unit) opt -> bool t opt -> eventHandler t meth

(**
   Adds an event listener with a specific event wrapper on a DOM Node or an
   object that has implemented goog.events.EventTarget. A listener can
   only be added once to an object.

   @param src The node to listen to
       events on.
   @param wrapper Event wrapper to use.
   @param listener Callback method, or an object with a
       handleEvent function.
   @param opt_capt Whether to fire in capture phase (defaults to
       false).
   @return This object, allowing for chaining of
       calls.
 *)
  method listenWithWrapper : 
      (eventTarget t, Dom_html.eventTarget t) Tools.Union.t 
      -> eventWrapper t -> (unit -> unit) opt -> bool t opt 
      -> eventHandler t meth

(**
   Unlistens to all events.
 *)
  method removeAll : unit meth

(**
   Unlistens on an event.
   @param src Event source.
   @param type Event type to listen for.
   @param opt_fn Optional callback function to be used as the
      listener or an object with handleEvent function.
   @param opt_capture Optional whether to use capture phase.
   @return This object, allowing for chaining of
       calls.
 *)
  method unlisten : 
      (eventTarget t, Dom_html.eventTarget t) Tools.Union.t 
      -> (js_string t, js_string t js_array t) Tools.Union.t
      -> (unit -> unit) opt -> bool t opt -> eventHandler t meth

(**
   Removes an event listener which was added with listenWithWrapper().

   @param src The target to stop
       listening to events on.
   @param wrapper Event wrapper to use.
   @param listener The listener function to remove.
   @param opt_capt In DOM-compliant browsers, this determines
       whether the listener is fired during the capture or bubble phase of the
       event.
   @return This object, allowing for chaining of
       calls.
 *)
  method unlistenWithWrapper : 
      (eventTarget t, Dom_html.eventTarget t) Tools.Union.t 
      -> eventWrapper t -> (unit -> unit) opt -> bool t opt 
      -> eventHandler t meth
end

and eventWrapper = object

(**
   Adds an event listener using the wrapper on a DOM Node or an object that has
   implemented goog.events.EventTarget. A listener can only be added
   once to an object.
   @param src The node to listen to
   events on.
   @param listener Callback method, or an object with a
   handleEvent function.
   @param opt_capt Whether to fire in capture phase (defaults to
   false).
   @param opt_eventHandler Event handler to add
   listener to.
 *)
  method listen : 
      (eventTarget t, Dom_html.eventTarget t) Tools.Union.t 
      -> (unit -> unit) opt -> bool t opt -> eventHandler t opt -> unit meth

(**
   Removes an event listener added using goog.events.EventWrapper.listen.

   @param src The node to remove listener
      from.
   @param listener Callback method, or an object with a
       handleEvent function.
   @param opt_capt Whether to fire in capture phase (defaults to
       false).
   @param opt_eventHandler Event handler to remove
       listener from.
*)
  method unlisten : 
      (eventTarget t, Dom_html.eventTarget t) Tools.Union.t 
      -> (unit -> unit) opt -> bool t opt -> eventHandler t opt-> unit meth
end

(**
   Super class for objects that want to easily manage a number of event
   listeners.  It allows a short cut to listen and also provides a quick way
   to remove all events listeners belonging to this object. It is optimized to
   use less objects if only one event is being listened to, but if that's the
   case, it may not be worth using the EventHandler anyway.
 *)
val eventHandler : (eventHandler t) constr
