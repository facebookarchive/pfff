(*
   OClosure Project - 2010

   Class goog.Timer
   
   @author : Oran Charles
   @version 0.2
 *)

open Js
class type timer = object
  inherit Events.eventTarget

  (** Gets the interval of the timer. 
      @return interval Number of ms between ticks. *)
  method getInterval : int meth

  (** Sets the interval of the timer.
      @param interval Number of ms between ticks.*)
  method setInterval : int -> unit meth

  (** Dispatches the TICK event. This is its own method so subclasses can override. *)
  method dispatchTick : unit meth
    
  (** Starts the timer. *)
  method start : unit meth
    
  (** Stops the timer. *)
  method stop : unit meth

  (** Disposes of the timer. *)
  method disposeInternal : unit meth
    
(*  (** Calls the given function once, after the optional pause
    @param listener Function or object that has a handleEvent method.
    @param opt_interval Number of ms between ticks (Default: 1ms).
    @param opt_handler Object in whose scope to call the listener.
    @return A handle to the timer ID. *)
    method callOnce : (unit -> unit) -> int opt -> object? -> unit meth *)

  (** Clears a timeout initiated by callOnce
      @param timerId a timer ID. *)
  method clear : int -> unit meth

end

(** Class for handling timing events.
   @param opt_interval Number of ms between ticks (Default: 1ms).
   @param opt_timerObject  An object that has setTimeout, setInterval, 
   clearTimeout and clearInterval (eg Window).
*)
val timer : (int opt -> timer t) constr
