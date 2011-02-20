(* 
   OClosure Project - 2010
   Class goog.ui.DatePicker
   
   @author Bozman Cagdas
   @version 0.2
*)
#ifndef UI
open Component
open Js
#endif

class type datePicker = object
  inherit component
 
  method create : unit meth

  method createDom : unit meth

  method decorateInternal : #Dom_html.element t -> unit meth

  method disposeInternal : unit meth

  method enterDocument : unit meth

  method exitDocument : unit meth

  method getAllowNone : bool t meth

  method getDate : Date.date t opt meth

  method getExtraWeekAtEnd : bool t meth

  method getFirstWeekday : int meth

  method getShowFixedNumWeeks : bool t meth

  method getShowOtherMonths : bool t meth

  method getShowToday : bool t meth

  method getShowWeekNum : bool t meth

  method getShowWeekdayNames : bool t meth

  method getWeekdayClass : int -> js_string t meth

  method isCreated : unit meth

  method nextMonth : unit meth

  method nextYear : unit meth

  method previousMonth : unit meth

  method selectNone : unit meth

  method selectToday : unit meth

  method setAllowNone : bool t -> unit meth

  method setDate : (Date.date t, Js.date t) Tools.Union.t -> unit meth

  method setDecorator : (Date.date t -> js_string t) -> unit meth

  method setExtraWeekAtEnd : bool t -> unit meth

  method setFirstWeekday : int -> unit meth

  method setShowFixedNumWeeks : bool t -> unit meth

  method setShowOtherMonths : bool t -> unit meth

  method setShowToday : bool t -> unit meth

  method setShowWeekNum : bool t -> unit meth

  method setShowWeekdayNames : bool t -> unit meth

  method setUseNarrowWeekdayNames : bool t -> unit meth

  method setUseSimpleNavigationMenu : bool t -> unit meth

  method setWeekdayClass : int -> js_string t -> unit meth
end

val datePicker : ((Date.date t, Js.date t) Tools.Union.t opt -> 
  I18n.dateTimeSymbols opt -> datePicker t) constr
