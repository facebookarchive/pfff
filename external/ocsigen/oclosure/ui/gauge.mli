(* 
    OClosure Project - 2010
    
    Class goog.ui.Gauge

    @author : Oran Charles
    @version 0.2
*)
#ifndef UI
open Component
open GaugeTheme
open Js
#endif
open Gdom
open Ggraphics

class type gaugeColoredRange = object
end

(**
   Information on how to decorate a range in the gauge. This is an internal-only class.
   @param fromValue The range start (minimal) value.
   @param toValue The range end (maximal) value.
   @param backgroundColor Color to fill the range background with.
*)
val gaugeColoredRange : (int -> int -> js_string t -> gaugeColoredRange t) constr

class type gauge = object
  inherit component

  (** @return The minimum value of the range. *)
  method getMinimum : int meth

  (** Sets the minimum value of the range
      @param min The minimum value of the range. *)
  method setMinimum : int -> unit meth

  (** @return The maximum value of the range. *)
  method getMaximum : int meth

  (** Sets the maximum number of the range
      @param max The maximum value of the range. *)
  method setMaximum : int -> unit meth

  (** Sets the current value range displayed by the gauge.
      @param value The current value for the gauge. This value determines the position of the needle of the gauge.
      @param opt_formattedValue The string value to show in the gauge. If not specified, no string value will be displayed. *)
  method setValue : int -> js_string t opt -> unit meth

  (** Sets the number of major tick sections and minor tick sections.
      @param majorUnits The number of major tick sections.
      @param minorUnits The number of minor tick sections for each major tick section. *)
  method setTicks : int -> int -> unit meth

  (** Sets the labels of the major ticks.
      @param tickLabels A text label for each major tick value. *)
  method setMajorTickLabels : string js_array t -> unit meth

  (** Sets the top title of the gauge. The top title is displayed above the center.
      @param text The top title text. *)
  method setTitleTop : js_string t -> unit meth

  (** Sets the bottom title of the gauge. The top title is displayed below the center.
      @param text The bottom title text. *)
  method setTitleBottom : js_string t -> unit meth

  (** Sets the font for displaying top and bottom titles.
      @param font The font for titles. *)
  method setTitleFont : font t -> unit meth

  (** Sets the font for displaying the formatted value.
      @param font The font for displaying the value. *) 
  method setValueFont : font t -> unit meth

  (** Sets the color theme for drawing the gauge.
      @param theme The color theme to use. *)
  method setTheme : gaugeTheme t -> unit meth

  (** Set the background color for a range of values on the gauge.
      @param fromValue The lower (start) value of the colored range.
      @param toValue The higher (end) value of the colored range.
      @param color The color name to paint the range with. For example 'red', '#ffcc00' or constants like goog.ui.Gauge.RED. *)
  method addBackgroundColor : int -> int -> js_string t -> unit meth

  (** Creates the DOM representation of the graphics area. *)
  method createDom : unit meth

  (** Redraws the entire gauge. Should be called after theme colors have been changed. *)
  method redraw : unit meth

  (** Called when the component is added to the DOM. *)
  method enterDocument : unit meth
    
  (** Called when the component is removed from the DOM. *)
  method exitDocument : unit meth 

  (** @inheritDoc *)
  method disposeInternal : unit meth
    
end   

(**
   A UI component that displays a gauge.
   A gauge displayes a current value within a round axis that represents a given range. The gauge is built from an external border, and internal border inside it, ticks and labels inside the internal border, and a needle that points to the current value.
   @param width The width in pixels.
   @param height The height in pixels.
   @param opt_domHelper The DOM helper object for the document we want to render
   in. *)
val gauge : (int -> int -> domHelper t opt -> gauge t) constr

module Gauge : sig
  (** Constant for a background color for a gauge area. *)
  val _RED : js_string t

  (** Constant for a background color for a gauge area. *)
  val _GREEN : js_string t
    
  (** Constant for a background color for a gauge area. *)
  val _YELLOW : js_string t
    
  (** The radius of the entire gauge from the canvas size. *)
  val _FACTOR_RADIUS_FROM_SIZE : float

  (** The ratio of internal gauge radius from entire radius. The remaining area is the border around the gauge. *)
  val _FACTOR_MAIN_AREA : float

  (** The ratio of the colored background area for value ranges. The colored area width is computed as InternalRadius * (1 - FACTOR_COLOR_RADIUS) *)
  val _FACTOR_COLOR_RADIUS : float

  (**The ratio of the major ticks length start position, from the radius. The major ticks length width is computed as InternalRadius (1 - FACTOR_MAJOR_TICKS) *)
  val _FACTOR_MAJOR_TICKS : float

  (** The ratio of the minor ticks length start position, from the radius. The minor ticks length width is computed as InternalRadius (1 - FACTOR_MINOR_TICKS) *)
  val _FACTOR_MINOR_TICKS : float

  (** The length of the needle front (value facing) from the internal radius. The needle front is the part of the needle that points to the value. *)
  val _FACTOR_NEEDLE_FRONT : float

  (** The length of the needle back relative to the internal radius. The needle back is the part of the needle that points away from the value. *)
  val _FACTOR_NEEDLE_BACK : float

  (** The width of the needle front at the hinge. This is the width of the curve control point, the actual width is computed by the curve itself. *)
  val _FACTOR_NEEDLE_WIDTH : float

  (** The width (radius) of the needle hinge from the gauge radius. *)
  val _FACTOR_NEEDLE_HINGE : float

  (** The title font size (height) for titles relative to the internal radius. *)
  val _FACTOR_TITLE_FONT_SIZE : float

  (** The offset of the title from the center, relative to the internal radius. *)
  val _FACTOR_TITLE_OFFSET : float

  (** The formatted value font size (height) relative to the internal radius. *)
  val _FACTOR_VALUE_FONT_SIZE : float

  (** The title font size (height) for tick labels relative to the internal radius. *)
  val _FACTOR_TICK_LABEL_FONT_SIZE : float

  (** The offset of the formatted value down from the center, relative to the internal radius. *)
  val _FACTOR_VALUE_OFFSET : float

  (** The font name for title text. *)
  val _TITLE_FONT_NAME : js_string t

  (** The maximal size of a step the needle can move (percent from size of range). If the needle needs to move more, it will be moved in animated steps, to show a smooth transition between values. *)
  val _NEEDLE_MOVE_MAX_STEP : float

  (** Time in miliseconds for animating a move of the value pointer. *)
  val _NEEDLE_MOVE_TIME : int

  (** Tolerance factor for how much values can exceed the range (being too low or too high). The value is presented as a position (percentage). *)
  val _MAX_EXCEED_POSITION_POSITION : float
end

