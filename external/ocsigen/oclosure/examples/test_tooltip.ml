open Js

let id = Goog.Tools.Union.i2 

let msg1 = "Tooltip widget. Appears next to the cursor when over an attached element or next to the element if it's active."
let tooltip1 = jsnew Goog.Ui.tooltip(
 Js.some (id (Js.string "btn1")), 
  Js.some (Js.string msg1), Js.null)
  
let tooltip2 = jsnew Goog.Ui.tooltip(
  Js.some (id (Js.string "btn2")), Js.null, Js.null)
let _ = 
  tooltip2##className <- Js.string "tooltip2";
  tooltip2##setHtml(
  Js.string "This is message two, using a different class name for the tooltip and <strong>HTML</strong> <em>markup</em>.<br> <button id=\"btn-nest\">Hover me</button>");
  tooltip2##attach(id (Js.string "btn5"))

let tooltip3 = jsnew Goog.Ui.tooltip(
  Js.some (id (Js.string "btn3")), 
  Js.some (Js.string "Tooltip for button 3"),
  Js.null)

let msg4 = "Tooltip for button 4, demonstrating that it's positioned
    correctly even when inside a scrolling container."
let tooltip4 = jsnew Goog.Ui.tooltip(
  Js.some (id (Js.string"btn4")), 
  Js.some (Js.string msg4), Js.null)

let msg5 = "tooltip for the word 'tooltips'."
let tooltip5 = jsnew Goog.Ui.tooltip(
  Js.some (id (Js.string "txt1")),
  Js.some (Js.string msg5), Js.null)

let _ = tooltip5##attach(id (Js.string "txt3"))
    
let tooltip6 = jsnew Goog.Ui.tooltip(
  Js.some (id (Js.string "txt2")), 
  Js.some (Js.string "outer tooltip"),
  Js.null)
    
let tooltip7 = jsnew Goog.Ui.tooltip(
  Js.some (id (Js.string "btn-nest")), Js.null, Js.null)
let _ = tooltip7##setHtml(Js.string "Even nested<br>tooltips!")
    
