open Tools

let id s = Goog.Tools.Union.i2 (Js.string s)

let tooltip = jsnew Goog.Ui.advancedTooltip(
  Js.some (id "btn"), Js.null, Js.null)
let _ =
  tooltip##className <- Js.string "tooltip";
  tooltip##setHtml(
  Js.string
    "<h2>AdvancedTooltip</h2>
    <ul><li>Move cursor towards the tooltip (<em>that's me!</em>)
and see that it remains open.</li>
<li>Before reaching it start moving the cursor in another
  direction...</li>
<li>Once the cursor reaches the tooltip the cursor tracking is turned
  off and  a 10px 'padding' around it gets added. As long as the cursor
    stays inside the box formed by the tooltip and the padding it remains
open.</li></ul><hr/><div style=\"text-align: center;\">
  <button id=\"btn-nest\">Hover me</button>&nbsp;
  <button id=\"btn-close\">Close</button></div>");
  tooltip##setHotSpotPadding(jsnew Goog.Math.box(5, 5, 5, 5));
  tooltip##setCursorTracking(Js._true);
  tooltip##setMargin(
  Js.some (Goog.Tools.Union.i1 (jsnew Goog.Math.box(100, 0, 0, 100))), 
    Js.null, Js.null, Js.null);
  tooltip##setHideDelayMs(250)
    
let at = jsnew Goog.Ui.advancedTooltip(
  Js.some (id "btn-nest"), Js.null, Js.null) in
at##setHtml(
Js.string "Clicking<br> this<br> button<br> has no effect.")
let at1 = jsnew Goog.Ui.tooltip(
  Js.some (id "btn-close"), Js.some (Js.string "Closes tooltip"), Js.null)

let bc = 
  Js.Opt.get (Dom_html.document##getElementById(Js.string "btn-close"))
    (fun () -> assert false)
 
let _ =
  Goog.Events.listen 
    (Goog.Tools.Union.i2 bc)
    (Js.string "click")
    (Js.wrap_callback (fun () -> tooltip##setVisible(Js._false);))
    Js.null
    
