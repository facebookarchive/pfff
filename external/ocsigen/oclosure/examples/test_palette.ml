open Js

let doc = Dom_html.document
let to_s = Js.string
let get_el s = Js.Opt.get (Dom_html.document##getElementById (Js.string s))
  (fun _ -> assert false)
let (^^) a b = to_s ((Js.to_string a)^b)
let get_div x : Dom_html.divElement t= Js.Opt.get x (fun _ -> assert false) 
let get x = Js.Opt.get x (fun _ -> assert false) 
let js_arr t = Js.array (Array.map Js.string t)


let numbers = [|1;2;3;4;5;6;7;8;9|]
let items = Js.array (Array.map 
    (fun i ->doc##createTextNode (to_s (string_of_int i))) numbers)

let p1 = jsnew Goog.Ui.palette(items, Js.null, Js.null) in
p1##render(Js.some (get_el "p1"));
(p1##getElement())##className <- ((p1##getElement())##className)^^" simple-palette";
Goog.Events.listen 
  (Goog.Tools.Union.i1 p1)
  Goog.Ui.Component.EventType._ACTION
  (Js.wrap_callback (fun () -> 
    (get_div (Dom_html.CoerceTo.div(get_el "p1_value")))##innerHTML <- 
      get (p1##getSelectedItem())##nodeValue))
  Js.null

let onColorEvent palette =
  let color = get (palette##getSelectedColor()) in
  ((get_el"cp_value")##style)##backgroundColor <- color;
  (get_el"cp_value")##title <- color;
  (get_el"cp_text")##innerHTML <- color

let createColorPaletteDemo colors width caption = 
  let p = Dom_html.createP doc in
  p##innerHTML <- caption;
  ignore((get_el "cp")##appendChild((p : #Dom.node t :> Dom.node t)));
  let cp = jsnew Goog.Ui.colorPalette(Js.some colors, Js.null, Js.null) in
  cp##setSize(Goog.Tools.Union.i2 width, Js.null); 
  cp##render(Js.some (get_el"cp"));
  ignore(Goog.Events.listen 
    (Goog.Tools.Union.i1 cp)
    Goog.Ui.Component.EventType._ACTION 
    (Js.wrap_callback (fun () -> onColorEvent cp)) Js.null)
    
let _ = 
  createColorPaletteDemo 
    (js_arr [|"black"; "blue"; "red"; "magenta";
      "green"; "cyan"; "orange"; "yellow"; "#404040"; "#808080"; "#b0b0b0";
      "white"|])
    4
    (Js.string "This is a 4x3 color palette with named colors:");


  createColorPaletteDemo
    (js_arr [|"#F00"; "#F90"; "#FF0"; "#3F3"; "#0FF"; "#00F"; "#90F"; "#F0F"|])
    8
    (to_s "These colors are bright:");

  createColorPaletteDemo
    (js_arr [|
  "#EA9999"; "#F9CB9C"; "#FFE599"; "#B6D7A8";
  "#A2C4C9"; "#9FC5E8"; "#B4A7D6"; "#D5A6BD";
  "#E06666"; "#F6B26B"; "#FFD966"; "#93C47D";
  "#76A5AF"; "#6FA8DC"; "#8E7CC3"; "#C27BA0";
  "#CC0000"; "#E69138"; "#F1C232"; "#6AA84F";
  "#45818E"; "#3D85C6"; "#674EA7"; "#A64D79"|])
    8
    (to_s "This is a lovely pastelle color palette:");

  createColorPaletteDemo (js_arr [|"#000"; "#444"; "#666"; "#999"; 
				   "#AAA"; "#CCC"; "#EEE"; "#FFF"|]) 8
    (to_s"This is a grey scale color palette:")

let cp = jsnew Goog.Ui.customColorPalette(js_arr[|"#FE1";"#ACD";"#119"|],
					  Js.null, Js.null)
let _ = 
  cp##setSize(Goog.Tools.Union.i2 6, Js.null);
  cp##render(Js.some (get_el"ccp"));
  (cp##getElement())##className <- ((cp##getElement())##className)^^" color-picker";
  Goog.Events.listen
    (Goog.Tools.Union.i1 cp)
    Goog.Ui.Component.EventType._ACTION
    (Js.wrap_callback (fun () -> 
      let palette = cp in
      let color = get (palette##getSelectedColor()) in
      (get_el"ccp_value")##style##backgroundColor <- color;
      (get_el"ccp_value")##title <- color;
      (get_el"ccp_text")##innerHTML <- color
	  ))
    Js.null
