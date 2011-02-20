let get_el s = Js.Opt.get (Dom_html.document##getElementById (Js.string s))
    (fun _ -> assert false)

let parent_node e = Js.Unsafe.coerce
   (Js.Opt.get (e##parentNode) (fun _ -> assert false))

let all = jsnew Goog.Ui.checkbox(Js.null)
let leaf1 = jsnew Goog.Ui.checkbox(Js.null)
let leaf2 = jsnew Goog.Ui.checkbox(Js.null)

let _ =
  all##decorate(get_el "all");
  all##setLabel(parent_node all##getElement());
  leaf1##decorate(get_el "leaf1");
(*leaf1##setLabel(leaf1.getElement().parentNode);*)
  leaf2##decorate(get_el "leaf2")
(*  leaf2##setLabel(leaf2.getElement().parentNode);*)
  
let rootChanged e =
  leaf1##setChecked_(all##getChecked());
  leaf2##setChecked_(all##getChecked())

let leafChanged e =
  let same = leaf1##getChecked() == leaf2##getChecked() in
  all##setChecked_(if same then leaf1##getChecked() else Js.null)
    
let _ =
 ignore(Goog.Events.listen 
    (Goog.Tools.Union.i1 all)
    Goog.Ui.Component.EventType._CHANGE 
    (Js.wrap_callback rootChanged)
    Js.null);
  ignore(Goog.Events.listen 
    (Goog.Tools.Union.i1 leaf1)
    Goog.Ui.Component.EventType._CHANGE 
    (Js.wrap_callback leafChanged)
    Js.null);
  ignore(Goog.Events.listen 
    (Goog.Tools.Union.i1 leaf2)
    Goog.Ui.Component.EventType._CHANGE 
    (Js.wrap_callback leafChanged)
    Js.null);
  
