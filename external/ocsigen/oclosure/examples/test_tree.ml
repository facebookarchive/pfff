open Goog.Ui.Tree
open Js

let d = Dom_html.document

let tree : unit treeControl t = 
  jsnew treeControl(Js.string "Fac", Js.null, Js.null)

let ufrInfo = tree##createNode(Js.string "Ufr Info")
let ufrChimie = tree##createNode(Js.string "Ufr chimie")
let _ = BaseNode.add tree ufrInfo Js.null; 
  BaseNode.add tree ufrChimie Js.null

let e = tree##createNode(Js.string "Gab")
let _ = BaseNode.add ufrInfo e Js.null

let abr = Js.Opt.get (d##getElementById (Js.string "treeContainer"))
    (fun () -> assert false)

let _ = tree##render(Obj.magic abr)
