
type controlContent 

module ControlContent : sig
  val string : Js.js_string Js.t -> controlContent
  val node : Dom.node Js.t -> controlContent
  val node_array : Dom.node Js.t Js.js_array Js.t -> controlContent
  val string_nodeList : Js.js_string Js.t Dom.nodeList Js.t -> controlContent
end 
