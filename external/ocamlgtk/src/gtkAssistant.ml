open Gaux
open Gobject
open Gtk
open Tags
open GtkAssistantProps
open GtkBase

external _gtkassistant_init : unit -> unit = "ml_gtkassistant_init"
let () = _gtkassistant_init ()

module Assistant = struct
  include Assistant
end
