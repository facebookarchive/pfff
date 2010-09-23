(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

external init : unit -> unit = "ml_gtkaction_init"
let () = init ()

module GtkAction = GtkActionProps
open GtkAction

class action_signals obj = object
  inherit [[> Gtk.action]] GObj.gobject_signals obj
  inherit OgtkActionProps.action_sigs
end

class action_skel obj = object
  val obj = obj
  method private obj = obj
  inherit OgtkActionProps.action_props
  method as_action = (obj :> Gtk.action Gobject.obj)

  method activate () = Action.activate obj
  method is_sensitive = Action.is_sensitive obj
  method is_visible = Action.is_visible obj
  method connect_proxy w = Action.connect_proxy obj (GObj.as_widget w)
  method disconnect_proxy w = Action.disconnect_proxy obj (GObj.as_widget w)
  method get_proxies = List.map (new GObj.widget) (Action.get_proxies obj)
  method connect_accelerator () = Action.connect_accelerator obj
  method disconnect_accelerator () = Action.disconnect_accelerator obj
  method set_accel_path = Action.set_accel_path obj
  method set_accel_group = Action.set_accel_group obj
  method block_activate_from (w : GObj.widget) = Action.block_activate_from obj w#as_widget
  method unblock_activate_from (w : GObj.widget) = Action.unblock_activate_from obj w#as_widget
end

class action obj = object
  inherit action_skel obj
  method connect = new action_signals obj
end

let action ~name () =
  new action (Action.create ~name [])

class toggle_action_signals obj = object
  inherit action_signals obj
  inherit OgtkActionProps.toggle_action_sigs
end

class toggle_action_skel obj = object
  inherit action_skel obj
  inherit OgtkActionProps.toggle_action_props
  method toggled () = ToggleAction.toggled obj
  method set_active = ToggleAction.set_active obj
  method get_active = ToggleAction.get_active obj
end

class toggle_action obj = object
  inherit toggle_action_skel obj
  method connect = new toggle_action_signals obj
end

let toggle_action ~name () =
  new toggle_action (ToggleAction.create [ Gobject.param Action.P.name name ])

class radio_action_signals obj = object
  inherit toggle_action_signals obj
  method changed ~callback =
    GtkSignal.connect 
      ~sgn:RadioAction.S.changed
      ~callback:(fun o -> callback (RadioAction.get_current_value o))
      ~after obj
end

class radio_action obj = object
  inherit toggle_action_skel obj
  inherit OgtkActionProps.radio_action_props
  method connect = new radio_action_signals obj
  method as_radio_action = (obj :> Gtk.radio_action Gobject.obj)
  method get_current_value = RadioAction.get_current_value obj
end

let radio_action ?group ~name ~value () =
  new radio_action (RadioAction.create 
		      (Gobject.Property.may_cons RadioAction.P.group
			 (Gaux.may_map (fun g -> Some (g#as_radio_action)) group)
			 [ Gobject.param Action.P.name name ;
			   Gobject.param RadioAction.P.value value ]))

class action_group_signals obj = object (self)
  inherit [[> Gtk.action_group]] GObj.gobject_signals obj
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method connect_proxy ~callback = self#connect
    {ActionGroup.S.connect_proxy with GtkSignal.marshaller = fun f ->
     GtkSignal.marshal2 
	(Gobject.Data.gobject : Gtk.action Gtk.obj Gobject.data_conv) 
	GObj.conv_widget
       "GtkActionGroup::connect_proxy" f}
      (fun o -> callback (new action o))
  method disconnect_proxy ~callback = self#connect
    {ActionGroup.S.disconnect_proxy with GtkSignal.marshaller = fun f ->
     GtkSignal.marshal2 
	(Gobject.Data.gobject : Gtk.action Gtk.obj Gobject.data_conv) 
	GObj.conv_widget
	"GtkActionGroup::disconnect_proxy" f}
      (fun o -> callback (new action o))
  method post_activate ~callback = self#connect ActionGroup.S.post_activate
      (fun o -> callback (new action o))
  method pre_activate ~callback = self#connect ActionGroup.S.pre_activate
      (fun o -> callback (new action o))
end

class action_group obj = object
  val obj = obj
  method private obj = obj
  inherit OgtkActionProps.action_group_props
  method as_group = (obj :> Gtk.action_group Gobject.obj)
  method connect = new action_group_signals obj
  method get_action n = new action (ActionGroup.get_action obj n)
  method list_actions = List.map (new action) (ActionGroup.list_actions obj)
  method add_action : 'a. (#action_skel as 'a) -> unit = 
    fun a -> ActionGroup.add_action obj a#as_action
  method add_action_with_accel : 'a. ?accel:string -> (#action_skel as 'a) -> unit = 
    fun ?accel a -> ActionGroup.add_action_with_accel obj a#as_action accel
  method remove_action : 'a. (#action_skel as 'a) -> unit = 
    fun a -> ActionGroup.remove_action obj a#as_action
end

let action_group ~name () =
  new action_group (ActionGroup.create ~name [])

type 'a entry = action_group -> 'a

let add_single_action ret a ?stock ?label ?accel ?tooltip
    (group : #action_group) =
  Gaux.may a#set_label label ;
  Gaux.may a#set_tooltip tooltip ;
  Gaux.may a#set_stock_id stock ;
  group#add_action_with_accel ?accel a ;
  ret a

let add_action name ?callback =
  let a = action ~name () in
  Gaux.may callback
    ~f:(fun cb -> a#connect#activate ~callback:(fun () -> cb a)) ;
  add_single_action ignore a

let add_toggle_action name ?active ?callback =
  let a = toggle_action ~name () in
  Gaux.may a#set_active active ;
  Gaux.may callback
    ~f:(fun cb -> a#connect#activate ~callback:(fun () -> cb a)) ;
  add_single_action ignore a

let add_radio_action name value =
  let a = radio_action ~name ~value () in
  add_single_action (fun a -> a) a
  
let add_actions ac_group =
  List.iter (fun f -> let () = f ac_group in ())
let group_radio_actions ?init_value ?callback radio_action_entries ac_group =
  let last_radio_ac =
    List.fold_left 
      (fun radio_grp f -> 
	let radio_ac = f ac_group in
	radio_ac#set_group radio_grp ;
	Gaux.may 
	  (fun init_v -> radio_ac#set_active (radio_ac#value = init_v)) 
	  init_value ;
	Some radio_ac#as_radio_action)
      None radio_action_entries in
  Gaux.may
    (fun cb ->
      Gaux.may
	(fun o ->
	  GtkSignal.connect
	    ~sgn:RadioAction.S.changed
	    ~callback:(fun curr -> cb (RadioAction.get_current_value curr))
	    o)
	last_radio_ac)
    callback ;
  ()

class ui_manager_signals obj = object (self)
  inherit [[> Gtk.ui_manager]] GObj.gobject_signals obj
  inherit OgtkActionProps.ui_manager_sigs
  method connect_proxy ~callback = self#connect
    {UIManager.S.connect_proxy with GtkSignal.marshaller = fun f ->
     GtkSignal.marshal2 
	(Gobject.Data.gobject : Gtk.action Gtk.obj Gobject.data_conv) 
	GObj.conv_widget
       "GtkUIManager::connect_proxy" f}
      (fun o -> callback (new action o))
  method disconnect_proxy ~callback = self#connect
    {UIManager.S.disconnect_proxy with GtkSignal.marshaller = fun f ->
     GtkSignal.marshal2 
	(Gobject.Data.gobject : Gtk.action Gtk.obj Gobject.data_conv) 
	GObj.conv_widget
       "GtkUIManager::disconnect_proxy" f}
      (fun o -> callback (new action o))
  method post_activate ~callback = self#connect UIManager.S.post_activate
      (fun o -> callback (new action o))
  method pre_activate ~callback = self#connect UIManager.S.pre_activate
      (fun o -> callback (new action o))
end

type ui_id = int

let invalid_id = 0

class ui_manager obj = object
  val obj = obj
  method private obj = obj
  inherit OgtkActionProps.ui_manager_props
  method connect = new ui_manager_signals obj
  method as_ui_manager = (obj:> Gtk.ui_manager Gtk.obj)
  method insert_action_group (g : action_group) = 
    UIManager.insert_action_group obj g#as_group
  method remove_action_group (g : action_group) =
    UIManager.remove_action_group obj g#as_group
  method get_action_groups =
    List.map (new action_group) (UIManager.get_action_groups obj)
  method get_accel_group = UIManager.get_accel_group obj
  method get_widget s = new GObj.widget (UIManager.get_widget obj s)
  method get_toplevels kind =
    List.map (new GObj.widget) (UIManager.get_toplevels obj kind)
  method get_action s = new action (UIManager.get_action obj s)
  method add_ui_from_string = UIManager.add_ui_from_string obj
  method add_ui_from_file = UIManager.add_ui_from_file obj
  method new_merge_id () = UIManager.new_merge_id obj
  method add_ui = UIManager.add_ui obj
  method remove_ui = UIManager.remove_ui obj
  method ensure_update () = UIManager.ensure_update obj
end

let ui_manager () =
  new ui_manager (UIManager.create [])
