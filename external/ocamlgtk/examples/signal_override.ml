(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

module C = Gobject.Closure

let add_closure argv =
  Printf.eprintf "invoking overridden ::add closure, %d args, " argv.C.nargs ;
  let typ = C.get_type argv 1 in
  Printf.eprintf "widget %s\n" (Gobject.Type.name typ) ;
  flush stderr ;
  GtkSignal.chain_from_overridden argv

let derived_frame_name = "GtkFrameCaml"

let derived_frame_gtype = 
  lazy begin
    let parent = Gobject.Type.from_name "GtkFrame" in
    let t = Gobject.Type.register_static ~parent ~name:derived_frame_name in
    GtkSignal.override_class_closure GtkBase.Container.S.add t (C.create add_closure) ;
    t
  end

let create_derived_frame =
  GtkBin.Frame.make_params [] 
    ~cont:(fun pl -> 
      GContainer.pack_container pl 
	~create:(fun pl -> 
	  ignore (Lazy.force derived_frame_gtype) ;
	  new GBin.frame (GtkObject.make derived_frame_name pl : Gtk.frame Gtk.obj)))

let main = 
  let w = GWindow.window ~title:"Overriding signals demo" () in
  w#connect#destroy GMain.quit ;

  let f = create_derived_frame ~label:"Talking frame" ~packing:w#add () in

  let l = GMisc.label ~markup:"This is the <b>GtkFrame</b>'s content" ~packing:f#add () in

  w#show () ;
  GMain.main ()
