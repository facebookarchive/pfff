
             LablGTK2 : an interface to the GIMP Tool Kit

Needed:
	ocaml-3.11 or more
	gtk+-2.x (gtk+-2.12.x for full functionality)
	GNU make (there is no standard for conditionals)

Info/upgrades:
        http://wwwfun.kurims.kyoto-u.ac.jp/soft/olabl/lablgtk.html

Status:
  LablGtk2 is now pretty stable.
  
  An important change in gtk-2 is the use of unicode (utf8) for
  all strings. If you use non-ascii strings, you must imperatively
  convert them to unicode. This can be done with the
  [Glib.Convert.locale_to_utf8] function. If your input is already in
  utf8, it is still a good idea to validate it with
  Glib.Utf8.validate, as malformed utf8 strings may cause segmentation
  faults.
  Note that setlocale is now always called (except if you set
  GTK_SETLOCALE to 0 in the environment), but LC_NUMERIC is reverted
  to "C" to avoid problems with floating point conversion in Caml.

  Note that some widgets are only supported in newer versions of GTK+.
  If you use them in older versions, you will get a runtime error:
    Failure "Gobject.unsafe_create : type GtkActionGroup is not yet defined"
  For unsupported methods, the error message is a bit clearer:
    Failure "gdk_pixbuf_get_file_info unsupported in Gtk 2.x < 2.4"

How to compile:

  You should normally not need to modify Makefiles.
  
  In case you are using the SVN version you may have to first type
  "aclocal && autoconf".
  
  Type "./configure && make world && make install" to compile 
  with all supported options enabled (libgl, libglade, libgnomecanvas, 
  librsvg, native compilation, thread support).
  
  You may use "./configure --help" to check for the different 
  configuration options.

  Lablgtk2 specific options are:

	--with-libdir=/path: install libs in /path/lablgtk2 
			     and /path/stublibs
	--with-gl --without-gl: override autodetected GtkGLArea support. 
				Requires LablGL
	--with-glade --without-glade: override autodetected libglade support
	--with-rsvg --without-rsvg: override autodetected librsvg support
	--with-gnomecanvas --without-gnomecanvas: 
		override autodetected libgnomecanvas support
        --with-gnomeui --without-gnomeui:
                override autodetected libgnomeui support
        --with-panel --without-panel:
                override autodetected libpanelapplet support
        --with-gtkspell --without-gtkspell:
                override autodetected gtkspell support
        --with-gtksourceview --without-gtksourceview:
                override autodetected gtksourceview support
        --with-gtksourceview2 --without-gtksourceview2:
                override autodetected gtksourceview2 support
	--enable-debug: enable debug mode

  "make install DESTDIR=..." prefixes the installation
  directories with DESTDIR.

  A META file for findlib is provided, but it only handles the
  base library: I have no idea on how to avoid a combinatorial
  explosion with the various extensions.

Contents:

	gdk.ml		low-level interface to the General Drawing Kit
	gtk.ml		low-level interface to the GIMP Tool Kit
	gtkThread.ml	main loop for threaded version
	g[A-Z]*.ml	object-oriented interface to GTK
	gdkObj.ml	object-oriented interface to GDK

	lablgtk2	toplevel

	examples/*.ml	various examples

        applications/browser    an ongoing port of ocamlbrowser
        applications/camlirc    an IRC client (by Nobuaki Yoshida)

How to run the examples:
  In the examples directory just type:
	lablgtk2 ???.ml

  Before installing lablgtk2 you have to use -localdir:
	../src/lablgtk2 -localdir ???.ml

How to link them:
  The lablgtk2 script loads an extra module GtkInit, whose only contents is:
        let locale = GtkMain.Main.init ()
  You must either add this line, or add this module to your link,
  before calling any Gtk function.
  ocamlc -I +lablgtk2 -w s lablgtk.cma gtkInit.cmo ???.ml -o ???

How to use the threaded toplevel:

	% lablgtk2 -thread
	        Objective Caml version 3.09
	
	# let w = GWindow.window ~show:true ();;
        # let b = GButton.button ~packing:w#add ~label:"Hello!" ();;

  You should at once see a window appear, and then a button.
  The GTK main loop is running in a separate thread. Any command
  is immediately reflected by the system.
  Beware that with bytecode threads, you cannot switch threads within
  a callback. The only thread related command you may use in a
  callback is Thread.create. Calling blocking operations may cause
  deadlocks. On the other hand, all newly created threads will be run
  outside of the callback, so they can use all thread operations.
  There is no such problem for posix and win32 threads, but win32
  threads have problems of their own. See the windows port section
  lower.

  When using threads in a stand-alone application, you must link with
  gtkThread.cmo and call GtkThread.main in place of GMain.main.

Structure of the (raw) Gtk* modules:

  These modules are composed of one submodule for each class.
  Signals specific to a widget are in a Signals inner module.
  A setter function is defined to give access to set_param functions.

Structure of the G[A-Z]* modules:

  These modules provide classes to wrap the raw function calls.
  Here are the widget classes contained in each module:

  GPango        Pango font handling
  GDraw         Gdk pixmaps, etc...
  GObj		gtkobj, widget, style
  GData		data, adjustment, tooltips
  GContainer	container, item_container
  GWindow	window, dialog, color_selection_dialog, file_selection, plug
  GPack		box, button_box, table, fixed, layout, packer, paned, notebook
  GBin  	scrolled_window, event_box, handle_box, frame,
		aspect_frame, viewport, socket
  GButton	button, toggle_button, check_button, radio_button, toolbar
  GMenu		menu_item, tearoff_item, check_menu_item, radio_menu_item,
		menu_shell, menu, option_menu, menu_bar, factory
  GMisc		separator, statusbar, calendar, drawing_area,
		misc, arrow, image, pixmap, label, tips_query,
                color_selection, font_selection
  GTree		tree_item, tree, view (also tree/list_store, model)
  GList		list_item, liste, clist
  GEdit		editable, entry, spin_button, combo
  GRange	progress, progress_bar, range, scale, scrollbar
  GText		view (also buffer, iter, mark, tag, tagtable)

  While subtyping follows the Gtk widget hierarchy, you cannot always
  use width subtyping (i.e. #super is not unifiable with all the
  subclasses of super). Still, it works for some classes, like
  #widget and #container, and allows subtyping without coercion towards
  these classes (cf. #container in examples/pousse.ml for instance).

  Practically, each widget class is composed of:
  * a coerce method, returning the object coerced to the type widget.
  * an as_widget method, returning the raw Gtk widget used for packing, etc...
  * a destroy method, sending the destroy signal to the object.
  * a get_oid method, the equivalent of Oo.id for Gtk objects.
  * a connect sub-object, allowing one to widget specific
    signals (this is what prevents width subtyping in subclasses.)
  * a misc sub-object, giving access to miscellanous functionality of
    the basic gtkwidget class, and a misc#connect sub-object.
  * an event sub-object, for Xevent related functions (only if the widget
    has an Xwindow), and an event#connect sub-object.
  * a drag  sub-object, containing drag and drop functions,
    and a drag#connect sub-object.
  * widget specific methods.

  Here is a diagram of the structure (- for methods, + for sub-objects)
        - coerce : widget
        - as_widget : Gtk.widget obj
        - destroy : unit -> unit
        - get_oid : int
        - ...
        + connect : mywidget_signals
        |   - after
        |   - signal_name : callback:(... -> ...) -> GtkSignal.id
        + misc : misc_ops
        |   - show, hide, disconnect, ...
        |   + connect : misc_signals
        + drag : drag_ops
        |   - ...
        |   + connect : drag_signals
        + event : event_ops
        |   - add, ...
        |   + connect : event_signals

  You create a widget by [<Module>.<widget name> options ... ()].
  Many optional arguments are admitted. The last two of them, packing:
  and show:, allow you respectively to call a function on your newly
  created widget, and to decide wether to show it immediately or not.
  By default all widgets except toplevel windows (GWindow module) are
  shown immediately.

Default arguments:
  For many constructor or method arguments, default values are provided.
  Generally, this default value is defined by GTK, and you must refer
  to GTK's documentation.
  For ML defined defaults, usually default values are either false, 0, None
  or `NONE, according to the expected type.
  Important exceptions are ~show, which default to true in all widgets
  except those in GWindow, and ~fill, which defaults to true or `BOTH.

Note about unit as method argument:

  O'Caml introduces no distinction between methods having side-effects
  and methods simply returning a value. In practice, this is
  confusing, and awkward when used as callbacks. For this reason all
  methods having noticeable side-effects should take arguments, and
  unit if they have no argument.

ML-side signals:

  The GUtil module provides two kinds of utilities: a memo table, to be
  able to dynamically cast widgets to their original class, and more
  interesting ML-side signals.
  With ML-side signals, you can combine LablGTK widgets into your own
  components, and add signals to them. Later you can connect to these
  signals, just like GTK signals. This proved very efficient to
  develop complex applications, abstracting the plumbing between
  various components. Explanations are provided in GUtil.mli.

Comments on some widgets:
  GText has changed a lot since the alpha release. In particular, most
  movements with GText.iter are now functional. You can still modify
  destructively by using the #nocopy interface.

Contributed components:

  The GToolbox module contains contributed components to help you build
  your applications.

Memory management:

  Important efforts have been dedicated to cooperate with Gtk's
  reference counting mechanism. As a result you should generally be
  able to use Gdk/Gtk data structures without caring about memory
  management. They will be freed when nobody points to them any more.
  This also means that you do not need to pay too much attention to
  whether a data structure is still alive or not. If it is not, you
  should get an error rather than a core dump.
  The case of Gtk objects deserves special care. Since they are
  interactive, we cannot just destroy them when they are no longer
  referenced. They have to be explicitely destroyed. If a widget was
  added to a container widget, it will automatically be destroyed when
  its last container is destroyed. For this reason you need only
  destroy toplevel widgets.

  IMPORTANT: Some Gtk data structures are allocated in the Caml heap,
  and there use in signals (Gtk functions internally cally callbacks)
  relies on their address being stable during a function call. For
  this reason automatic compation is disabled in GtkMain. If you need
  it, you may use compaction through Gc.compact where it is safe
  (timeouts, other threads...), but do not enable automatic compaction.

LibGlade support:

  There is support for Glade generated XML UI description files, using
  libglade. You can read in a file, access to widgets, and define
  callbacks.
  A tool for extracting widget definitions from glade description is
  provided. It generates a wrapper class, and you can then generate an
  object corresponding to the intended layout, and access individual
  widgets through its methods. Example:

        % lablgladecc2 project1.glade > project1.ml
        % lablgtk2 -thread
        # #use "project1.ml" ;;
        class window1 : ...
        # let w1 = new window1 () ;;
        # w1#bind ~name:"on_paste1_activate"
            ~callback:(fun () -> w1#text1#insert "some text\n");;

  See lablgladecc2 -help for other features (tracing and source
  embedding).
  The executable must be linked with lablglade.cma.

GL extension:

  You can use lablgtk in combination with LablGL

  * get and install lablGL 1.04 from
      http://wwwfun.kurims.kyoto-u.ac.jp/soft/olabl/lablgl.html
  * get and install gtkglarea-1.99.0.tar.gz from
      ftp://ftp.gnome.org/pub/gnome/sources/gtkglarea/1.99/
    or any other gnome mirror site
  * reconfigure

  You can then use the widget GlGtk.gl_area as an OpenGL window.
  Some examples are in examples/GL, but basically any LablGL example
  can be easily ported.
  The executable must be linked with both lablgl.cma and
  lablgtkgl.cma.

SVG support:

  This binding was contributed by Olivier Andrieu.
  It requires librsvg-2.x (preferably 2.2.x).
  See an example in examples/rsvg.
  The executable must be linked with lablrsvg.cma.

GnomeCanvas support:

  This binding was also contributed by Olivier Andrieu.
  It requires libgnomecanvas-2.x.
  See examples in examples/canvas.
  The executable must be linked with lablgnomecanvas.cma.

GtkSourceView 1 support:
  This binding was contributed by Maxence Guesdon and Stefano Zacchiroli.
  It requires libgtksourceview-1.x.
  See examples in examples/sourceview.
  The executable must be linked with lablgtksourceview.cma.

GtkSourceView 2 support:
  This binding was contributed by Benjamin Monate: it is based on 
  the aforementioned GtksourceView 1 support.
  It requires libgtksourceview-2.x.
  See examples in examples/sourceview/*2.ml
  The executable must be linked with lablgtksourceview2.cma.

Windows port

  See README.win32 for detailed information on installation.

  If you want to use threads, you must be aware of windows specific
  restrictions; see for instance:
     http://article.gmane.org/gmane.comp.video.gimp.windows.devel/314
  I.e. all GTK related calls must occur in the same thread, the one
  that runs the main loop. If you want to call them from other threads
  you need to do some forwarding. Fortunately, with a functional
  language this is easy. Two functions,
    val async : ('a -> unit) -> 'a -> unit
    val sync : ('a -> 'b) -> 'a -> 'b
  are available in the GtkThread module to help you. They will forward
  your call to the main thread (between handling two GUI events). This
  can be either asynchronous or synchronous. In the synchronous case,
  beware of deadlocks (the trivial case, when you are calling from the
  same thread, is properly avoided). Note also that since callbacks
  are always called from the main loop thread, you can freely use GTK
  in them. Also, non-graphical operations are thread-safe.
  Here is an example using the lablgtk toplevel with threads:
	% lablgtk2.bat -thread
	        Objective Caml version 3.09

        # open GtkThread;;	
	# let w = sync (GWindow.window ~show:true) ();;
        # let b = sync (GButton.button ~packing:w#add ~label:"Hello!") ();;
        # b#connect#clicked (fun () -> prerr_endline "Hello");;

Authors:
	Jacques Garrigue <garrigue@math.nagoya-u.ac.jp>
	Benjamin Monate  <benjamin.monate@free.fr>
        Olivier Andrieu  <oandrieu@nerim.net>
	Jun Furuse       <furuse@yl.is.s.u-tokyo.ac.jp>
        Maxence Guesdon  <maxence.guesdon@inria.fr>
        Stefano Zacchiroli  <zack@cs.unibo.it>
  For lablgtk1:
	Hubert Fauque  <hubert.fauque@wanadoo.fr>
	Koji Kagawa    <kagawa@eng.kagawa-u.ac.jp>
				   
Bug reports:
	Jacques Garrigue <garrigue@math.nagoya-u.ac.jp>

$Id: README 1486 2009-09-25 09:18:09Z garrigue $
