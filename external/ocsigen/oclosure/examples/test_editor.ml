let get_el s = Js.Opt.get (Dom_html.document##getElementById(Js.string s))
    (fun _ -> Dom_html.window##alert (Js.string s); assert false)

let get_textarea s = Js.Opt.get (Dom_html.CoerceTo.textarea (get_el s))
    (fun _ -> assert false)

(* Create an editable field. *)
let myField = jsnew Goog.Geditor.field(Js.string "editMe", Js.null)

let updateFieldContents () = 
  (get_textarea "fieldContents")##value <- myField##getCleanContents()
 
(* Create and register all of the editing plugins you want to use *)
let _ = 
  Goog.Geditor.Field.registerPlugin myField 
    (jsnew Goog.Geditor.Plugins.basicTextFormatter());
  Goog.Geditor.Field.registerPlugin myField 
    (jsnew Goog.Geditor.Plugins.removeFormatting());
  Goog.Geditor.Field.registerPlugin myField 
    (jsnew Goog.Geditor.Plugins.undoRedo(Js.null));
  Goog.Geditor.Field.registerPlugin myField 
    (jsnew Goog.Geditor.Plugins.listTabHandler());
  Goog.Geditor.Field.registerPlugin myField 
    (jsnew Goog.Geditor.Plugins.spacesTabHandler());
  Goog.Geditor.Field.registerPlugin myField 
    (jsnew Goog.Geditor.Plugins.enterHandler());
  Goog.Geditor.Field.registerPlugin myField 
    (jsnew Goog.Geditor.Plugins.headerFormatter());
  Goog.Geditor.Field.registerPlugin myField 
    (jsnew Goog.Geditor.Plugins.loremIpsum(Js.string "Click here to edit"));
  Goog.Geditor.Field.registerPlugin myField 
    (jsnew Goog.Geditor.Plugins.linkDialogPlugin());
  Goog.Geditor.Field.registerPlugin myField 
    (jsnew Goog.Geditor.Plugins.linkBubble(Js.array [||]))
 
(* Specify the buttons to add to the toolbar, using built in default buttons. *)
let buttons = Js.array (Array.map Goog.Tools.Union.i1 [|
  Goog.Geditor.Command._BOLD;
  Goog.Geditor.Command._ITALIC;
  Goog.Geditor.Command._UNDERLINE;
  Goog.Geditor.Command._FONT_COLOR;
  Goog.Geditor.Command._BACKGROUND_COLOR;
  Goog.Geditor.Command._FONT_FACE;
  Goog.Geditor.Command._FONT_SIZE;
  Goog.Geditor.Command._LINK;
  Goog.Geditor.Command._UNDO;
  Goog.Geditor.Command._REDO;
  Goog.Geditor.Command._UNORDERED_LIST;
  Goog.Geditor.Command._ORDERED_LIST;
  Goog.Geditor.Command._INDENT;
  Goog.Geditor.Command._OUTDENT;
  Goog.Geditor.Command._JUSTIFY_LEFT;
  Goog.Geditor.Command._JUSTIFY_CENTER;
  Goog.Geditor.Command._JUSTIFY_RIGHT;
  Goog.Geditor.Command._SUBSCRIPT;
  Goog.Geditor.Command._SUPERSCRIPT;
  Goog.Geditor.Command._STRIKE_THROUGH;
  Goog.Geditor.Command._REMOVE_FORMAT
|])

let myToolbar = Goog.Ui.Editor.DefaultToolbar.makeToolbar 
    buttons
    (get_el "toolbar")
    Js.null
 
(* Hook the toolbar into the field *)
let myToolbarController =
  jsnew Goog.Ui.Editor.toolbarController(myField, myToolbar)
 
let get_input s = Js.Opt.get (Dom_html.CoerceTo.input(get_el s))
    (fun _ -> assert false)

(* Watch for field changes, to display below.*)
let _ = ignore(
  Goog.Events.listen
    (Goog.Tools.Union.i1 myField)
    (Goog.Geditor.Field.EventType._DELAYEDCHANGE)
    (Js.wrap_callback updateFieldContents)
    Js.null);
  myField##makeEditable(Js.null);
  (get_input "setFieldContent_b")##onclick <- 
    Dom_html.handler (
      fun _ -> 
	myField##setHtml(Js._false, 
			 Js.some ((get_textarea "fieldContents")##value), 
			 Js.null, Js.null); Js._false);
  updateFieldContents();
