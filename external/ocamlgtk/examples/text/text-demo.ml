(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let create_tags (buffer:GText.buffer) =
  buffer#create_tag ~name:"heading" 
    [`WEIGHT `BOLD; `SIZE (15*Pango.scale)];
  buffer#create_tag ~name:"italic" [`STYLE `ITALIC];
  buffer#create_tag ~name:"bold" [`WEIGHT `BOLD];  
  buffer#create_tag ~name:"big" [`SIZE 20];
  buffer#create_tag ~name:"xx-small" [`SCALE `XX_SMALL];
  buffer#create_tag ~name:"x-large" [`SCALE `X_LARGE];
  buffer#create_tag ~name:"monospace" [`FAMILY "monospace"];
  buffer#create_tag ~name:"blue_foreground" [`FOREGROUND "blue"];
  buffer#create_tag ~name:"red_background" [`BACKGROUND "red"];

  let stipple = Gdk.Bitmap.create_from_data 2 2 "\002\001" in
  buffer#create_tag ~name:"background_stipple" [`BACKGROUND_STIPPLE stipple];
  buffer#create_tag ~name:"foreground_stipple" [`FOREGROUND_STIPPLE stipple];
  buffer#create_tag ~name:"big_gap_before_line" [`PIXELS_ABOVE_LINES 30];
  buffer#create_tag ~name:"big_gap_after_line" [`PIXELS_BELOW_LINES 30];
  buffer#create_tag ~name:"double_spaced_line" [`PIXELS_INSIDE_WRAP 10];
  buffer#create_tag ~name:"not_editable" [`EDITABLE false];
  buffer#create_tag ~name:"word_wrap" [`WRAP_MODE `WORD];
  buffer#create_tag ~name:"char_wrap" [`WRAP_MODE `CHAR];
  buffer#create_tag ~name:"no_wrap" [`WRAP_MODE `NONE];
  buffer#create_tag ~name:"center" [`JUSTIFICATION `CENTER];
  buffer#create_tag ~name:"right_justify" [`JUSTIFICATION `RIGHT];
  buffer#create_tag ~name:"wide_margins" [`LEFT_MARGIN  50; `RIGHT_MARGIN 50];
  buffer#create_tag ~name:"strikethrough" [`STRIKETHROUGH true];
  buffer#create_tag ~name:"underline" [`UNDERLINE `SINGLE];
  buffer#create_tag ~name:"double_underline" [`UNDERLINE `DOUBLE];
  buffer#create_tag ~name:"superscript"
    [`RISE (10*Pango.scale); `SIZE (8*Pango.scale)];
  
  buffer#create_tag ~name:"subscript"
    [`RISE (-10*Pango.scale); `SIZE (8*Pango.scale)];
  buffer#create_tag ~name:"rtl_quote" 
    [`WRAP_MODE `WORD;
     `DIRECTION `RTL;
     `INDENT 30;
     `LEFT_MARGIN 20;
     `RIGHT_MARGIN 20];
  ()

let insert_text (buffer:GText.buffer) =  
  let pixbuf = GdkPixbuf.from_file "gtk-logo-rgb.gif" in
  let scaled = GdkPixbuf.create ~has_alpha:true ~width:32 ~height:32 () in
  GdkPixbuf.scale ~dest:scaled ~width:32 ~height:32 ~interp:`BILINEAR pixbuf;
  let pixbuf = scaled in
  let iter = buffer#get_iter_at_char 0 in
  buffer#insert ~iter "The text widget can display text with all kinds of nifty attributes. It also supports multiple views of the same buffer; this demo is showing the same buffer in two places.\n\n";
  buffer#insert ~iter ~tag_names:["heading"] "Font styles. ";
  buffer#insert ~iter "For example, you can have ";
  buffer#insert ~iter ~tag_names:["italic"] "italic";
  buffer#insert ~iter ", ";
  buffer#insert ~iter ~tag_names:["bold"] "bold";
  buffer#insert ~iter ", or ";
  buffer#insert ~iter ~tag_names:["monospace"] 
    "monospace(typewriter)";
  buffer#insert ~iter ", or ";
  buffer#insert ~iter ~tag_names:["big"] "big";
  buffer#insert ~iter " text. ";
  buffer#insert ~iter "It's best not to hardcode specific text sizes; you can use relative sizes as with CSS, such as ";
  buffer#insert ~iter ~tag_names:["xx-small"] "xx-small";
  buffer#insert ~iter ", or ";
  buffer#insert ~iter ~tag_names:["x-large"] "x-large";
  buffer#insert ~iter " to ensure that your program properly adapts if the user changes the default font size.\n\n";
  buffer#insert ~iter ~tag_names:["heading"] "Colors. ";
  buffer#insert ~iter "Colors such as ";
  buffer#insert ~iter ~tag_names:["blue_foreground"] "a blue foreground";
  buffer#insert ~iter ", or ";
  buffer#insert ~iter ~tag_names:["red_background"] "a red background";
  buffer#insert ~iter ", or even ";
  buffer#insert ~iter ~tag_names:["red_background";"background_stipple"] 
    "a stippled red background";
  buffer#insert ~iter " or ";
  buffer#insert ~iter ~tag_names:["blue_foreground";
				   "red_background";
				   "foreground_stipple"] 
    "a stippled blue foreground on solid red background";
  buffer#insert ~iter " (select that to read it) can be used.\n\n";
  buffer#insert ~iter  ~tag_names:["heading"] 
    "Underline, strikethrough, and rise. ";
  buffer#insert ~iter  ~tag_names:["strikethrough"] 
    "Strikethrough";
  buffer#insert ~iter ", ";
  buffer#insert ~iter  ~tag_names:["underline"] 
    "underline";
  buffer#insert ~iter ", ";
  buffer#insert ~iter  ~tag_names:["double_underline"] 
    "double underline";
  buffer#insert ~iter ", ";
  buffer#insert ~iter  ~tag_names:["superscript"] 
    "superscript";
  buffer#insert ~iter ", ";
  buffer#insert ~iter  ~tag_names:["subscript"] 
    "subscript";
  buffer#insert ~iter " are all supported.\n\n";
  buffer#insert ~iter  ~tag_names:["heading"] 
    "Images";
  buffer#insert ~iter "The buffer can have images in it: ";
  buffer#insert_pixbuf ~iter ~pixbuf;
  buffer#insert_pixbuf ~iter ~pixbuf;
  buffer#insert_pixbuf ~iter ~pixbuf;
  buffer#insert ~iter " for example.\n\n";
  buffer#insert ~iter  ~tag_names:["heading"] 
    "Spacing";
  buffer#insert ~iter
    "You can adjust the amount of space before each line.\n";
  buffer#insert ~iter  ~tag_names:["big_gap_before_line";"wide_margins"] 
    "This line has a whole lot of space before it.\n";
  buffer#insert ~iter  ~tag_names:["big_gap_after_line";"wide_margins"] 
    "You can also adjust the amount of space after each line; this line has a whole lot of space after it.\n";
  buffer#insert ~iter  ~tag_names:["double_spaced_line";"wide_margins"] 
    "You can also adjust the amount of space between wrapped lines; this line has extra space between each wrapped line in the same paragraph. To show off wrapping, some filler text: the quick brown fox jumped over the lazy dog. Blah blah blah blah blah blah blah blah blah.\n";
  buffer#insert ~iter
    "Also note that those lines have extra-wide margins.\n\n";
  buffer#insert ~iter  ~tag_names:["heading"] 
    "Editability";
  buffer#insert ~iter ~tag_names:["not_editable"] 
    "This line is 'locked down' and can't be edited by the user - just try it! You can't delete this line.\n\n";
   buffer#insert ~iter  ~tag_names:["heading"] 
    "Wrapping";
  buffer#insert ~iter ~tag_names:["char_wrap"] 
    "This line has character-based wrapping, and can wrap between any two character glyphs. Let's make this a long paragraph to demonstrate: blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah\n\n" ;
  buffer#insert ~iter ~tag_names:["no_wrap"] 
    "This line has all wrapping turned off, so it makes the horizontal scrollbar appear.\n\n\n";
   buffer#insert ~iter  ~tag_names:["heading"] 
    "Justification";
  buffer#insert ~iter ~tag_names:["center"] 
    "\nThis line has center justification.\n";
  buffer#insert ~iter ~tag_names:["right_justify"] 
    "\nThis line has right justification.\n";
  buffer#insert ~iter ~tag_names:["wide_margins"] 
    "\nThis line has big wide margins. Text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text.\n";
   buffer#insert ~iter  ~tag_names:["heading"] 
    "Internationalization";
   buffer#insert ~iter
    "You can put all sorts of Unicode text in the buffer.\n\nGerman (Deutsch Süd) Grüß Gott\nGreek (Ελληνικά) Γειά σας\nHebrew	שלום\nJapanese (日本語)\n\nThe widget properly handles bidirectional text, word wrapping, DOS/UNIX/Unicode paragraph separators, grapheme boundaries, and so on using the Pango internationalization framework.\n";  
   buffer#insert ~iter
    "Here's a word-wrapped quote in a right-to-left language:\n";
   buffer#insert ~iter ~tag_names:["rtl_quote"]
    "وقد بدأ ثلاث من أكثر المؤسسات تقدما في شبكة اكسيون برامجها كمنظمات لا تسعى للربح، ثم تحولت في السنوات الخمس الماضية إلى مؤسسات مالية منظمة، وباتت جزءا من النظام المالي في بلدانها، ولكنها تتخصص في خدمة قطاع المشروعات الصغيرة. وأحد أكثر هذه المؤسسات نجاحا هو »بانكوسول« في بوليفيا.\n\n";
   buffer#insert ~iter
       "You can put widgets in the buffer: Here's a button: ";
   buffer#create_child_anchor iter;
   buffer#insert ~iter
       " and a menu : ";
   buffer#create_child_anchor iter;
   buffer#insert ~iter
       " and a scale : ";
   buffer#create_child_anchor iter;
   buffer#insert ~iter
       " and an animation : ";
   buffer#create_child_anchor iter;
   buffer#insert ~iter
       " finally a text entry : ";
   buffer#create_child_anchor iter;
   buffer#insert ~iter
       ".\n";
   buffer#insert ~iter
       "\n\nThis demo doesn't demonstrate all the GtkTextBuffer features; it leaves out, for example: invisible/hidden text (doesn't work in GTK 2, but planned), tab stops, application-drawn areas on the sides of the widget for displaying breakpoints and such...";
   let start,stop = buffer#bounds in
   buffer#apply_tag_by_name "word_wrap" ~start ~stop ; 
   ()


let rec find_anchor (iter : GText.iter) =
  if iter#is_end then false else
  match iter#nocopy#forward_char ; iter#contents with
    `CHILD _ -> true
  | _ -> find_anchor iter

let rec recursive_attach_view depth (view:GText.view) anchor =
  if depth <= 4 then begin
    let child_view = GText.view ~buffer:(view#buffer) () in
    let event_box = GBin.event_box () in
    let color = `NAME "black" in
    event_box#misc#modify_bg [`NORMAL,color];
    let align = GBin.alignment () in
    align#set_border_width 1;
    event_box#add align#coerce;
    align#add child_view#coerce;
    view#add_child_at_anchor event_box#coerce anchor;
    recursive_attach_view (depth+1) child_view anchor
  end

    
let easter_egg_callback =
  let window = ref None in
  fun () -> match !window with Some w -> w#present ()
    | None -> 
	let buffer = GText.buffer () in
	let iter = buffer#start_iter in
	buffer#insert ~iter "This buffer is shared by a set of nested text views.\n Nested view:\n";
	let anchor = buffer#create_child_anchor iter in
	buffer#insert ~iter 
	  "\nDon't do this in real applications, please.\n";
	let view = GText.view ~buffer () in
	recursive_attach_view 0 view anchor;
	let w' = GWindow.window ~kind:`TOPLEVEL () in
	w'#connect#destroy ~callback:(fun () -> window:=None);
	window := Some w';
	let sw = GBin.scrolled_window () in
	sw#set_hpolicy `AUTOMATIC;
	sw#set_vpolicy `AUTOMATIC;
	w'#add sw#coerce;
	sw#add view#coerce;
	w'#set_default_size ~width:300 ~height:400;
	w'#misc#show_all ()

    
  


let attach_widgets (text_view:GText.view) =
  let buffer = text_view#buffer in
  let iter = buffer#start_iter in
  let i = ref 0 in
  while find_anchor iter do
    let anchor = match iter#contents with 
      | `CHILD c -> c 
      | _ -> assert false
    in
    let widget = match !i with 
      | 0 -> let b = GButton.button ~label:"Click me!" () in
	b#connect#clicked ~callback:easter_egg_callback;
	b#coerce
      | 1 -> let menu = GMenu.menu () in
	let widget = GMenu.option_menu () in
	let menu_item = GMenu.menu_item  ~label:"Option 1" () in
	menu#append menu_item;
	let menu_item = GMenu.menu_item  ~label:"Option 2" () in
	menu#append menu_item;
	let menu_item = GMenu.menu_item  ~label:"Option 3" () in
	menu#append menu_item;
	widget#set_menu menu;
	widget#coerce
      | 2 -> let widget = GRange.scale `HORIZONTAL () in
	widget#adjustment#set_bounds ~lower:0. ~upper:100. ();
	widget#misc#set_size_request ~height:(-1) ~width:70 ();
	widget#coerce
      | 3 -> let image = GMisc.image () in
	image#set_file "floppybuddy.gif";
	image#coerce
      | 4 -> (GEdit.entry ())#coerce
      | _ -> assert false
    in
    text_view#add_child_at_anchor widget anchor;
    incr i
  done

let main () =
  let window = GWindow.window ~width:450 ~height:450 ~title:"TextView"
      ~border_width:0 () in
  window#connect#destroy ~callback:(fun _ -> exit 0);
  let vpaned = GPack.paned `VERTICAL ~border_width:5 ~packing:window#add () in
  let view1 = GText.view () in
  let buffer = view1#buffer in
  let view2 = GText.view ~buffer () in
  let sw = GBin.scrolled_window ~packing:vpaned#add1
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
  sw#add view1#coerce;
  let sw = GBin.scrolled_window ~packing:vpaned#add2
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
  sw#add view2#coerce;
  create_tags buffer;
  insert_text buffer;  
  attach_widgets view1;
  attach_widgets view2;
  window#show ();
  ()

let _ = GtkMain.Main.init ();
  main () ;
  
  GMain.Main.main ();; 
