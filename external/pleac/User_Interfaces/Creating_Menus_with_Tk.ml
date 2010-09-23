(* ********************************************************************** *)
(* Creating Menus with Tk *)
(* ********************************************************************** *)
let pleac_Creating_Menus_with_Tk () = 
  (* LablTk is included in the OCaml standard library. *)
  #directory "+labltk";;
  #load "labltk.cma";;
  
  open Tk
  
  let main = openTk ()
  
  (* Create a horizontal space at the top of the window for the
     menu to live in. *)
  let menubar = Frame.create ~relief:`Raised ~borderwidth:2 main
  let () = pack ~anchor:`Nw ~fill:`X [menubar]
  
  (* Create a button labeled "File" that brings up a menu *)
  let file_menubutton = Menubutton.create ~text:"File" ~underline:1 menubar
  let () = pack ~side:`Left [file_menubutton]
  
  (* Create entries in the "File" menu *)
  let file_menu = Menu.create file_menubutton
  let () = Menubutton.configure ~menu:file_menu file_menubutton
  let () = Menu.add_command ~label:"Print" ~command:print file_menu
  let () = Menu.add_command ~label:"Save" ~command:save file_menu
  
  (*-----------------------------*)
  
  (* Create a menu item using an anonymous callback *)
  let () =
    Menu.add_command
      ~label:"Quit Immediately"
      ~command:(fun () -> exit 0)
      file_menu
  
  (*-----------------------------*)
  
  (* Add a separator (a horizontal line) to the menu *)
  let () = Menu.add_separator file_menu
  
  (*-----------------------------*)
  
  (* Create a checkbutton menu item *)
  let debug = Textvariable.create ~on:options_menu ()
  let () =
    Menu.add_checkbutton
      ~label:"Create Debugging File"
      ~variable:debug
      ~onvalue:"1"
      ~offvalue:"0"
      options_menu
  
  (*-----------------------------*)
  
  (* Create radiobutton menu items *)
  let log_level = Textvariable.create ~on:options_menu ()
  let () =
    Menu.add_radiobutton
      ~label:"Level 1"
      ~variable:log_level
      ~value:"1"
      debug_menu
  let () =
    Menu.add_radiobutton
      ~label:"Level 2"
      ~variable:log_level
      ~value:"2"
      debug_menu
  let () =
    Menu.add_radiobutton
      ~label:"Level 3"
      ~variable:log_level
      ~value:"3"
      debug_menu
  
  (*-----------------------------*)
  
  (* Create a nested menu *)
  let font_menu = Menu.create format_menubutton
  let () = Menu.add_cascade ~label:"Font" ~menu:font_menu format_menu
  let font_name = Textvariable.create ~on:font_menu ()
  let () =
    Menu.add_radiobutton
      ~label:"Courier"
      ~variable:font_name
      ~value:"courier"
      font_menu
  let () =
    Menu.add_radiobutton
      ~label:"Times Roman"
      ~variable:font_name
      ~value:"times"
      font_menu
  
  (*-----------------------------*)
  
  (* To disable tearoffs, use ~tearoff:false when calling Menu.create *)
  let font_menu = Menu.create ~tearoff:false format_menubutton
  
  (*-----------------------------*)
  
  (* Start the Tk event loop and display the interface *)
  let () = Printexc.print mainLoop ()
  

