(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: $ *)

(* The tutorial is translated to OCaml from Chapter 5 of Foundations of 
   GTK+ Development (published April 2007). You can find more information
   about the book at http://www.gtkbook.com. *)
(* See also 
   {http://www.linuxquestions.org/linux/articles/Technical/New_GTK_Widgets_GtkAssistant} *)

(* If there is text in the GtkEntry, set the page as complete. Otherwise,
  stop the user from progressing the next page. *)
let entry_changed assistant entry () = 
  let text = entry#text in
  let num = assistant#current_page in
  let page = assistant#nth_page num in
  assistant#set_page_complete page (String.length (text) > 0)

(* If the check button is toggled, set the page as complete. Otherwise,
   stop the user from progressing the next page. *)
let button_toggled toggle assistant () = 
  let active = toggle#active in
  assistant#set_page_complete toggle#as_widget active


(* Fill up the progress bar, 10% every second when the button is clicked. Then,
  set the page as complete when the progress bar is filled. *)
let button_clicked button assistant progress () = 
  let percent = ref 0.0 in
  button#misc#set_sensitive false;
  while (!percent <= 100.0) do
    let message = Printf.sprintf "%.0f%% Complete" !percent in
    progress#set_fraction (!percent /. 100.0);
    progress#set_text message;
    
    while Glib.Main.pending () do
      Glib.Main.iteration true
    done;
    
    Glib.usleep 500000;
    percent := !percent +. 5.0;
  done;
  let page = assistant#nth_page 3 in
  assistant#set_page_complete page true



(* If the dialog is cancelled, delete it from memory and then clean up after
   the Assistant structure. *)
let assistant_cancel assistant () = 
  assistant#destroy ()
    
(* This function is where you would apply the changes and destroy 
   the assistant. *)
let assistant_close assistant () = 
  prerr_endline "You would apply your changes now!";
  assistant#destroy ()



let main () =
  let assistant = GAssistant.assistant () in
  assistant#misc#set_size_request ~width:450 ~height:300 ();
  assistant#set_title "GtkAssistant Example";
  assistant#connect#destroy (fun () -> exit 0);
  let page_0 = GMisc.label ~text:"This is an example of a GtkAssistant. By
clicking the forward button, you can continue
to the next section!"
    ()
  in
  let page_1 = GPack.hbox ~homogeneous:false ~spacing:5 () in
  let page_2 = GButton.check_button ~label:"Click Me To Continue!" () in
  let page_3 = 
    GBin.alignment ~xalign:0.5 ~yalign:0.5 ~xscale:0.0 ~yscale:0.0 ()
  in
  let page_4 =  GMisc.label ~text:"Text has been entered in the label and the
combo box is clicked. If you are done, then
it is time to leave!" () 
  in
  
  (* Create the necessary widgets for the second page. *)
  let _label = GMisc.label  
    ~text:"Your Name: " 
    ~packing:(page_1#pack ~expand:false ~fill:false ~padding:5) 
    () 
  in
  let entry = GEdit.entry 
    ~packing:(page_1#pack ~expand:false ~fill:false ~padding:5) 
    ()
  in

(* Create the necessary widgets for the fourth page. 
   Then Attach the progress bar to the GtkAlignment widget for later access.*)
  let button = GButton.button ~label:"Click me!" () in
  let progress = GRange.progress_bar () in
  let hbox = GPack.hbox ~homogeneous:false ~spacing:5 () in
  hbox#pack  ~expand:true ~fill:false ~padding:5 progress#coerce;
  hbox#pack  ~expand:false ~fill:false ~padding:5 button#coerce;
  page_3#add hbox#coerce;

  (* Add five pages to the GtkAssistant dialog. *)
  assistant#append_page 
    ~title:"Introduction" 
    ~page_type:`INTRO 
    ~complete:true
    page_0#as_widget;
  assistant#append_page 
    ~page_type:`CONTENT 
    page_1#as_widget;
  assistant#append_page 
    ~title:"Click the Check Button"
    ~page_type:`CONTENT 
    page_2#as_widget;
  assistant#append_page 
     ~title:"Click the Button"
    ~page_type:`PROGRESS 
    page_3#as_widget;
  assistant#append_page 
     ~title:"Confirmation"
    ~page_type:`CONFIRM
    ~complete:true
    page_4#as_widget;

 (* Update whether pages 2 through 4 are complete based upon whether there is
    text in the GtkEntry, the check button is active, or the progress bar
    is completely filled. *)
  entry#connect#changed ~callback:(entry_changed assistant entry);
  page_2#connect#toggled ~callback:(button_toggled page_2 assistant);
  button#connect#clicked ~callback:(button_clicked button assistant progress);
  assistant#connect#cancel ~callback:(assistant_cancel assistant);
  assistant#connect#close ~callback:(assistant_close assistant);

  assistant#show ();
  GMain.Main.main ()


let () = 
  main ()
    
