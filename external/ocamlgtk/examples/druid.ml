(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

type color = 
  | RED
  | BLUE
  | YELLOW

type food =
  | DONUTS
  | YOGHURTS
  | PIZZA

class answer = object
  val mutable color = RED
  val mutable food = DONUTS
  method answer_color c () = color <- c
  method answer_food f () = food <- f
  method get_answer = "42"
end


let radio_color poll packing =
  let f = GBin.frame ~label:"Color" ~packing () in
  let vb = GPack.vbox ~packing:f#add () in
  let rb = GButton.radio_button ~label:"Red" ~packing:(vb#pack) () in
  rb#connect#clicked (poll#answer_color RED) ;
  let rb2 = GButton.radio_button ~group:rb#group ~label:"Blue" ~packing:(vb#pack) () in
  rb2#connect#clicked (poll#answer_color BLUE) ;
  let rb3 = GButton.radio_button ~group:rb#group ~label:"Yellow" ~packing:(vb#pack) () in
  rb3#connect#clicked (poll#answer_color YELLOW)

let radio_food poll =
  let vb = GPack.vbox () in
  let rb = GButton.radio_button ~label:"Donuts" ~packing:(vb#pack) () in
  rb#connect#clicked (poll#answer_food DONUTS) ;
  let rb2 = GButton.radio_button ~group:rb#group ~label:"Pizza" ~packing:(vb#pack) () in
  rb2#connect#clicked (poll#answer_food PIZZA) ;
  let rb3 = GButton.radio_button ~group:rb#group ~label:"Yoghurt" ~packing:(vb#pack) () in
  rb3#connect#clicked (poll#answer_food YOGHURTS) ;
  vb


let are_you_sure quit =
  let md = GWindow.message_dialog 
      ~message:"Are you sure ?"
      ~message_type:`QUESTION 
      ~buttons:GWindow.Buttons.yes_no
      ~modal:true () in
  let res = md#run () = `YES in
  md#destroy () ;
  if res then quit ()


let make_druid poll quit =
  let d = GnoDruid.druid () in

  d#connect#cancel (fun () -> are_you_sure quit) ;
  
  begin 
    let fp = GnoDruid.druid_page_edge ~position:`START ~aa:true ~title:"Poll !!" () in
    fp#set_text "Here is our great new poll.\nPlease answer all the questions !" ;
    d#append_page fp 
  end ;

  begin 
    let cp = GnoDruid.druid_page_standard ~title:"Color" () in
    radio_color poll cp#vbox#pack ;
    d#append_page cp 
  end ;

  begin 
    let mp = GnoDruid.druid_page_standard ~title:"Food" () in
    mp#append_item ~question:"Favorite food ?" ~additional_info:""
      (radio_food poll)#coerce ;
    d#append_page mp 
  end ;

  begin 
    let ep = GnoDruid.druid_page_edge ~position:`FINISH ~aa:true ~title:"The end" () in
    ep#set_text "Thank you for your co-operation." ; 
    d#append_page ep ;

    ep#connect#finish 
      (fun _ -> 
	let res = GWindow.message_dialog 
	    ~message:(Printf.sprintf "The answer is %s!" poll#get_answer)
	    ~message_type:`INFO ~buttons:GWindow.Buttons.close
	    ~modal:true () in
	res#run () ;
	res#destroy () ;
	quit ())
  end ;
  d

let window_and_druid () =
  let w = GWindow.window ~title:"Druid test" () in
  let poll = new answer in
  w#add (make_druid poll GMain.quit)#coerce ;
  w#event#connect#delete 
    (fun _ -> are_you_sure GMain.quit ; true) ;
  w

let _ = 
  let w = window_and_druid () in
  w#show () ;
  GMain.main ()
