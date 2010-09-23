(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: tron.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

(* Tron? Game *)
open GMain

let m_pi = acos (-1.)
let clRed   = `NAME "red"  (* `BLACK *)
let clBlue  = `NAME "blue" (* `WHITE *)
let clBlack = `BLACK

type point = {mutable x: int; mutable y: int}

let main () =
(* Game State *)
  let gameSize = 64 in
  let gameState =
    Array.create_matrix (gameSize+2) (gameSize+2) 0 in
  let gameInit _ = 
    for i=1 to gameSize do
      for j=1 to gameSize do
        gameState.(i).(j) <- 0;
      done
    done;
    for i=0 to gameSize do
      gameState.(0).(i) <- 3;            (* left wall *)
      gameState.(i).(gameSize+1) <- 3;   (* floor *) 
      gameState.(gameSize+1).(i+1) <- 3; (* right wall *)
      gameState.(i+1).(0) <- 3           (* ceiling *)
    done in
  gameInit ();
  let lpos = {x=4; y=4} in
  let lspeed = {x=0; y=1} in
  let rpos = {x=gameSize-3; y=gameSize-3} in
  let rspeed = {x=0; y= -1} in
  let keys = "asdfhjkl" in
  let keyMapL = [|(-1, 0); (0, -1); (0, 1); (1, 0)|] in
  let keyMapR = [|(-1, 0); (0, 1); (0, -1); (1, 0)|] in

(* User Interface *)
  let window = GWindow.window ~border_width:10 ~title:"tron(?)" () in
  window#event#connect#delete
     ~callback:(fun _ -> prerr_endline "Delete event occured"; false);
  window#connect#destroy ~callback:Main.quit;
  let vbx = GPack.vbox ~packing:window#add () in
  let area = GMisc.drawing_area ~width:((gameSize+2)*4) ~height:((gameSize+2)*4)
      ~packing:vbx#add () in
  let drawing = area#misc#realize (); new GDraw.drawable (area#misc#window) in
  let style = area#misc#style#copy in
  style#set_bg [`NORMAL,`WHITE];
  area#misc#set_style style;
  drawing#set_background `WHITE;
  let area_expose _ =
    for i=0 to gameSize+1 do
      for j=0 to gameSize+1 do
        if gameState.(i).(j) = 1 then begin
          drawing#set_foreground clRed;
          drawing#rectangle ~filled:true ~x:(i*4) ~y:(j*4) ~width:4 ~height:4 ()
        end
        else if gameState.(i).(j) = 2 then begin
          drawing#set_foreground clBlue;
          drawing#rectangle ~filled:true ~x:(i*4) ~y:(j*4) ~width:4 ~height:4 ()
        end
        else if gameState.(i).(j) = 3 then begin
          drawing#set_foreground clBlack;
          drawing#rectangle ~filled:true ~x:(i*4) ~y:(j*4) ~width:4 ~height:4 ()
        end 
      done
    done;
    false
  in
  area#event#connect#expose ~callback:area_expose;
  let control = GPack.table ~rows:3 ~columns:7 ~packing:vbx#pack () in

  let abuttonClicked num (lbl : GMisc.label) _ = begin
    let dialog = GWindow.window ~border_width:10 ~title:"Key remap" () in
    let dvbx = GPack.box `VERTICAL ~packing:dialog#add () in
    let entry  = GEdit.entry ~max_length:1 ~packing: dvbx#add () in
    let txt = String.make 1 keys.[num] in
    entry#set_text txt;
    let dquit = GButton.button ~label:"OK" ~packing: dvbx#add () in 
    dquit#connect#clicked ~callback:
      begin fun _ ->
	let chr = entry#text.[0] in
        let txt2 = String.make 1 chr in
        lbl#set_text txt2;
        keys.[num]<-chr; 
        dialog#destroy ()
      end;
    dialog#show ()
  end in
  let attach = control#attach ~expand:`BOTH in
  let new_my_button ~label:label ~left:left ~top:top =
      let str = String.make 1 keys.[label] in
      let btn = GButton.button ~packing:(attach ~left:left ~top:top) () in
      let lbl = GMisc.label ~text:str ~packing:(btn#add) () in
      btn#connect#clicked ~callback:(abuttonClicked label lbl);
      btn
  in
  new_my_button ~label:0 ~left:1 ~top:2;
  new_my_button ~label:1 ~left:2 ~top:1;
  new_my_button ~label:2 ~left:2 ~top:3;
  new_my_button ~label:3 ~left:3 ~top:2;
  new_my_button ~label:4 ~left:5 ~top:2;
  new_my_button ~label:5 ~left:6 ~top:3;
  new_my_button ~label:6 ~left:6 ~top:1;
  new_my_button ~label:7 ~left:7 ~top:2;
  let quit =
    GButton.button ~label:"Quit" ~packing:(attach ~left:4 ~top:2) () in
  quit#connect#clicked ~callback:window#destroy;
  let message = GMisc.label ~text:"tron(?) game" ~packing:vbx#pack () in

  let game_step () =
        let lx = lpos.x in let ly = lpos.y in
        gameState.(lx).(ly) <- 1;
        drawing#set_foreground clRed;
        drawing#rectangle ~filled:true ~x:(lx*4) ~y:(ly*4) ~width:4 ~height:4 ();
        let rx = rpos.x in let ry = rpos.y in
        gameState.(rx).(ry) <- 2;
        drawing#set_foreground clBlue;
        drawing#rectangle ~filled:true ~x:(rx*4) ~y:(ry*4) ~width:4 ~height:4 ()
  in
  game_step ();
  let keyDown ev = begin
    let key = GdkEvent.Key.keyval ev in
    for i=0 to (Array.length keyMapL)-1 do
       let (x, y) = keyMapL.(i) in
       let k = keys.[i] in
       if key = Char.code k then begin
         lspeed.x <- x;
         lspeed.y <- y 
       end;
       let (x, y) = keyMapR.(i) in
       let k = keys.[i+4] in
       if key = Char.code k then begin
         rspeed.x <- x;
         rspeed.y <- y 
       end
    done;       
    false end in
  window#event#connect#key_press ~callback:keyDown;
  let safe_check _ = 
    if lpos.x == rpos.x && lpos.y == rpos.y then
      3
    else
      (* player 1 *)
      (if gameState.(lpos.x).(lpos.y) != 0  then 2 else 0)
      +
      (* player 2 *)
      (if gameState.(rpos.x).(rpos.y) != 0  then 1 else 0)
      in
  let timerID = ref (* dummy *) (Timeout.add ~ms:100 ~callback:(fun _ -> true)) in
  let timerTimer _ = begin
     lpos.x <- lpos.x+lspeed.x;
     lpos.y <- lpos.y+lspeed.y;
     rpos.x <- rpos.x+rspeed.x;
     rpos.y <- rpos.y+rspeed.y;
     let result = safe_check() in
     if result!=0 then begin
        Timeout.remove (!timerID);
        message#set_text ("player "^string_of_int result^" won.")
     end
     else begin
       game_step()
     end;
     true
  end in
  let count = ref 3 in
  let timerTimer2 _ = begin
(*    message#set_label (string_of_int (!count)); *)
    if (!count==0) then begin
      Timeout.remove (!timerID);
      timerID := Timeout.add ~ms:100 ~callback:timerTimer
    end
    else begin
      count := !count-1;
    end;
    true
  end in
  let restartClicked () =
    Timeout.remove !timerID;
    gameInit();
    lpos.x <- 4; lpos.y <- 4;
    lspeed.x <- 0; lspeed.y <- 1;
    rpos.x <- gameSize-3; rpos.y <- gameSize-3;
    rspeed.x <- 0; rspeed.y <- -1;
    drawing#set_foreground `WHITE;
    drawing#rectangle ~filled:true ~x:0 ~y:0
      ~width:((gameSize+2)*4) ~height:((gameSize+2)*4) ();
    area_expose();
    count := 3;
    timerID := Timeout.add ~ms:300 ~callback:timerTimer2;
  in
  let restart =
    GButton.button ~label: "Restart" ~packing:(attach ~left:4 ~top:3) () in
  restart#connect#clicked ~callback:restartClicked;
  restartClicked ();

  window#show ();
  Main.main ()

let _ = Printexc.print main ()


