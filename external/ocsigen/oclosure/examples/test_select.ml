open Goog
module D = Dom_html
let d = D.document

let dom = jsnew Gdom.domHelper()

let get_el s = Js.Opt.get (d##getElementById (Js.string s)) (fun _ -> assert false)

let add_item (select : Ui.select Js.t) name = 
  let item = jsnew Ui.menuItem(Ui.ControlContent.string (Js.string name),Js.null,Js.null) in
  select##addItem_(Tools.Union.i1 item)

let get_option ?(model = Js.null) name = 
  jsnew Ui.option(Ui.ControlContent.string (Js.string name),model,Js.null)

let add_option (select : Ui.select Js.t) option = 
  select##addItem_(Tools.Union.i1 option)

let listener select id sentence = 
  Events.listen
    (Tools.Union.i1 select)
    Ui.Component.EventType._ACTION
    (Js.wrap_callback (fun () -> 
       let value = Js.string (sentence ^ (Js.to_string (Js.Optdef.get select##getValue() (fun _ -> assert false))) ^ "!") in
	 dom##setTextContent(get_el id,value)  
    ))
    Js.null 

let _ =
  let select1 = jsnew Ui.select(Js.null,Js.null,Js.null,Js.null) in
    add_item select1 "Blade Runner";
    add_item select1 "Godfather Part II";
    add_item select1 "Citizen Kane";
    select1##setSelectedIndex(0);
    select1##render(Js.some (get_el "select1"));

  let select2 = jsnew Ui.select(Js.null,Js.null,Js.null,Js.null) 
  and disableItem = get_option "Howard the Duck" in 
    add_option select2 (get_option "Transformers");
    add_option select2 (get_option "Spider-man 3");
    add_option select2 disableItem;
    disableItem##setEnabled(Js._false);
    add_option select2 (get_option "Catwoman");
    select2##setSelectedIndex(1);
    select2##render(Js.some (get_el "select2"));
    
  let select3 = jsnew Ui.select(Js.some (Ui.ControlContent.string (Js.string "Click to select")),Js.null,Js.null,Js.null) in
    select3##setAutoStates(Ui.Component.State.state_pre_of_state Ui.Component.State.HOVER, Js._false);
    let t = Js.some Js._true and f = Js.some Js._false in
      add_option select3 (get_option ~model:t "enabled");
      add_option select3 (get_option ~model:f "disabled" );
      select3##render(Js.some (get_el "select3"));
      select3##setSelectedIndex(0);
      
   ignore(listener select1 "value1" "Yeah ");
   ignore(listener select2 "value2" "Boo ");
   Events.listen
     (Tools.Union.i1 select3)
     Ui.Component.EventType._ACTION
     (Js.wrap_callback (fun () -> 
	let res = Js.to_string (Js.Optdef.get select3##getValue() (fun _ -> assert false)) in
	  if res = "true" then
	    select2##setEnabled(Js._true)
	  else 
	    select2##setEnabled(Js._false)
     ))
     Js.null 
