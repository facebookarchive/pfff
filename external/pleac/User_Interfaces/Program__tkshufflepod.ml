(* ********************************************************************** *)
(* Program: tkshufflepod *)
(* ********************************************************************** *)
let pleac_Program__tkshufflepod () = 
  #!/usr/bin/ocaml
  (* tkshufflepod - reorder =head1 sections in a pod file *)
  #directory "+labltk";;
  #load "labltk.cma";;
  
  open Tk
  
  (* Custom text viewer widget. *)
  
  class viewer parent =
    let toplevel = Toplevel.create parent in
    let frame = Frame.create toplevel in
    let text = Text.create ~width:80 ~height:30 ~state:`Disabled frame in
    let vscroll = Scrollbar.create ~orient:`Vertical frame in
  
  object (self)
    initializer
      self#hide ();
      Text.configure ~yscrollcommand:(Scrollbar.set vscroll) text;
      Scrollbar.configure ~command:(Text.yview text) vscroll;
      pack ~side:`Right ~fill:`Y [vscroll];
      pack ~side:`Left ~fill:`Both ~expand:true [text];
      pack ~side:`Right ~fill:`Both ~expand:true [frame];
      Wm.protocol_set toplevel "WM_DELETE_WINDOW" self#hide
  
    method show () = Wm.deiconify toplevel; raise_window toplevel
    method hide () = Wm.withdraw toplevel
  
    method set_title = Wm.title_set toplevel
    method set_body body =
      Text.configure ~state:`Normal text;
      Text.delete ~start:(`Atxy (0, 0), []) ~stop:(`End, []) text;
      Text.insert ~index:(`End, []) ~text:body text;
      Text.configure ~state:`Disabled text
  end
  
  (* Give list references a similar interface to Tk
     listbox widgets so we can keep the two in sync. *)
  
  let listref_get listref index =
    match index with
      | `Num i -> List.nth !listref i
      | _ -> failwith "listref_get"
  
  let listref_delete listref index =
    match index with
      | `Num i ->
          let rec loop current list =
            match list with
              | head :: tail when current = i -> loop (current + 1) tail
              | head :: tail -> head :: loop (current + 1) tail
              | [] -> [] in
          listref := loop 0 !listref
      | _ -> failwith "listref_delete"
  
  let listref_insert listref index elt =
    match index with
      | `Num i ->
          let rec loop current list =
            match list with
              | head :: tail when current = i ->
                  elt :: head :: loop (current + 1) tail
              | head :: [] when current = i - 1 -> head :: [elt]
              | head :: tail -> head :: loop (current + 1) tail
              | [] -> [] in
          listref := loop 0 !listref
      | _ -> failwith "listref_insert"
  
  (* Use a line stream to produce a stream of POD chunks. *)
  
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
  let pod_stream_of_channel channel =
    let lines = line_stream_of_channel channel in
    let is_head s = String.length s >= 6 && String.sub s 0 6 = "=head1" in
    let rec next pod_head pod_lines i =
      match Stream.peek lines, pod_head, pod_lines with
        | None, "", _ ->
            (* EOF, no POD found, return EOF *)
            None
        | None, _, _ ->
            (* EOF, POD found, return POD *)
            Some (pod_head, List.rev pod_lines)
        | Some head, "", _ when is_head head ->
            (* Head found *)
            Stream.junk lines;
            next head [] i
        | _, "", _ ->
            (* No head found, keep looking *)
            Stream.junk lines;
            next "" [] i
        | Some head, _, _ when is_head head ->
            (* Next head found, return POD *)
            Some (pod_head, List.rev pod_lines)
        | Some line, _, _ ->
            (* Line found, buffer and continue reading *)
            Stream.junk lines;
            next pod_head (line :: pod_lines) i in
    Stream.from (next "" [])
  
  (* Read the POD file into memory, and split it into sections. *)
  
  let podfile =
    if Array.length Sys.argv < 2
    then "-"
    else Sys.argv.(1)
  
  let sections = ref []
  
  (* Turn !sections into a list of (text, head) pairs. *)
  
  let () =
    let channel = if podfile = "-" then stdin else open_in podfile in
    Stream.iter
      (fun (head, lines) ->
         sections := (String.concat "\n" lines, head) :: !sections)
      (pod_stream_of_channel channel);
    sections := List.rev !sections;
    close_in channel
  
  (* Fire up Tk and display the list of sections. *)
  let main = openTk ()
  let listbox = Listbox.create ~width:60 main
  let dragging = ref None
  
  (* Singleton viewer instance. *)
  let viewer = new viewer main
  
  (* Called when the user clicks on an item in the Listbox. *)
  let down event =
    dragging := Some (Listbox.nearest listbox event.ev_MouseY)
  
  (* Called when the user releases the mouse button in the Listbox. *)
  let up event =
    dragging := None
  
  (* Called when the user moves the mouse in the Listbox. *)
  let move event =
    let dest = Listbox.nearest listbox event.ev_MouseY in
    match !dragging with
      | Some src when src <> dest ->
          let elt = listref_get sections src in
          listref_delete sections src;
          listref_insert sections dest elt;
          let elt = Listbox.get listbox src in
          Listbox.delete listbox ~first:src ~last:src;
          Listbox.insert listbox ~index:dest ~texts:[elt];
          dragging := Some dest
      | _ -> ()
  
  (* Called to save the list of sections. *)
  let save event =
    let channel = if podfile = "-" then stdout else open_out podfile in
    List.iter
      (fun (text, head) ->
         output_string channel head;
         output_string channel "\n";
         output_string channel text;
         output_string channel "\n";
         flush channel)
      !sections;
    if podfile <> "-" then close_out channel
  
  (* Called to display the widget.  Uses the viewer widget. *)
  let view event =
    dragging := None; (* cancel drag *)
    List.iter
      (fun (`Num i) ->
         let (text, head) = List.nth !sections i in
         viewer#set_title head;
         viewer#set_body (head ^ "\n" ^ text);
         viewer#show ())
      (Listbox.curselection listbox)
  
  let () =
    pack ~expand:true ~fill:`Both [listbox];
  
    List.iter
      (fun (text, title) -> Listbox.insert listbox `End [title])
      !sections;
  
    (* Permit dragging by binding to the Listbox widget. *)
    bind ~events:[`ButtonPress] ~fields:[`MouseY] ~action:down listbox;
    bind ~events:[`ButtonRelease] ~action:up listbox;
    bind ~events:[`Motion] ~fields:[`MouseY] ~action:move listbox;
  
    (* Permit viewing by binding double-click. *)
    bind ~events:[`Modified ([`Double], `ButtonRelease)] ~action:view listbox;
  
    (* 'q' quits and 's' saves *)
    bind ~events:[`KeyPressDetail "s"] ~action:save main;
    bind ~events:[`KeyPressDetail "q"] ~action:(fun _ -> exit 0) main;
  
    Printexc.print mainLoop ()
  
  

