(* ********************************************************************** *)
(* Creating Dialog Boxes with Tk *)
(* ********************************************************************** *)
let pleac_Creating_Dialog_Boxes_with_Tk () = 
  (* Tk::DialogBox is a CPAN module that replaces Tk's standard Dialog
     widget with one that can be customized with additional inputs. To
     get this effect in OCaml would require translating the whole CPAN
     module; instead, for this simple example, we will use the built-in
     Dialog. *)
  
  #directory "+labltk";;
  #load "labltk.cma";;
  
  open Tk
  
  let main = openTk ()
  
  let dialog =
    Dialog.create
      ~title:"Register This Program"
      ~buttons:["Register"; "Cancel"]
      ~parent:main
      ~message:"..."
  
  let () =
    match dialog () with
      | 0 -> print_endline "Register"
      | 1 -> print_endline "Cancel"
      | _ -> failwith "this shouldn't happen"
  
  let () = Printexc.print mainLoop ()
  
  (*-----------------------------*)
  
  (* Normally, uncaught exceptions are printed to standard error. However,
     by overriding the "camlcb" callback, a custom error handler can be
     installed which creates dialogs instead. *)
  
  #directory "+labltk";;
  #load "labltk.cma";;
  
  open Tk
  
  let main = openTk ()
  
  let show_error =
    let dialog =
      Dialog.create
        ~title:"Error"
        ~buttons:["Acknowledge"]
        ~parent:main in
    fun message -> ignore (dialog ~message ())
  
  (* Override the "camlcb" callback. Note that this is an undocumented
     feature that relies on some internals of Labltk. *)
  let () =
    Callback.register "camlcb"
      (fun id args ->
         try (Hashtbl.find Protocol.callback_naming_table id) args
         with e -> show_error (Printexc.to_string e))
  
  let make_error () = failwith "This is an error"
  
  let button1 =
    Button.create ~text:"Make An Error" ~command:make_error main
  let () = pack ~side:`Left [button1]
  
  let button2 =
    Button.create ~text:"Quit" ~command:(fun () -> exit 0) main
  let () = pack ~side:`Left [button2]
  
  let () = Printexc.print mainLoop ()
  

