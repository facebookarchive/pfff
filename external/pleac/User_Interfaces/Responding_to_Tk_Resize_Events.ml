(* ********************************************************************** *)
(* Responding to Tk Resize Events *)
(* ********************************************************************** *)
let pleac_Responding_to_Tk_Resize_Events () = 
  open Tk
  
  let main = openTk ()
  
  (* Prevent the user from resizing the window. *)
  let () =
    bind main
      ~events:[`Configure]
      ~action:(fun _ ->
                 let width = Winfo.width main in
                 let height = Winfo.height main in
                 Wm.minsize_set main width height;
                 Wm.maxsize_set main width height)
  
  (* Or, use pack to control how widgets are resized. *)
  let () = pack ~fill:`Both ~expand:true [widget]
  let () = pack ~fill:`X ~expand:true [widget]
  
  (* Make the main area expand horizontally and vertically. *)
  let () = pack ~fill:`Both ~expand:true [mainarea]
  
  (* Make the menu bar only expand horizontally. *)
  let () = pack ~fill:`X ~expand:true [menubar]
  
  (* Anchor the menu bar to the top-left corner. *)
  let () = pack ~fill:`X ~expand:true ~anchor:`Nw [menubar]
  

