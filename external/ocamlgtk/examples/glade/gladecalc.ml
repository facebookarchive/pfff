(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: gladecalc.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels

(* lablgladecc2 project2.glade > project2.ml *)
#use "project2.ml";;

let w1 = new window1 ()

let numbers =
  [| w1#button0; w1#button1; w1#button2; w1#button3; w1#button4;
     w1#button5; w1#button6; w1#button7; w1#button8; w1#button9 |]

let label = w1#label1

type state = Input | Result
let state = ref Result
let pending = ref (fun x -> x)

let insert_digit n =
  let prev = if !state = Result then "" else label#text in
  label#set_text (prev ^ string_of_int n);
  state := Input

let get_float () = float_of_string label#text

let insert_dot () =
  let prev = label#text in
  if not (String.contains prev '.') then label#set_text (prev ^ ".")

let set_pending f =
  pending := f (get_float ()); state := Result

let equals () =
  if !state = Input then
    label#set_text (string_of_float (!pending (get_float())));
  state := Result

let _ =
  for i = 0 to 9 do
    numbers.(i)#connect#clicked ~callback:(fun () -> insert_digit i)
  done;
  w1#button_dot#connect#clicked ~callback:insert_dot;
  List.iter ~f:
    begin fun (b, f) ->
      ignore (b#connect#clicked ~callback:(fun () -> set_pending f))
    end
    [ w1#button_add, (+.); w1#button_sub, (-.); w1#button_mul, ( *. );
      w1#button_div, (fun x y -> if y = 0. then 0. else x/.y) ];
  w1#button_eq#connect#clicked ~callback:equals;
  w1#window1#event#connect#key_press ~callback:
    begin fun ev ->
      let key = GdkEvent.Key.string ev in
      if String.length key <> 1 then false else begin
        begin match key.[0] with
        | '0'..'9' -> insert_digit (int_of_string key)
        | '.' -> insert_dot ()
        | '+' -> set_pending (+.)
        | '-' -> set_pending (-.)
        | '*' -> set_pending ( *. )
        | '/' -> set_pending (fun x y -> if y = 0. then 0. else x/.y)
        | '=' -> equals ()
        | 'q' -> GMain.Main.quit ()
        | _ -> ()
        end;
        true
      end
    end;
  GMain.Main.main ()
