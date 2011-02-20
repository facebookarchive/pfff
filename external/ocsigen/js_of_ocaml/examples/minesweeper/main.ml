
module Html = Dom_html
let js = Js.string
let document = Html.window##document

let int_input name value =
  let res = document##createDocumentFragment() in
  Dom.appendChild res (document##createTextNode(js name));
  let input = Html.createInput ~_type:(js"text") document in
  input##value <- js (string_of_int !value);
  input##onchange <- Html.handler
    (fun _ ->
       begin try
         value := int_of_string (Js.to_string (input##value))
       with Invalid_argument _ ->
         ()
       end;
       input##value <- js (string_of_int !value);
       Js._false);
  Dom.appendChild res input;
  res

let button name callback =
  let res = document##createDocumentFragment() in
  let input = Html.createInput ~_type:(js"submit") document in
  input##value <- js name;
  input##onclick <- Html.handler callback;
  Dom.appendChild res input;
  res

let onload _ =
  let main =
    Js.Opt.get (document##getElementById(js"main"))
      (fun () -> assert false)
  in
  let nbr, nbc, nbm = ref 10, ref 12, ref 15 in
  Dom.appendChild main (int_input "Number of columns" nbr);
  Dom.appendChild main (Html.createBr document);
  Dom.appendChild main (int_input "Number of rows" nbc);
  Dom.appendChild main (Html.createBr document);
  Dom.appendChild main (int_input "Number of mines" nbm);
  Dom.appendChild main (Html.createBr document);
  Dom.appendChild main
    (button "nouvelle partie"
       (fun _ ->
          let div = Html.createDiv document in
          Dom.appendChild main div;
          Minesweeper.run div !nbc !nbr !nbm;
          Js._false));
  Js._false

let _ = Html.window##onload <- Html.handler onload
