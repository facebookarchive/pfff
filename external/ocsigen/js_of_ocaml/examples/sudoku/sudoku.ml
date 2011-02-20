(* Copyright (C) 2009 Jake Donham *)

module D = Dom_html
let d = D.document

let make_board () =
  let make_input () =
    let input = D.createInput ~_type:(Js.string "text") d in
    input##size <- 1;
    input##maxLength <- 1;
    let style = input##style in
    style##border <- Js.string "none";
    style##padding <- Js.string "0px";
    let enforce_digit _ =
      match Js.to_string input##value with
        | "1" | "2" | "3" | "4" | "5"
        | "6" | "7" | "8" | "9" -> Js._false
        | _ -> input##value <- Js.string ""; Js._false in
    input##onchange <- D.handler enforce_digit;
    input in

  let make_td i j input =
    let td = D.createTd d in
    let style = td##style in
    style##borderStyle <- Js.string "solid";
    style##borderColor <- Js.string "#000000";
    let widths = function
      | 0 -> 2, 0 | 2 -> 1, 1 | 3 -> 1, 0
      | 5 -> 1, 1 | 6 -> 1, 0 | 8 -> 1, 2
      | _ -> 1, 0 in
    let (top, bottom) = widths i in
    let (left, right) = widths j in
    let px k = Js.string (string_of_int k ^ "px") in
    style##borderTopWidth <- px top;
    style##borderBottomWidth <- px bottom;
    style##borderLeftWidth <- px left;
    style##borderRightWidth <- px right;
    Dom.appendChild td input;
    td in

  let rows =
    Array.init 9 (fun i ->
      Array.init 9 (fun j ->
        make_input ())) in

  let table = D.createTable d in
  table##cellPadding <- Js.string "0px";
  table##cellSpacing <- Js.string "0px";
  let tbody = D.createTbody d in
  Dom.appendChild table tbody;
  ArrayLabels.iteri rows ~f:(fun i row ->
    let tr = D.createTr d in
    ArrayLabels.iteri row ~f:(fun j cell ->
      let td = make_td i j cell in
      Dom.appendChild tr td);
    Dom.appendChild tbody tr);

  (rows, table)

let check_board rows _ =
  let error i j =
    let cell = rows.(i).(j) in
    cell##style##backgroundColor <- Js.string "#ff0000" in

  let check_set set =
    let seen = Array.make 9 None in
    ArrayLabels.iter set ~f:(fun (i,j) ->
      let cell = rows.(i).(j) in
      match Js.to_string cell##value with
        | "" -> ()
        | v ->
            let n = int_of_string v in
            match seen.(n - 1) with
              | None ->
                  seen.(n - 1) <- Some (i,j)
              | Some (i',j') ->
                  error i j;
                  error i' j') in

  let check_row i =
    check_set (Array.init 9 (fun j -> (i,j))) in

  let check_column j =
    check_set (Array.init 9 (fun i -> (i,j))) in

  let check_square i j =
    let set = Array.init 9 (fun k ->
      i * 3 + k mod 3, j * 3 + k / 3) in
    check_set set in

  ArrayLabels.iter rows ~f:(fun row ->
    ArrayLabels.iter row ~f:(fun cell ->
      cell##style##backgroundColor <- Js.string "#ffffff"));

  for i = 0 to 8 do check_row i done;
  for j = 0 to 8 do check_column j done;
  for i = 0 to 2 do
    for j = 0 to 2 do
      check_square i j
    done
  done;
  Js._false

let onload _ =
  let (rows, table) = make_board () in
  let check =
    Js.Opt.get (d##getElementById (Js.string "check"))
      (fun () -> assert false) in
  check##onclick <- D.handler (check_board rows);
  let board =
    Js.Opt.get (d##getElementById (Js.string "board"))
      (fun () -> assert false) in
  ignore (Dom.appendChild board table);
  Js._false

let _ = D.window##onload <- D.handler onload
