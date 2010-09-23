(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: csview.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

(* Compile with
     ocamlc -pp camlp4o -I +lablgtk2 lablgtk.cma csview.ml -o csview
   or run with
     lablgtk2 camlp4o.cma csview.ml <file.csv>
*)

open StdLabels

(* A simple CSV data viewer *)

type data =
    { fields : string list;
      titles : string list;
      data : string list list }

let mem_string ~char s =
  try
    for i = 0 to String.length s - 1 do
      if s.[i] = char then raise Exit
    done;
    false
  with Exit -> true

let rec until ~chars ?(escapes="") ?(buf = Buffer.create 80) s =
  match Stream.peek s with
    Some c ->
      if mem_string ~char:c escapes then begin
        Stream.junk s;
        Buffer.add_char buf (Stream.next s);
        until ~chars ~escapes ~buf s
      end else if mem_string ~char:c chars then
        Glib.Convert.locale_to_utf8 (Buffer.contents buf)
      else begin
        Buffer.add_char buf c;
        Stream.junk s;
        until ~chars ~escapes ~buf s
      end
  | None ->
      if Buffer.length buf > 0 then raise (Stream.Error "until")
      else raise Stream.Failure

let rec ignores ?(chars = " \t") s =
  match Stream.peek s with
    Some c when mem_string ~char:c chars ->
      Stream.junk s; ignores ~chars s
  | _ -> ()

let parse_field = parser
    [< ''"'; f = until ~chars:"\"" ~escapes:"\\"; ''"'; _ = ignores >] ->
      for i = 0 to String.length f - 1 do
        if f.[i] = '\031' then f.[i] <- '\n'
      done;
      f
  | [< f = until ~chars:",\n\r" >] -> f
  | [< >] -> ""

let comma = parser [< '','; _ = ignores >] -> ()

let rec parse_list ~item ~sep = parser
    [< i = item; s >] ->
      begin match s with parser
        [< _ = sep; l = parse_list ~item ~sep >] -> i :: l
      | [< >] -> [i]
      end
  | [< >] -> []

let parse_one = parse_list ~item:parse_field ~sep:comma

let lf = parser [< ''\n'|'\r'; _ = ignores ~chars:"\n\r"; _ = ignores >] -> ()

let parse_all = parse_list ~item:parse_one ~sep:lf

let read_file ic =
  let s = Stream.of_channel ic in
  let data = parse_all s in
  match data with
    ("i"::fields) :: ("T"::titles) :: data ->
      {fields=fields; titles=titles; data=List.map ~f:List.tl data}
  | titles :: data ->
      {fields=titles; titles=titles; data=data}
  | _ -> failwith "Insufficient data"

let print_string s =
  Format.print_char '"';
  for i = 0 to String.length s - 1 do
    match s.[i] with
      '\'' -> Format.print_char '\''
    | '"' -> Format.print_string "\\\""
    | '\160'..'\255' as c -> Format.print_char c
    | c -> Format.print_string (Char.escaped c)
  done;
  Format.print_char '"'  

(*
#install_printer print_string;;
*)

open GMain

let field_widths =
  [ "i", 0;
    "ATTR", 0;
    "NAME", 17;
    "NAPR", 8;
    "TEL1", 14;
    "ZIPC", 12;
    "ADR1", 40;
    "BRTH", 10;
    "RMRK", 20;
    "CHK1", 0;
    "CHK2", 0;
    "CHK3", 0;
    "CHK4", 0;
    "TIM1", 16;
    "TIM2", 16;
    "ALRM", 0;
    "ATTM", 0;
  ]

let rec genlist ~start ~stop =
  if start >= stop then [] else (start,-1) :: genlist ~start:(start+1) ~stop

let rec star p = parser
    [< l = plus p >] -> l
  | [< >] -> []
and plus p = parser
    [< e = p; l = star p >] -> e :: l

let parse_int s =
  let l =
    plus (parser [< ''0'..'9' as n >] -> Char.code n - Char.code '0') s in
  List.fold_left l ~init:0 ~f:(fun acc n -> acc * 10 + n)

let parse_range ~start = parser
  | [< ''-'; stop = parse_int >] ->
      genlist ~start ~stop
  | [< '':'; width = parse_int >] ->
      [start,width]
  | [< >] ->
      [start,-1]

let rec parse_fields = parser
    [< n = parse_int; s >] ->
      let l = parse_range ~start:(n-1) s in
      l @ parse_fields s
  | [< '','|' '; s >] -> parse_fields s
  | [< >] -> []

let select_columns ~items ~titles =
  let w = GWindow.dialog ~modal:true () in
  let vbox = w#vbox in
  List.iter2 titles (Array.to_list items) ~f:
    begin fun title item ->
      match item with None -> ()
      | Some it ->
          let b =
            GButton.check_button ~label:title ~active:it#active
              ~packing:vbox#add () in
          ignore (b#connect#toggled
                    ~callback:(fun () -> it#set_active b#active))
    end;
  let close = GButton.button ~label:"Close" ~packing:w#action_area#add () in
  close#connect#clicked ~callback:w#destroy;
  w#show ()

let main () =
  let file = ref "" and fields = ref "" in
  Arg.parse ["-fields", Arg.Set_string fields, "fields to display"]
    ((:=) file) "Usage: csview <csv file>";
  let fields = parse_fields (Stream.of_string !fields) in
  let locale = Main.init ~setlocale:true () in
  let ic = if !file = "" then stdin else open_in !file in
  let data = read_file ic in
  if !file <> "" then close_in ic;
  let w = GWindow.window () in
  w#connect#destroy ~callback:Main.quit;
  let vbox = GPack.vbox ~packing:w#add () in
  let mbar = new GMenu.factory (GMenu.menu_bar ~packing:vbox#pack ()) in
  let columns = new GMenu.factory (mbar#add_submenu "Columns") in
  let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~width:600 ~height:300 ~packing:vbox#add () in
  let cl = GList.clist ~titles:data.titles ~packing:sw#add () in
  let metrics = cl#misc#pango_context#get_metrics () in
  let w0 = GPango.to_pixels metrics#approx_digit_width in
  let items = Array.create (List.length data.titles) None in
  columns#add_item "Select"
    ~callback:(fun () -> select_columns ~items ~titles:data.titles);
  let sort_col = ref (-1) in
  cl#connect#click_column ~callback:
    begin fun n ->
      cl#set_sort ~column:n ();
      cl#sort ();
      (*
      match items.(n) with None -> ()
      | Some it -> it#set_active false
       *)
    end;
  let width ~col ~f =
    let w =
      try List.assoc col fields with Not_found -> -1 in
    if w <> -1 then w else
    try List.assoc f field_widths with Not_found -> -1
  in
  List.fold_left2 data.titles data.fields ~init:0 ~f:
    begin fun col title f ->
      let width = width ~col ~f in
      let active = (fields = [] && width <> 0) || List.mem_assoc col fields in
      items.(col) <- Some
          (columns#add_check_item title ~active
             ~callback:(fun b -> cl#set_column col ~visibility:b));
      if not active then
        cl#set_column ~visibility:false col
      else if f = "NAPR" || f = "TIM1" || f = "CLAS" then
        cl#set_sort ~auto:true ~column:col ();
      succ col
    end;
  List.iter data.data
    ~f:(fun l -> if List.length l > 1 then ignore (cl#append l));
  cl#columns_autosize ();
  List.fold_left data.fields ~init:0 ~f:
    begin fun col f ->
      let width = width ~col ~f in
      if width > 0 then cl#set_column ~width:(width * w0) col;
      succ col
    end;
  w#show ();
  Main.main ()

let () =
  if not !Sys.interactive then main ()
