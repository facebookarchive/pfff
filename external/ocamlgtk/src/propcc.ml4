(* -*- caml -*- *)
(* $Id: propcc.ml4 1473 2009-09-01 08:49:45Z ben_99_9 $ *)

open StdLabels
open MoreLabels

let caml_keywords = ["type","kind"; "class","classe"; "list", "liste"]
let caml_modules = ["List", "Liste"]

let is_not_uppercase = function
  | 'A' .. 'Z' -> false
  | _ -> true
let camlize id =
  let b = Buffer.create (String.length id + 4) in
  for i = 0 to String.length id - 1 do
    match id.[i] with
    | 'A' .. 'Z' as c ->
	if i > 0 && 
	  (is_not_uppercase id.[i-1] || 
	  (i < String.length id - 1 && is_not_uppercase id.[i+1]))
	then Buffer.add_char b '_' ;
	Buffer.add_char b (Char.lowercase c)
    | '-' ->
	Buffer.add_char b '_'
    | c ->
	Buffer.add_char b c
  done;
  let s = Buffer.contents b in
  try List.assoc s caml_keywords with Not_found -> s

let camlizeM s =
  try List.assoc s caml_modules with Not_found -> s

let check_suffix s suff =
  let len1 = String.length s and len2 = String.length suff in
  len1 > len2 && String.sub s (len1-len2) len2 = suff

(* Arity of a caml type. Doesn't handle object types... *)
let arity s =
  let parens = ref 0 and arity = ref 0 in
  for i = 0 to String.length s - 1 do
    if s.[i] = '(' || s.[i] = '[' then incr parens else
    if s.[i] = ')' || s.[i] = ']' then decr parens else
    if !parens = 0 && s.[i] = '-' && s.[i+1] = '>' then incr arity
  done;
  if !parens <> 0 then failwith ("bad type : " ^ s);
  !arity

let rec min_labelled = function
  | [] -> []
  | a :: l ->
      let l = min_labelled l in
      if l = [] && a = "" then [] else a::l


(* The real data *)
let conversions = Hashtbl.create 17

let enums = [
  "Gtk", "GtkEnums",
  [ "Justification"; "ArrowType"; "ShadowType"; "ResizeMode";
    "ReliefStyle"; "ImageType"; "WindowType"; "WindowPosition";
    "ButtonsType"; "MessageType"; "ButtonBoxStyle"; "PositionType";
    "Orientation"; "ToolbarStyle"; "IconSize"; "PolicyType";
    "CornerType"; "SelectionMode"; "SortType"; "WrapMode";
    "SpinButtonUpdatePolicy"; "UpdateType"; "ProgressBarStyle";
    "ProgressBarOrientation"; "CellRendererMode"; "CellRendererAccelMode";
    "TreeViewColumnSizing"; "SortType"; "TextDirection"; "SizeGroupMode";
    (* in signals *)
    "MovementStep"; "ScrollType"; "MenuDirectionType"; "DeleteType";
    "StateType";
    (* for canvas *)
    "AnchorType"; "DirectionType"; 
  ];
  "Gdk", "GdkEnums",
  [ "ExtensionMode"; "WindowTypeHint"; "EventMask";
    (* for canvas *)
    "CapStyle"; "JoinStyle"; "LineStyle"];
  "Pango", "PangoEnums",
  [ "Stretch"; "Style"; "Underline"; "Variant"; "EllipsizeMode" ];
  (* GtkSourceView *)
  "Gtk","SourceView2Enums",
  ["SourceSmartHomeEndType"; "SourceDrawSpacesFlags"]
]

(* These types must be registered with g_boxed_register! *)
let boxeds = [
  "Gdk", ["Color"; "Font";];
  "Pango", ["FontDescription";];
  "Gtk", ["IconSet";"SelectionData";"TextIter";"TreePath"; "TreeIter";];
]

let classes = [
  "Gdk", [ "Image"; "Pixmap"; "Bitmap"; "Screen"; "DragContext";];
  "Gtk", [ "Style"; "TreeStore"; "TreeModel"; "TreeModelFilter"; "Tooltip" ]
]

let specials = [
  "GtkWidget", "GObj.conv_widget";
  "GtkWidget_opt", "GObj.conv_widget_option";
  "GtkAdjustment", "GData.conv_adjustment";
  "GtkAdjustment_opt", "GData.conv_adjustment_option";
]

let add_pointer conv gtk name =
  Hashtbl.add conversions gtk
    (Printf.sprintf "(%s : %s data_conv)" conv name);
  Hashtbl.add conversions (gtk ^ "_opt")
    (Printf.sprintf "(%s_option : %s option data_conv)" conv name)

let add_object = add_pointer "gobject"
let add_boxed = add_pointer "unsafe_pointer" (* the type is not used *)

let () =
  List.iter ~f:(fun t -> Hashtbl.add conversions ("g"^t) t)
    [ "boolean"; "char"; "uchar"; "int"; "uint"; "long"; "ulong";
      "int32"; "uint32"; "int64"; "uint64"; "float"; "double" ];
  List.iter ~f:(fun (gtype,conv) -> Hashtbl.add conversions gtype conv)
    [ "gchararray", "string";
      "gchararray_opt", "string_option";
      "string", "string"; "bool", "boolean"; "int", "int";
      "int32", "int32"; "float", "float";
    ];
  List.iter enums ~f:(fun (pre, modu, l) ->
    List.iter l ~f:
      begin fun name ->
        Hashtbl.add conversions (pre ^ name)
          (Printf.sprintf "%s.%s_conv" modu (camlize name))
      end);
  List.iter boxeds ~f:(fun (pre, l) ->
    List.iter l ~f:(fun name -> add_boxed (pre^name) (pre^"."^camlize name)));
  List.iter classes ~f:(fun (pre,l) ->
    List.iter l ~f:(fun t -> add_object (pre^t) (pre^"."^camlize t)));
  add_object "GObject" "unit obj";
  add_object "GtkWidget" "Gtk.widget obj"

open Genlex

let lexer = make_lexer ["{"; "}"; ":"; "/"; "("; ")";"->";"method";"signal"]

let rec star ?(acc=[]) p = parser
    [< x = p ; s >] -> star ~acc:(x::acc) p s
  | [< >] -> List.rev acc

let may_token tok s =
  if Stream.peek s = Some tok then Stream.junk s

let ident = parser [< ' Ident id >] -> id

let string = parser [< ' String s >] -> s

let may_colon p def = parser
  | [< ' Kwd":"; s >] -> p s
  | [< >] -> def

let may_string def = parser
    [< ' String s >] -> s
  | [< >] -> def

let may_name s = parser
    [< ' Kwd"("; ' Ident id; ' Kwd")" >] -> id
  | [< >] -> (camlize s)

let next_attr = parser
    [< ' Kwd"/"; ' Ident id; ids = star ~acc:[id] ident >] ->
      String.concat ~sep:"" ids

let attributes =
  ["Read";"Write";"Construct";"ConstructOnly";"NoSet";"Set";
   "NoWrap";"Wrap";"NoGet";"VSet";"NoVSet"]

let label_type2 id = parser
  | [< ' Kwd":"; ' Ident ty >] -> (id,ty)
  | [< >] -> ("",id)
let label_type = parser
    [< ' Ident id ; lty = label_type2 id >] -> lty

type marshal =
    Function of string | Types of (string list * string list * string)

let return_type (l,types) = parser
    [< ' Kwd"->"; ' Ident ret >] -> Types (l, types, ret)
  | [< >] -> Types (l, types, "")

let marshaller = parser
  | [< ' String s >] -> Function s
  | [< ' Kwd":"; types = star label_type; s >] ->
      return_type (List.split types) s
  | [< >] -> Types ([], [], "")

let simple_attr = parser [< ' Kwd"/"; ' Ident s >] -> s

let field = parser
    [< ' String name; mlname = may_name name; ' Ident gtype; ' Kwd":";
       ' Ident attr0; attrs = star ~acc:[attr0] next_attr >] ->
         if not (List.for_all attrs ~f:(List.mem ~set:attributes)) then
           raise (Stream.Error "bad attribute");
         `Prop (name, mlname, gtype, attrs)
  | [< ' Kwd"method"; ' Ident name; ty = may_colon string "unit";
       attrs = star simple_attr >] ->
         if not (List.for_all attrs ~f:(List.mem ~set:["Wrap"])) then
           raise (Stream.Error "bad attribute");
         `Method (name, ty, attrs)
  | [< ' Kwd"signal"; ' Ident name; m = marshaller; l = star simple_attr >] ->
      if not (List.for_all l ~f:(List.mem ~set:["Wrap";"NoWrap"])) then
        raise (Stream.Error "bad attribute");
      `Signal (name, m, l)

let split_fields l =
  List.fold_right l ~init:([],[],[]) ~f:
    (fun field (props,meths,sigs) -> match field with
      `Prop p   -> (p::props,meths,sigs)
    | `Method m -> (props,m::meths,sigs)
    | `Signal s -> (props,meths,s::sigs))

let verb_braces = ref 0

let rec verbatim buf = parser
  | [< ''}' ; s >] ->
      if !verb_braces = 0 then Buffer.contents buf else begin
        decr verb_braces; Buffer.add_char buf '}'; verbatim buf s;
      end
  | [< ''{'; s >] ->
      Buffer.add_char buf '{'; incr verb_braces; verbatim buf s
  | [< ''\\' ; 'c ; s >] ->
      if c <> '}' && c <> '{' then Buffer.add_char buf '\\';
      Buffer.add_char buf c; verbatim buf s
  | [< 'c ; s >] -> Buffer.add_char buf c; verbatim buf s

let read_pair = parser
  | [< ' Ident cls ; data = may_string (camlize cls) >] -> (cls,data)

let qualifier = parser
  | [< ' Ident id ; data = may_string "" >] -> (id,data)

let prefix = ref ""
let tagprefix = ref ""
let decls = ref []
let headers = ref []
let oheaders = ref []
let checks = ref false
let class_qualifiers =
  ["abstract";"notype";"hv";"set";"wrap";"wrapset";"vset";"tag";"wrapsig";
   "type";"gobject";]

let process_phrase ~chars = parser
    [< ' Ident"class"; ' Ident name; gtk_name = may_string (!prefix ^ name);
       attrs = star qualifier; parent = may_colon ident "";
       ' Kwd"{"; fields = star field; ' Kwd"}" >] ->
         if List.exists attrs ~f:
             (fun (x,_) -> not (List.mem x class_qualifiers))
         then raise (Stream.Error "bad qualifier");
         let attrs = ("parent",parent) :: attrs in
         let attrs =
           if parent = "GObject" then ("gobject","")::attrs else attrs in
         let props, meths, sigs = split_fields fields in
         decls := (name, gtk_name, attrs, props, meths, sigs) :: !decls
  | [< ' Ident"header"; ' Kwd"{" >] ->
      let h = verbatim (Buffer.create 1000) chars in
      headers := !headers @ [h]
  | [< ' Ident"oheader"; ' Kwd"{" >] ->
      let h = verbatim (Buffer.create 1000) chars in
      oheaders := !oheaders @ [h]
  | [< ' Ident"prefix"; ' String id >] ->
      prefix := id
  | [< ' Ident"tagprefix"; ' String id >] ->
      tagprefix := id
  | [< ' Ident"conversions"; pre1 = may_string ""; pre2 = may_string pre1;
       ' Kwd"{"; l = star read_pair; ' Kwd"}" >] ->
      List.iter l ~f:(fun (k,d) ->
        Hashtbl.add conversions (pre1^k) (if pre2="" then d else pre2^"."^d))
  | [< ' Ident"classes"; ' Kwd"{"; l = star read_pair; ' Kwd"}" >] ->
      List.iter l ~f:(fun (k,d) -> add_object k d)
  | [< ' Ident"boxed"; ' Kwd"{"; l = star read_pair; ' Kwd"}" >] ->
      List.iter l ~f:(fun (k,d) -> add_boxed k d)
  | [< ' _ >] ->
      raise (Stream.Error "")
  | [< >] ->
      raise End_of_file

let all_props = Hashtbl.create 137
let all_pnames = Hashtbl.create 137
let outfile = ref ""
let ooutfile = ref ""

let process_file f =
  let base = Filename.chop_extension f in
  let baseM = String.capitalize base in
  prefix := baseM;
  (* Input *)
  (* Redefining saves space in bytecode! *)
  headers  := ["open Gobject"; "open Data";
               "module Object = GtkObject"];
  oheaders := ["open GtkSignal"; "open Gobject"; "open Data";
               "let set = set"; "let get = get"; "let param = param"];
  let ic = open_in f in
  let chars = Stream.of_channel ic in
  let s = lexer chars in
  begin try while true do process_phrase ~chars s done with
    End_of_file -> ()
  | Stream.Error _ | Stream.Failure ->
      Printf.eprintf "Parse error in file `%s' before char %d\n"
        f (Stream.count chars);
      exit 2
  | exn ->
      Printf.eprintf "Exception %s in file `%s' before char %d\n"
        (Printexc.to_string exn) f (Stream.count chars);
      exit 2
  end;
  (* Preproccess *)
  let type_name name ~attrs =
    try List.assoc "type" attrs with Not_found ->
      if List.mem_assoc "gobject" attrs then camlize name
      else if !prefix <> ""
      then !prefix ^ "." ^ camlize name ^ " obj"
      else camlize name ^ " obj"
  in
  let decls = List.rev !decls in
  let decls = List.filter decls
      ~f:(fun (_,_,attrs,_,_,_) -> not (List.mem_assoc "notype" attrs)) in
  List.iter decls ~f:
    (fun (name, gtk_name, attrs, _, _, _) ->
      add_object gtk_name (type_name name ~attrs));
  (* Output modules *)
  if !outfile = "" then outfile := base ^ "Props.ml";
  let oc = open_out !outfile in
  let ppf = Format.formatter_of_out_channel oc in
  let out fmt = Format.fprintf ppf fmt in
  List.iter !headers ~f:(fun s -> out "%s@." s);
  let decls =
    List.map decls ~f:
      begin fun (name, gtk_name, attrs, props, meths, sigs) ->
        (name, gtk_name, attrs,
         List.filter props ~f:
           begin fun (name,_,gtype,_) ->
             try ignore (Hashtbl.find conversions gtype);
               try
                 let count, _ = Hashtbl.find all_props (name,gtype) in
                 incr count;
                 true
               with Not_found ->
                 Hashtbl.add all_props (name,gtype) (ref 1, ref ""); true
             with Not_found ->
               prerr_endline ("Warning: no conversion for type " ^ gtype ^
                              " in class " ^ gtk_name);
               false
           end,
         meths,
         List.filter sigs ~f:
           begin function
           | _, Function _, _ -> true
           | _, Types(_, l, ret), _ ->
               List.for_all (if ret = "" then l else ret::l) ~f:
                 (fun ty ->
                   if Hashtbl.mem conversions ty then true else
                   (prerr_endline ("Warning: no conversion for type " ^ ty ^
                                   " in class " ^ gtk_name);
                    false))
           end)
      end in
  let defprop ~name ~mlname ~gtype ~tag =
    let conv = Hashtbl.find conversions gtype in
    out "@ @[<hv2>let %s " mlname;
    if tag <> "gtk" then out ": ([>`%s],_) property " tag;
    out "=@ @[<hov1>{name=\"%s\";@ conv=%s}@]@]" name conv
  in
  let shared_props =
    Hashtbl.fold all_props ~init:[] ~f:
      begin fun ~key:(name,gtype) ~data:(count,rpname) acc ->
        if !count <= 1 then acc else
        let pname = camlize name in
        let pname =
          if Hashtbl.mem all_pnames pname then pname ^ "_" ^ gtype
          else (Hashtbl.add all_pnames pname (); pname) in
        rpname := "PrivateProps." ^ pname;
        (pname,name,gtype) :: acc
      end
  in
  if shared_props <> [] then begin
    out "@[<hv2>module PrivateProps = struct";
    List.iter (List.sort compare shared_props) ~f:
      (fun (pname,name,gtype) ->
        defprop ~name ~mlname:pname ~gtype ~tag:"gtk");
    out "@]\nend\n@.";
  end;
  (* Redefining saves space in bytecode! *)
  out "let may_cons = Property.may_cons\n";
  out "let may_cons_opt = Property.may_cons_opt\n@.";
  let may_cons_props props =
    if props <> [] then begin
      out "@ @[<hv2>let pl = ";
      List.iter props ~f:
        begin fun (name,mlname,gtype,_) ->
          let op =
            if check_suffix gtype "_opt" then "may_cons_opt" else "may_cons"
          in
          out "(@;<0>%s P.%s %s " op (camlize name) mlname;
        end;
      out "pl";
      for k = 1 to List.length props do out ")" done;
      out " in@]"
    end
  in
  let omarshaller ~gtk_class ~name ppf (l,tyl,ret) =
    let out fmt = Format.fprintf ppf fmt in
    out "fun f ->@ @[<hov2>marshal%d" (List.length l);
    if ret <> "" then
      out "_ret@ ~ret:%s" (Hashtbl.find conversions ret);
    List.iter tyl ~f:(fun ty -> out "@ %s" ty);
    out "@ \"%s::%s\"" gtk_class name;
    if List.for_all l ~f:((=) "") then out " f" else begin
      let l = min_labelled l in
      out "@ @[<hov2>(fun ";
      for i = 1 to List.length l do out "x%d " i done;
      out "->@ f";
      let i = ref 0 in
      List.iter l ~f:
        (fun p ->
          incr i; if p="" then out "@ x%d" !i else out "@ ~%s:x%d" p !i);
      out ")@]";
    end;
    out "@]"
  in
  List.iter decls ~f:
    begin fun (name, gtk_class, attrs, props, meths, sigs) ->
      out "@[<hv2>module %s = struct" (camlizeM name);
      out "@ @[<hv2>let cast w : %s =@ try_cast w \"%s\"@]"
        (type_name name ~attrs) gtk_class;
      let tag =
        try List.assoc "tag" attrs
        with Not_found -> !tagprefix ^ String.lowercase name
      in
      if props <> [] then begin
        out "@ @[<hv2>module P = struct";
        List.iter props ~f:
          begin fun (name, _, gtype, attrs) ->
            let count, rpname = Hashtbl.find all_props (name,gtype) in
            if !count > 1 then begin
              out "@ let %s : ([>`%s],_) property = %s"
                (camlize name) tag !rpname
            end else
              defprop ~name ~mlname:(camlize name) ~gtype ~tag
          end;
        out "@]@ end"
      end;
      if sigs <> [] then begin
        out "@ @[<hv2>module S = struct@ open GtkSignal";
        List.iter sigs ~f:
          begin fun (name,marshaller,_) ->
            out "@ @[<hv2>let %s =" (camlize name);
            out "@ @[<hov1>{name=\"%s\";@ classe=`%s;@ marshaller="
              name tag;
            begin match marshaller with
            | Function s -> out "%s" s
            | Types ([], [], "") -> out "marshal_unit" 
            | Types ([], [], ret) ->
                out "fun f -> marshal0_ret ~ret:%s f"
                  (Hashtbl.find conversions ret)
            | Types (l, tyl, ret) ->
                omarshaller ~gtk_class ~name ppf
                  (l, List.map (Hashtbl.find conversions) tyl, ret)
            end;
            out "}@]@]";
          end;
        out "@]@ end";
      end;
      if not (List.mem_assoc "abstract" attrs) then begin
        let cprops = List.filter props ~f:(fun (_,_,_,a) ->
          List.mem "ConstructOnly" a && not (List.mem "NoSet" a)) in
        out "@ @[<hv2>let create";
        List.iter cprops ~f:(fun (_,name,_,_) -> out " ?%s" name);
        if List.mem_assoc "hv" attrs then begin
          out " (dir : Gtk.Tags.orientation) pl : %s ="
            (type_name name ~attrs);
          may_cons_props cprops;
          out "@ @[<hov2>Object.make";
          out "@ (if dir = `HORIZONTAL then \"%sH%s\" else \"%sV%s\")@  pl"
            !prefix name !prefix name;
          out "@]@]";
        end else begin
          out " pl : %s =" (type_name name ~attrs);
          may_cons_props cprops;
          if List.mem_assoc "gobject" attrs then
            out "@ Gobject.unsafe_create"
          else out "@ Object.make";
           out " \"%s\" pl@]" gtk_class;
        end
      end;
      List.iter meths ~f:
        begin fun (name, typ, attrs) ->
          out "@ @[<hov2>external %s :" name;
          out "@ @[<hv>[>`%s] obj ->@ %s@]" tag typ;
          let cname = camlize ("ml" ^ gtk_class) ^ "_" ^ name in
          out "@ = \"";
          if arity typ > 4 then out "%s_bc\" \"" cname;
          out "%s\"@]" cname;
        end;
      let set_props =
        let set = List.mem_assoc "set" attrs in
        List.filter props ~f:
          (fun (_,_,_,a) ->
            (set || List.mem "Set" a) && List.mem "Write" a &&
            not (List.mem "ConstructOnly" a || List.mem "NoSet" a))
      in
      if set_props <> [] then begin
        let props = set_props in
        out "@ @[<hv2>@[<hov4>let make_params ~cont pl";
        List.iter props ~f:(fun (_,name,_,_) -> out "@ ?%s" name);
        out " =@]";
        may_cons_props props;
        out "@ cont pl@]";
      end;
      if !checks && (props <> [] || sigs <> []) then begin
        if List.mem_assoc "abstract" attrs then 
          out "@ @[<hv2>let check w ="
        else begin
          out "@ @[<hv2>let check () =";
          out "@ let w = create%s [] in"
            (if List.mem_assoc "hv" attrs then " `HORIZONTAL" else "");
        end;
        if props <> [] then out "@ let c p = Property.check w p in";
        if sigs <> [] then begin
          out "@ let closure = Closure.create ignore in";
          out "@ let s name = GtkSignal.connect_by_name";
          out " w ~name ~closure ~after:false in";
        end;
        out "@ @[<hov>";
        List.iter props ~f:
          (fun (name,_,gtype,attrs) ->
            if List.mem "Read" attrs then out "c P.%s;@ " (camlize name));
        List.iter sigs ~f:(fun (name,_,_) -> out "s %s;@ " name);
        out "()@]";
      end;
      out "@]@.end\n@."
    end;
  close_out oc;
  (* Output classes *)
  if !ooutfile = "" then ooutfile := "o" ^ !outfile;
  let oc = open_out !ooutfile in
  let ppf = Format.formatter_of_out_channel oc in
  let out fmt = Format.fprintf ppf fmt in
  List.iter !oheaders ~f:(fun s -> out "%s@." s);
  out "open %s@." (String.capitalize (Filename.chop_extension !outfile));
  out "@[<hv>";
  let oprop ~name ~gtype ppf pname =
    try
      let conv = List.assoc gtype specials in
      Format.fprintf ppf "{%s.P.%s with conv=%s}"
        (camlizeM name) (camlize pname) conv
    with Not_found ->
      Format.fprintf ppf "%s.P.%s" (camlizeM name) (camlize pname)
  in
  List.iter decls ~f:
    begin fun (name, gtk_class, attrs, props, meths, sigs) ->
      let wrap = List.mem_assoc "wrap" attrs in
      let wrapset = wrap || List.mem_assoc "wrapset" attrs in
      let wr_props =
        List.filter props ~f:
          (fun (_,_,_,set) ->
            let has = List.mem ~set in
            (wrapset || has "Wrap" || has "WrapSet") && has "Write" &&
            not (has "ConstructOnly" || has "NoWrap" || has "WrapGet"))
      and rd_props =
        List.filter props ~f:
          (fun (_,_,_,set) ->
            let has = List.mem ~set in
            (wrap || has "Wrap" || has "WrapGet") && has "Read" &&
            not (has "NoWrap" || has "WrapSet"))
      and wr_meths =
        List.filter meths ~f:(fun (_,_,attrs) -> List.mem "Wrap" attrs)
      in
      if wr_props <> [] || rd_props <> [] || wr_meths <> [] then begin
        (* pre 3.10
        out "@ @[<hv2>class virtual %s_props = object (self)" (camlize name);
        out "@ method private virtual obj : _ obj";
        List.iter wr_props ~f:(fun (pname,mlname,gtype,_) ->
          out "@ @[<hv2>method set_%s =@ set %a self#obj@]"
            mlname (oprop ~name ~gtype) pname);
        List.iter rd_props ~f:(fun (pname,mlname,gtype,_) ->
          out "@ @[<hv2>method %s =@ get %a self#obj@]"
            mlname (oprop ~name ~gtype) pname);
        List.iter wr_meths ~f:(fun (mname,typ,_) ->
          out "@ @[<hv2>method %s %s=@ %s.%s self#obj@]"
            mname (if typ = "unit" then "() " else "") (camlizeM name) mname);
        *)
        (* post 3.10 *)
        out "@ @[<hv2>class virtual %s_props = object" (camlize name);
        out "@ val virtual obj : _ obj";
        List.iter wr_props ~f:(fun (pname,mlname,gtype,_) ->
          out "@ @[<hv2>method set_%s =@ set %a obj@]"
            mlname (oprop ~name ~gtype) pname);
        List.iter rd_props ~f:(fun (pname,mlname,gtype,_) ->
          out "@ @[<hv2>method %s =@ get %a obj@]"
            mlname (oprop ~name ~gtype) pname);
        List.iter wr_meths ~f:(fun (mname,typ,_) ->
          out "@ @[<hv2>method %s %s=@ %s.%s obj@]"
            mname (if typ = "unit" then "() " else "") (camlizeM name) mname);
        out "@]@ end@ "
      end;
      let vset = List.mem_assoc "vset" attrs in
      let vprops =
        List.filter props ~f:
          (fun (_,_,_,set) ->
            let has = List.mem ~set in
            (vset || has "VSet") && has "Write" &&
            not (has "ConstructOnly" || has "NoVSet"))
      in
      if vprops <> [] then begin
        out "@ @[<hv2>let %s_param = function" (camlize name);
        List.iter vprops ~f:(fun (pname,mlname,gtype,_) ->
          out "@ @[<hv4>| `%s p ->@ param %a p@]"
            (String.uppercase mlname) (oprop ~name ~gtype) pname);
        out "@]@ ";
      end;
      let wsig = List.mem_assoc "wrapsig" attrs in
      let wsigs =
        List.filter sigs ~f:
          (fun (_,_,attrs) ->
            List.mem "Wrap" attrs || wsig && not (List.mem "NoWrap" attrs))
      in
      if wsigs <> [] then begin
        out "@ @[<hv2>class virtual %s_sigs = object (self)" (camlize name);
        out "@ @[<hv2>method private virtual connect :";
        out "@ 'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id@]";
        List.iter wsigs ~f:
          begin fun (sname, types,_) ->
            match types with Types(l, tyl,ret)
              when List.exists tyl ~f:(List.mem_assoc ~map:specials) ->
                let convs =
                  List.map tyl ~f:
                    (fun ty -> try List.assoc ty specials
                      with Not_found -> Hashtbl.find conversions ty)
                in
                out "@ @[<hov2>method %s =@ self#connect" sname;
                out "@ @[<hov1>{%s.S.%s with@ marshaller = %a}@]@]"
                  (camlizeM name) sname
                  (omarshaller ~gtk_class ~name:sname) (l, convs,ret)
            | _ ->
                out "@ @[<hv2>method %s =@ self#connect %s.S.%s@]"
                  sname (camlizeM name) sname
          end;
        out "@]@ end@ "
      end
    end;
  out "@.";
  close_out oc;
  outfile := "";
  ooutfile := ""

let main () =
  Arg.parse
    [ "-checks", Arg.Set checks, "generate code for checks";
      "-o", Arg.String (fun s -> outfile := s), "basic output file name";
      "-oo", Arg.String (fun s -> ooutfile := s), "wrappers output file name" ]
    process_file "usage: propcc <options> file.props ..."

let () = Printexc.print main ()
