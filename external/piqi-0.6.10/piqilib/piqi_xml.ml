(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


(* XML format parsing and generation using Xmlm library:
 *
 *      http://erratique.ch/software/xmlm
 *
 * Details about how Xmlm parses XML are available here:
 *
 *      http://erratique.ch/software/xmlm/doc/Xmlm
 *)


module C = Piqi_common


type xml = Piqi_xml_type.xml
type xml_elem = Piqi_xml_type.xml_elem


type xml_parser =
  {
    input : Xmlm.input;
    fname : string; (* name of the file *)
  }


let init_xml_parser ?(fname = "input") source :xml_parser =
  (* don't strip whitespace in CDATA and expect UTF-8 input (no other encodings
   * are supported by Piqi)
   *
   * NOTE: according to Xmlm documentation, even when we specify ~strip:false,
   * "all kinds of line ends are translated to the newline character (U+000A)"
   *
   * NOTE: we use a custom whitespace stripper below that doesn't strip leading
   * and trailing whitespace in text nodes.
   *)
  let input = Xmlm.make_input source ~enc:(Some `UTF_8) ~strip:false in
  {
    input = input;
    fname = fname;
  }


let init_from_channel ?fname ch =
  let source = `Channel ch in
  init_xml_parser source ?fname


let init_from_string ?fname s =
  let source = `String (0, s) in
  init_xml_parser source ?fname


(* XML input *)

(* custom whitespace stripper, that srips only formatting whitespace and leaves
 * text nodes untouched *)
let strip_whitespace (l :xml list) =
  match l with
    | [(`Data _)] -> l
    | _ ->
      (* there is at least one element in the list; stripping all the data around
       * and between the elements *)
      List.filter (function `Elem _ -> true | `Data _ -> false) l


let do_read_xml_obj xml_parser :xml =
  let make_loc (line, col) =
    (xml_parser.fname, line, col)
  in
  (* below are cusomized versions of Xmlm.input_tree and Xmlm.input_doc_tree
   * functions that capture accurate information about location of elements and
   * data in the input stream *)
  let input_tree ~el ~data i =
    let rec aux tags context =
      let pos = Xmlm.pos i in
      match Xmlm.input i with
        | `El_start tag ->
            aux ((pos, tag) :: tags) ([] :: context)
        | `El_end -> 
            begin match tags, context with
            | (pos, tag) :: tags', childs :: context' ->
                let el = el pos tag (List.rev childs) in 
                begin match context' with
                | parent :: context'' -> aux tags' ((el :: parent) :: context'')
                | [] -> el
                end
            | _ -> assert false
            end
        | `Data d ->
            begin match context with
            | childs :: context' -> aux tags (((data pos d) :: childs) :: context')
            | [] -> assert false
            end
        | `Dtd _ -> assert false
    in 
    aux [] []
  in
  let input_doc_tree ~el ~data i =
    let pos = Xmlm.pos i in
    match Xmlm.input i with
     | `Dtd d -> d, input_tree ~el ~data i
     | _ ->
         C.error_at (make_loc pos) "invalid XML header"
  in
  let el pos tag contents =
    let (ns, name), attr = tag in
    let contents = strip_whitespace contents in
    let loc = make_loc pos in

    (* check that there is no namespace and no attributes *)
    if ns <> ""
    then C.error_at loc "namespaces are not allowed in XML element names";

    if attr <> []
    then C.error_at loc "attributes are not allowed in XML elements";

    let xml_elem = (name, contents) in
    let res = `Elem xml_elem in

    (* add information about term locations to the location database *)
    Piqloc.addloc loc name;
    Piqloc.addloc loc xml_elem;
    Piqloc.addloc loc res;

    res
  in
  let data pos d =
    let res = `Data d in
    (* add information about term locations to the location database *)
    let loc = make_loc pos in
    Piqloc.addloc loc d;
    Piqloc.addloc loc res;

    res
  in
  try
    let _dtd, xml = input_doc_tree ~el ~data xml_parser.input in
    xml
  with
    Xmlm.Error (pos, err) ->
      let loc = make_loc pos in
      let errstr = Xmlm.error_message err in
      C.error_at loc errstr


let read_xml_obj (xml_parser :xml_parser) :xml option =
  let is_eoi =
    try Xmlm.eoi xml_parser.input
    with
      | Xmlm.Error (_pos, `Unexpected_eoi) ->
          (* raised on a completely empty input *)
          true
      | Xmlm.Error ((line, col), err) ->
          let loc = xml_parser.fname, line, col in
          let errstr = Xmlm.error_message err in
          C.error_at loc errstr
  in
  if is_eoi
  then None
  else 
    let xml = do_read_xml_obj xml_parser in
    Some xml


(* XML output
 *
 * We use 2-space indentation and output a newline character after the root
 * element.
 *
 * We do not use Xmlm's library indentation mode (although it would've been so
 * convenient), because it inserts indentation around text nodes which leads to
 * extra whitespace. This means we will get extra whitespace when we read
 * serialized values of primitive types back. And this is not what we want for
 * data serialization purposes.
 *
 * For this reason, we do indentation ourselves using `Data elements filled with
 * newlines and whitespace. This way we generate indented XML and still can read
 * it back reliably. Other XML serializers may behave differently, but we don't
 * really care as we can set our own rules in this case.
 *)

(* helpers *)
let ws = `Data "  " (* 2 spaces indentation *)
let nl = `Data "\n" (* newline *)


(* build a list of [ ws; ... ws ] :: l where the number of ws nodes is
 * determined by the depth parameter *)
let indent_list depth l =
  let rec aux depth accu =
    if depth = 0
    then accu
    else aux (depth-1) (ws :: accu)
  in
  aux depth l


(* rewrite xml tree to inject indentation represented as `Data elements
 * containing either whitespace or newlines *)
let indent_tree (xml: xml) :xml =
  let rec aux depth node =
    match node with
      | `Data _               (* don't indent data elements *)
      | `Elem (_, [`Data _])
      | `Elem (_, []) -> node (* nothing to indent inside empty element *)
      | `Elem (name, contents) -> (* non-empty non-data contents *)
          let l =
            List.fold_left (fun accu x ->
              let x = aux (depth + 1) x in (* indent the sub-tree *)
              indent_list (depth + 1) (x :: nl :: accu)
            )
            (indent_list depth []) (* indent right before the closing tag *)
            (List.rev contents)
          in
          let contents = nl :: l in (* newline after the opening tag *)
          `Elem (name, contents)
  in
  aux 0 xml


let gen_xml ?(pretty_print=true) ?(nl=false) ?(decl=true) dest (xml :xml) =
  let frag = function (* xml to Xmlm.frag converter *)
    | `Data x -> `Data x
    | `Elem (name, contents) ->
        let tag = ("", name), [] in (* no namespace, no attributes *)
        `El (tag, contents)
  in
  let xml =
    if pretty_print
    then indent_tree xml
    else xml
  in
  let output = Xmlm.make_output dest ~nl ~decl in
  let dtd = None in
  Xmlm.output_doc_tree frag output (dtd, xml)


let xml_to_buffer ?pretty_print ?decl buf xml =
  let dest = `Buffer buf in
  gen_xml dest xml ?pretty_print ?decl


let xml_to_channel ?pretty_print ch xml =
  let dest = `Channel ch in
  (* output a newline character after the root element so that the file ends
   * with a newline *)
  gen_xml dest xml ?pretty_print ~nl:true


let xml_to_string ?pretty_print ?decl xml =
  let buf = Buffer.create 256 in
  xml_to_buffer buf xml ?pretty_print ?decl;
  Buffer.contents buf


(* for internal use only: read one parsed XML value from its string
 * representation *)
let xml_of_string s :xml list =
  let xml_parser = init_from_string s in
  let res =
    try read_xml_obj xml_parser
    with C.Error ((_, lnum', cnum'), error) ->
      (* string location can be missing when we parse from XML embedded in
       * Protobuf *)
      let (fname, lnum, cnum) =
        try Piqloc.find s
        with Not_found -> ("embedded", 1, -1)
      in
      (* adjust location column number: add the original column number of the
       * '#' character + 1 for the space that follows it; note that this method
       * doesn't give 100% guarantee that the offset is correct, but it is
       * accurate if all the text literal lines start at the same column *)
      let loc = (fname, lnum + lnum' - 1, cnum + cnum' + 1) in
      C.error_at loc ("error parsing embedded XML: " ^ error)
  in
  (* TODO: check for trailing XML data -- there shouldn't be any after this
   * object we've just read *)
  match res with
    | Some (`Elem (_name, xml_list)) ->
        (* as in other places, e.g. Piqobj_of_xml, we ignore the top-level
         * elemnt's name *)
        xml_list
    | Some xml ->
        (* this should never happen, because the xml parser always returns
         * top-level element *)
        C.error xml "XML root element expected"
    | None ->
        C.error s "string doesn't have XML data"


let _ =
  (* pretty print and skip <?xml ...> declaration *)
  Piqobj.string_of_xml := (fun x -> xml_to_string x ~pretty_print:true ~decl:false);

  (* parse xml from string while not expecting <?xml ...> declaration *)
  Piqobj.xml_of_string := (fun x -> xml_of_string x)

