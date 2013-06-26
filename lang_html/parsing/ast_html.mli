
type html_raw = HtmlRaw of string

type html_tree = 
  | Element of tag * (attr_name * attr_value) list * html_tree list
  | Data of string wrap

 and tag = Tag of string wrap
 and attr_name = Attr of string wrap
 and attr_value = Val of string wrap

  and info = Parse_info.info
  and 'a wrap = 'a * info

(* html type in ocamlnet *)
(* type html_tree2 = Nethtml.document list *)

(* precise html AST; see lib_parsing_html.html_tree_to_html *)
type html = Html of attrs * head * (body, frameset) Common.either
 and head = Head of attrs * head_content list
 and head_content =
  | Title of attrs * plain_text
  | Style of attrs * plain_text
  | Meta of attrs
  | Link of attrs
  | Head_Script of attrs * plain_text
  | Head_Server of attrs * plain_text
  | Base of attrs
  | HeadContent_IsIndex of attrs
  | NextId of attrs

 and body = Body of attrs * body_content list
 and frameset = Frameset of attrs * frameset_content list
 and frameset_content = Frame of attrs | NoFrame of attrs * body_content list

 and body_content =
    Body_Heading of heading
  | Hr of attrs
  | Body_Flow of flow
  | Del of attrs * flow
  | Ins of attrs * flow
  | Address of attrs * address_content list
  | Marquee of attrs * style_text
  | Map of attrs * area list
  | Layer of attrs * body_content
  | Bgsound of attrs
 and heading =
    H1 of attrs * text
  | H2 of attrs * text
  | H3 of attrs * text
  | H4 of attrs * text
  | H5 of attrs * text
  | H6 of attrs * text
 and block = block_content list
  and block_content =
    Block_P of attrs * text
  | Div of attrs * body_content
  | Blockquote of attrs * body_content
  | Center of attrs * body_content
  | Form of attrs * form_content list
  | Table of attrs * caption option * colgroup list * table_content list
  | Pre of attrs * pre_content list
  | Samp of attrs * text
  | Listing of attrs * literal_text
  | Menu of attrs * li list
  | Multicol of attrs * body_content
  | Dl of attrs * dl_content list1
  | Ul of attrs * li list1
  | Ol of attrs * li list1
  | Block_Script of attrs * plain_text
  | Block_IsIndex of attrs
  | Basefont of attrs * body_content
  | Dir of attrs * li list1
  | Nobr of attrs * text
  | Xmp of attrs * literal_text
 and text = text_content list
  and text_content =
    PlainText of plain_text
  | PhysicalStyle of physical_style
  | ContentStyle of content_style
  | A of attrs * a_content list
  | Br of attrs
  | Img of attrs
  | Iframe of attrs
  | Embed of attrs
  | NoEmbed of attrs * text
  | Applet of attrs * applet_content
  | Object of attrs * object_content
  | NoScript of attrs * text
  | Ilayer of attrs * body_content
  | Spacer of attrs
  | Wbr of attrs
  and physical_style =
    B of attrs * text
  | I of attrs * text
  | Tt of attrs * text
  | Big of attrs * text
  | Small of attrs * text
  | Strike of attrs * text
  | S of attrs * text
  | Blink of attrs * text
  | U of attrs * text
  | Font of attrs * style_text
  | Sub of attrs * text
  | Sup of attrs * text
  | Span of attrs * text
  | Bdo of attrs * text
  and content_style =
    Em of attrs * text
  | Strong of attrs * text
  | Abbr of attrs * text
  | Acronym of attrs * text
  | Cite of attrs * text
  | Code of attrs * text
  | Dfn of attrs * text
  | Kbd of attrs * text
  | Q of attrs * text
  | Var of attrs * text
 and flow = flow_content list
  and flow_content = Flow_Block of block | Flow_Text of text
 and form_content =
    Form_Input of attrs
  | Form_Body of body_content
  | Form_TextArea of attrs * plain_text
  | Form_Select of attrs * select_content list
  | Fieldset of attrs * legend option * form_content list
  | Label of attrs * label_content list
  | Keygen of attrs
 and label_content =
    Label_Input of attrs
  | Label_Body of body_content
  | Label_TextArea of attrs * plain_text
  | Label_Select of attrs * select_content list
 and select_content =
    OptGroup of attrs * option_tag list
  | SelectOption of option_tag
 and legend = Legend of attrs * text
 and option_tag = Option of attrs * plain_text
 and caption = Caption of attrs * body_content
 and colgroup = Colgroup of attrs | ColgroupContent of colgroup_content list
 and colgroup_content = Col of attrs
 and table_content =
    THead of attrs
  | TFoot of attrs
  | TBody of attrs
  | Tr of attrs * table_cell list
 and table_cell = Th of attrs * body_content | Td of attrs * body_content
 and applet_content = Applet_Body of body_content | AppletParams of param list
 and object_content = applet_content
 and param = unit
 and li = Li of attrs * flow
 and dl_content = dt * dd
 and dt = Dt of attrs * text
 and dd = Dd of attrs * flow
 and a_content = A_Heading of heading | A_Text of text
 and pre_content =
    Pre_Br of attrs
  | Pre_Hr of attrs
  | Pre_A of attrs
  | Pre_Text of style_text
 and address_content = Address_P of attrs * text | Address_Text of text
 and area = unit
 and attrs = (attr_name * attr_value) list
 and plain_text = string wrap
 and style_text = string wrap
 and literal_text = string wrap
 and 'a list1 = 'a * 'a list

type any =
  | HtmlTree of html_tree

val fakeInfo:
  ?next_to:(Parse_info.token_location * int) option -> 
  ?str:string -> unit -> 
  Parse_info.info

val str_of_tag: tag -> string
