(* $Id: xHTML.mli,v 1.30 2005/06/20 17:57:58 ohl Exp $

   Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>

   XHTML is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by 
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   XHTML is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)  

(* IDEAS:

     It might be possible to factorize attributes and elements into separate
     modules.  Problems are attributes like [class] and [for] that class with
     reserved words.  Then the [a_] prefix would have to be maintained and the
     only advantage are a potentially better mapping of the XHTML modularization
     to O'Caml modules. *)

(** Typesafe constructors for XHTML 1.1 documents.
    @see <http://www.w3.org/TR/xhtml-modularization/abstract_modules.html> W3C Recommendation *)

module type T =
  sig

(** The elements, attributes, attribute types and data types are given names
    that match the names in the W3C recommendation as closely as allowed by
    a strict typing discipline and the lexical conventions of O'Caml:
    {ul
      {- {e elements} are implemented as O'Caml constructors with the same name as
         in the W3C recommendation.  The domain and codomain are specified as ['a elt],
         where ['a] is a concrete phantom type build out of polymorphic variants.}
      {- {e attributes} are implemented as O'Caml constructors with [a_] prefixed to the
         name.  The name is the same as in the W3C recommendation, unless an additional
         prefix is required to disambiguate:
         {ul
           {- [a_fs_rows] and [a_fs_cols] instead of [a_rows] and [a_cols] for framesets,
              because of the different argument types.}}}
      {- {e attribute types} are implemented as O'Caml types that all have the same names
         as in the W3C recommendation, but are all lowercase.}
      {- {e data types} are also implemented as O'Caml types that all have the same names
         as in the W3C recommendation and are again all lowercase.}}

    Finite sets of alternatives are mapped to polymorphic variants.

    The phantom type is always the {e most general} required by any (supported)
    version of the standard.  Type discipline is enforced by exporting or not-exporting
    the corresponding constructor.  *)

(** {1 Attribute Types}
    @see <http://www.w3.org/TR/xhtml-modularization/abstraction.html#s_common_attrtypes> Modularization of XHTML *)

    type cdata = string
(** Character data *)

    type id = string
(** A document-unique identifier *)

    type idref = string
(** A reference to a document-unique identifier *)

    type idrefs = idref list
(** A space-separated list of references to document-unique identifiers *)

    type name = string
(** A name with the same character constraints as ID above *)

    type nmtoken = string
(** A name composed of only name tokens as defined in XML 1.0
    @see <http://www.w3.org/TR/2000/REC-xml-20001006> XML 1.0 *)

    type nmtokens = nmtoken list
(** One or more white space separated NMTOKEN values *)

    type pcdata = string
(** Processed character data *)

(** {2 Data Types} *)

    type character = char
(** A single character from ISO 10646. *)

    type charset = string
(** A character encoding, as per RFC2045 (MIME).
    @see <http://www.ietf.org/rfc/rfc2045.txt> RFC2045 *)

    type charsets = charset list
(** A space-separated list of character encodings, as per RFC2045 (MIME).
    @see <http://www.ietf.org/rfc/rfc2045.txt> RFC2045 *)

    type color =
        [ `Aqua | `Black | `Blue | `Fuchsia | `Gray | `Green | `Lime | `Maroon
        | `Navy | `Olive | `Purple | `Red | `Silver | `Teal | `White | `Yellow
        | `Hex of string | `RGB of int * int * int ]
(** The attribute value type [color] refers to color definitions as specified in
    SRGB.  A color value may either be a hexadecimal number (prefixed by a hash mark)
    or one of the following sixteen color names. The color names are case-insensitive. 
    @see <http://www.w3.org/Graphics/Color/sRGB> A Standard Default Color Space for the Internet. *)

    type contenttype = string
(** A media type, as per RFC2045 (MIME).
    @see <http://www.ietf.org/rfc/rfc2045.txt> RFC2045 *)

    type contenttypes = contenttype list
(** A comma-separated list of media types, as per RFC2045 (MIME).
    @see <http://www.ietf.org/rfc/rfc2045.txt> RFC2045 *)

    type coords = string list
(** Comma- separated list of coordinates to use in defining areas. *)

    type datetime = string
(** Date and time information. *)

    type fpi = string
(** A character string representing an SGML Formal Public Identifier. *)

    type frametarget = string
(** Frame name used as destination for results of certain actions. *)

    type languagecode = string
(** A language code, as per RFC3066.
    @see <http://www.ietf.org/rfc/rfc3066.txt> RFC3066 *)

    type length = [ `Pixels of int | `Percent of int ]
(** The value may be either in pixels or a percentage of the available
    horizontal or vertical space. Thus, the value [`Percent 50] means half of
    the available space. *)

    type linktypes =
        [ `Alternate | `Appendix | `Bookmark | `Chapter | `Contents
        | `Copyright | `Glossary | `Help | `Index | `Next | `Prev
        | `Section | `Start | `Stylesheet | `Subsection] list
(** Authors may use the following recognized link types, listed here with
    their conventional interpretations. A LinkTypes value refers to a
    space-separated list of link types. White space characters are not
    permitted within link types.  These link types are case-insensitive, i.e.,
    ["Alternate"] has the same meaning as ["alternate"]. 

    User agents, search engines, etc. may interpret these link types in a
    variety of ways. For example, user agents may provide access to linked
    documents through a navigation bar.

    {ul
      {- [`Alternate]:
         Designates substitute versions for the document in which the link occurs.
         When used together with the hreflang attribute, it implies a translated
         version of the document. When used together with the media attribute,
         it implies a version designed for a different medium (or media).}
      {- [`Stylesheet]:
         Refers to an external style sheet. See the Style Module for details.
         This is used together with the link type ["Alternate"] for user-selectable
         alternate style sheets.}
      {- [`Start]:
         Refers to the first document in a collection of documents.
         This link type tells search engines which document is considered
         by the author to be the starting point of the collection.}
      {- [`Next]:
         Refers to the next document in a linear sequence of documents.
         User agents may choose to pre-load the "next" document, to reduce
         the perceived load time.}
      {- [`Prev]:
         Refers to the previous document in an ordered series of documents.
         Some user agents also support the synonym "Previous".}
      {- [`Contents]:
         Refers to a document serving as a table of contents. Some user
         agents also support the synonym ToC (from "Table of Contents").}
      {- [`Index]:
         Refers to a document providing an index for the current document.}
      {- [`Glossary]:
         Refers to a document providing a glossary of terms that pertain to
         the current document.}
      {- [`Copyright]:
         Refers to a copyright statement for the current document.}
      {- [`Chapter]:
         Refers to a document serving as a chapter in a collection of documents.}
      {- [`Section]:
         Refers to a document serving as a section in a collection of documents.}
      {- [`Subsection]:
         Refers to a document serving as a subsection in a collection of documents.}
      {- [`Appendix]:
         Refers to a document serving as an appendix in a collection of documents.}
      {- [`Help]:
         Refers to a document offering help (more information, links to other
         sources information, etc.)}
      {- [`Bookmark]:
         Refers to a bookmark. A bookmark is a link to a key entry point within
         an extended document. The title attribute may be used, for example, to
         label the bookmark. Note that several bookmarks may be defined in each
         document.}} *)

    type mediadesc =
        [ `All | `Aural | `Braille | `Handheld | `Print
        | `Projection | `Screen | `TTY | `TV ] list
(** The MediaDesc attribute is a comma-separated list of media descriptors.
    The following is a list of recognized media descriptors:
    {ul
      {- [`Screen]:
         Intended for non-paged computer screens.}
      {- [`TTY]:
         Intended for media using a fixed-pitch character grid, such as
         teletypes, terminals, or portable devices with limited display
         capabilities.}
      {- [`TV]:
         Intended for television-type devices (low resolution, color,
         limited scrollability).}
      {- [`Projection]:
         Intended for projectors.}
      {- [`Handheld]:
         Intended for handheld devices (small screen, monochrome,
         bitmapped graphics, limited bandwidth).}
      {- [`Print]:
         Intended for paged, opaque material and for documents viewed
         on screen in print preview mode.}
      {- [`Braille]:
         Intended for braille tactile feedback devices.}
      {- [`Aural]:
         Intended for speech synthesizers.}
      {- [`All]:
         Suitable for all devices.}}

    Future versions of XHTML may introduce new values and may allow
    parameterized values. To facilitate the introduction of these
    extensions, conforming user agents must be able to parse the media
    attribute value as follows:
    {ol
      {- The value is a comma-separated list of entries. For example,
         [media="screen, 3d-glasses, print and resolution > 90dpi"]
         is mapped to: ["screen"], ["3d-glasses"],
         ["print and resolution > 90dpi"].}
      {- Each entry is truncated just before the first character that
         isn't a US ASCII letter [\[a-zA-Z\]] (ISO 10646 hex 41-5a,
         61-7a), digit [\[0-9\]] (hex 30-39), or hyphen-minus (hex 2d).
         In the example, this gives: ["screen"], ["3d-glasses"], ["print"].}
      {- A case-insensitive match is then made with the set of media
         types defined above. User agents may ignore entries that
         don't match.  In the example we are left with ["screen"] and
         ["print"].}}

    Note. Style sheets may include media-dependent variations within them
    (e.g., the [CSS \@media] construct). In such cases it may be appropriate
    to use ["media=all"]. *)

    type multilength = [ length | `Relative of int ]
(** The value may be a Length or a relative length. A relative length
    has the form ["i*"], where ["i"] is an integer. When allotting space
    among elements competing for that space, user agents allot pixel
    and percentage lengths first, then divide up remaining available
    space among relative lengths. Each relative length receives a
    portion of the available space that is proportional to the integer
    preceding the ["*"]. The value ["*"] is equivalent to ["1*"]. Thus, if
    60 pixels of space are available after the user agent allots pixel
    and percentage space, and the competing relative lengths are ["1*"],
    ["2*"], and ["3*"], the ["1*"] will be allotted 10 pixels, the ["2*"] will be
    allotted 20 pixels, and the ["3*"] will be allotted 30 pixels. *)

    type multilengths = multilength list (* comma-separated *)
(** A comma separated list of items of type MultiLength. *)

    type number = int
(** One or more digits. *)

    type pixels = int

(** The value is an integer that represents the number of pixels of
    the canvas (screen, paper). Thus, the value ["50"] means fifty
    pixels. For normative information about the definition of a pixel,
    please consult CSS2.
    @see <http://www.w3.org/TR/1998/REC-CSS2-19980512> CSS2 *)

    type script = string
(** Script data can be the content of the ["script"] element and the
    value of intrinsic event attributes. User agents must not evaluate
    script data as HTML markup but instead must pass it on as data to a
    script engine.

    The case-sensitivity of script data depends on the scripting
    language.

    Please note that script data that is element content may not
    contain character references, but script data that is the value of
    an attribute may contain them. *)

    type shape = string
(** The shape of a region. *)

    type text = string
(** Arbitrary textual data, likely meant to be human-readable. *)

    type uri = string
(** A Uniform Resource Identifier, as per RFC2396.
    @see <http://www.ietf.org/rfc/rfc2396.txt> RFC2396 *)

    type uris = uri
(** A space-separated list of Uniform Resource Identifiers, as per RFC2396.
    @see <http://www.ietf.org/rfc/rfc2396.txt> RFC2396 *)


(** {1 Common Attributes} *)

    type 'a attrib
    type 'a attribs
    (** ['a] is known as a {i phantom type}.  The implementation is
       actually monomorphic (the different element types are distinguished
       by a homogeneous variable, such as their textual representation)
       and the type variable [`a] is just used by the type checker.
    
       NB: It might be possible to use polymorphic variants directly, without
       phantom types, but the implementation is likely to be more involved. *)
    
(** {2 Core} *)

    type core = [ `Class | `Id | `Title | `Color | `Aux ]

    val a_class : nmtokens -> [>`Class] attrib
(** This attribute assigns a class name or set of class names to an
    element. Any number of elements may be assigned the same class
    name or names.  *)

    val a_id : id -> [>`Id] attrib
(** This attribute assigns a name to an element. This name must be
    unique in a document. *)

    val a_title : cdata -> [>`Title] attrib
(** This attribute offers advisory information about the element for
    which it is set. *)

    val a_color : color -> [>`Color] attrib

    val a_style : string -> [>`Aux] attrib

(** Values of the title attribute may be rendered by user agents in a
    variety of ways. For instance, visual browsers frequently display
    the title as a {i tool tip} (a short message that appears when the
    pointing device pauses over an object). Audio user agents may
    speak the title information in a similar context.  *)

(** The title attribute has an additional role when used with the [link]
    element to designate an external style sheet. Please consult the
    section on links and style sheets for details.  *)

(** {2 I18N} *)

    type i18n = [ `XML_lang ]
    val a_xml_lang : nmtoken -> [>`XML_lang] attrib
    
(** {2 Style}
    The Style collection is deprecated, because the Style Attribute Module is
    deprecated. *)

    type common = [ core | i18n ]
    
(** {1 Modules, Element Sets and Attributes } *)

(** {2 5.2. Core Modules} *)
    
(** {3 5.2.1. Structure Module} *)
    
    module STRUCTURE :
        sig
          type t = [ `Body | `Head | `Html | `Title ]
        end
    
    val a_profile : uri -> [>`Profile] attrib
    val a_version : cdata -> [>`Version] attrib
    val a_xmlns : [< `W3_org_1999_xhtml ] -> [>`XMLns] attrib
    
(** {3 5.2.2. Text Module} *)
    
    module TEXT :
        sig
          type heading = [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]
          type block = [ `Address | `Blockquote | `Div | `P | `Pre ]
          type inline =
              [ `Abbr | `Acronym | `Br | `Cite | `Code | `Dfn
              | `Em | `Kbd | `Q | `Samp | `Span | `Strong | `Var ]
          type flow = [ heading | block | inline ]
        end
    
    val a_cite : uri -> [>`Cite] attrib
    val a_xml_space : [< `Preserve ] -> [>`XML_space] attrib
    
(** {3 5.2.3. Hypertext Module} *)
    
    module HYPERTEXT :
        sig
          type inline = [ `A ]
          type flow = inline
        end
    
    val a_accesskey : character -> [>`Accesskey] attrib
(** This attribute assigns an access key to an element. An access key
    is a single character from the document character
    set. NB: authors should consider the input method of the
    expected reader when specifying an accesskey. *)

    val a_charset : charset -> [>`Charset] attrib
(** This attribute specifies the character encoding of the resource
    designated by the link. Please consult the section on character
    encodings for more details. *)

    val a_href : uri -> [>`Href] attrib
(** This attribute specifies the location of a Web resource, thus
    defining a link between the current element (the source anchor)
    and the destination anchor defined by this attribute. *)

    val a_hreflang : languagecode -> [>`Hreflang] attrib
(** This attribute specifies the base language of the resource
    designated by href and may only be used when href is specified. *)

    val a_rel : linktypes -> [>`Rel] attrib
(** This attribute describes the relationship from the current document
    to the anchor specified by the href attribute. The value of this attribute
    is a space-separated list of link types. *)

    val a_rev : linktypes -> [>`Rev] attrib
(** This attribute is used to describe a reverse link from the anchor specified
    by the href attribute to the current document. The value of this attribute
    is a space-separated list of link types. *)

    val a_tabindex : number -> [>`Tabindex] attrib
(** This attribute specifies the position of the current element in
    the tabbing order for the current document. This value must be a
    number between 0 and 32767. User agents should ignore leading
    zeros. *)

    val a_type : contenttype -> [>`Type] attrib
(** This attribute gives an advisory hint as to the content type of
    the content available at the link target address. It allows user
    agents to opt to use a fallback mechanism rather than fetch the
    content if they are advised that they will get content in a
    content type they do not support.Authors who use this attribute
    take responsibility to manage the risk that it may become
    inconsistent with the content available at the link target
    address. *)
    
(** {3 5.2.3. List Module} *)
    
    module LIST :
        sig
          type list = [ `Dl | `Ol | `Ul ]
          type t = [ `Dd | `Dt | `Li ]
          type flow = list
        end
    
(** {2 5.3. Applet Module}
    This module is deprecated. Similar functionality
    can be found in the Object Module. *)
    
(** {2 5.4. Text Extension Modules} *)
    
(** {3 5.4.1. Presentation Module} *)
    
    module PRESENTATION :
        sig
          type block = [ `Hr ]
          type inline = [ `B | `Big | `I | `Small | `Sub | `Sup | `Tt ]
        end
    
(** {3 5.4.2. Edit Module} *)
    
(** {3 5.4.3. Bi-directional Text Module} *)
    
(** {2 5.5. Forms Modules} *)
    
(** {3 5.5.1. Basic Forms Module} *)
    
    module FORMS :
        sig
          type t = [ `Option ]
          type form = [ `Form ]
          type formctrl_sans_label = [ `Input | `Select | `Textarea ]
          type formctrl = [ formctrl_sans_label | `Label ]
          type block = form
          type inline_sans_label = formctrl_sans_label
          type inline = formctrl
          type flow_sans_label = [block | inline_sans_label ]
          type flow = [ block | inline ]
        end
    
    val a_action : uri -> [>`Action] attrib
(** This attribute specifies a form processing agent. User agent
    behavior for a value other than an HTTP URI is undefined. *)

    val a_checked : [< `Checked ] -> [>`Checked] attrib
(** When the [type] attribute has the value ["radio"] or ["checkbox"],
    this boolean attribute specifies that the button is on. User
    agents must ignore this attribute for other control types. *)

    val a_cols : number -> [>`Cols] attrib
(** This attribute specifies the visible width in average character
    widths. Users should be able to enter longer lines than this, so
    user agents should provide some means to scroll through the
    contents of the control when the contents extend beyond the
    visible area. User agents may wrap visible text lines to keep long
    lines visible without the need for scrolling. *)

    val a_enctype : contenttype -> [>`Enctype] attrib
    val a_for : idref -> [>`For] attrib
    val a_maxlength : number -> [>`Maxlength] attrib
    val a_method : [< `Get | `Post ] -> [>`Method] attrib
    val a_multiple : [< `Multiple ] -> [>`Multiple] attrib

    val a_name : cdata -> [>`Name] attrib
(** This attribute assigns the control name. *)

    val a_rows : number -> [>`Rows] attrib
(** This attribute specifies the number of visible text lines. Users
    should be able to enter more lines than this, so user agents
    should provide some means to scroll through the contents of the
    control when the contents extend beyond the visible area. *)

    val a_selected : [< `Selected ] -> [>`Selected] attrib
(** When set, this boolean attribute specifies that this option is pre-selected. *)

    val a_size : number -> [>`Size] attrib
    val a_src : uri -> [>`Src] attrib
    val a_input_type :
        [< `Text | `Password | `Checkbox | `Radio | `Submit | `Reset | `Hidden ] ->
          [>`Input_Type] attrib

    val a_value : cdata -> [>`Value] attrib
(** This attribute specifies the initial value of the control. If this
    attribute is not set, the initial value is set to the contents of
    the [option] element. *)


(** {3 5.5.2. Forms Module} *)
    
(** {2 5.6. Table Modules} *)
    
(** {3 5.6.1. Basic Tables Module} *)
    
    module TABLES :
        sig
          type t = [ `Caption | `Td | `Th | `Tr ]
          type block = [ `Table ]
          type flow = block
        end
    
    val a_abbr : text -> [>`Abbr] attrib
    val a_align : [< `Left | `Center | `Right | `Justify | `Char ] ->
      [>`Align] attrib
    val a_axis : cdata -> [>`Axis] attrib
    val a_colspan : number -> [>`Colspan] attrib
    val a_headers : idrefs -> [>`Headers] attrib
    val a_rowspan : number -> [>`Rowspan] attrib
    val a_scope : [< `Row | `Col | `Rowgroup | `Colgroup ] -> [>`Scope] attrib
    val a_summary : text -> [>`Summary] attrib
    val a_valign : [< `Top | `Middle | `Bottom | `Baseline ] ->
      [>`Valign] attrib
    
(** {3 5.6.2. Tables Module} *)
    
    val a_border : pixels -> [>`Border] attrib
    val a_cellpadding : length -> [>`Cellpadding] attrib
    val a_cellspacing : length -> [>`Cellspacing] attrib
    val a_datapagesize : cdata -> [>`Datapagesize] attrib
    val a_frame :
        [< `Void | `Above | `Below | `Hsides | `LHS | `RHS
        | `Vsides | `Box | `Border ] -> [>`Frame] attrib
    val a_rules : [< `None | `Groups | `Rows | `Cols | `All ] -> [>`Rules] attrib
    val a_char : character -> [>`Char] attrib
    val a_charoff : length -> [>`Charoff] attrib

(** {2 5.7. Image Module} *)
    
    module IMAGE :
        sig
          type inline = [ `Img ]
        end
    
    val a_alt : text -> [>`Alt] attrib
    val a_height : length -> [>`Height] attrib
    val a_longdesc : uri -> [>`Longdesc] attrib
    val a_width : length -> [>`Width] attrib

(** {2 5.8. Client-side Image Map Module} *)
    
(** {2 5.9. Server-side Image Map Module} *)
    
(** {2 5.10. Object Module} *)
    
(** {2 5.11. Frames Module} *)

    val a_fs_rows : multilengths -> [>`FS_Rows] attrib
    val a_fs_cols : multilengths -> [>`FS_Cols] attrib
    val a_frameborder : [< `Zero | `One ] -> [>`Frameborder] attrib
    val a_marginheight : pixels -> [>`Marginheight] attrib
    val a_marginwidth : pixels -> [>`Marginwidth] attrib
    val a_noresize : [< `Noresize ] -> [>`Noresize] attrib
    val a_scrolling : [< `Yes | `No | `Auto ] -> [>`Scrolling] attrib

(** {2 5.12. Target Module} *)

    val a_target : frametarget -> [>`Target] attrib

(** {2 5.13. Iframe Module} *)
    
(** {2 5.14. Intrinsic Events Module} *)
    
(** {2 5.15. Metainformation Module} *)
    
    module METAINFORMATION :
        sig
          type t = [ `Meta ]
        end

    val a_content : cdata -> [>`Content] attrib
    val a_http_equiv : nmtoken -> [>`Http_equiv] attrib
    val a_scheme : cdata -> [>`Scheme] attrib

(** {2 5.16. Scripting Module} *)
    
(** {2 5.17. Style Sheet Module} *)
    
    module STYLE_SHEET :
        sig
          type t = [ `Style ]
        end

    val a_media : mediadesc -> [>`Media] attrib

(** {2 5.18. Style Attribute Module} *)
    
(** {2 5.19. Link Module} *)

    module LINK :
        sig
          type t = [ `Link ]
        end

(** {2 5.20. Base Module} *)
    
    module BASE :
        sig
          type t = [ `Base ]
        end
    
(** {2 5.21. Name Identification Module}
    This module is deprecated in XHTML 1.1, but supported for XHTML 1.0
    using [`Name_01_00] . *)
    
(** {2 5.22. Legacy Module} *)
    
(** {1 Combined Element Sets:} *)
    
    type block =
        [ TEXT.block | PRESENTATION.block | FORMS.block | TABLES.block ]
    type block_sans_form =
        [ TEXT.block | PRESENTATION.block | TABLES.block ]
    
    type flow =
        [ TEXT.flow | HYPERTEXT.flow | LIST.flow | FORMS.flow | TABLES.flow ]
    type flow_sans_table =
        [ TEXT.flow | HYPERTEXT.flow | LIST.flow | FORMS.flow ]
    
    type inline =
        [ TEXT.inline | HYPERTEXT.inline | PRESENTATION.inline
        | FORMS.inline | IMAGE.inline]
    type inline_sans_a =
        [ TEXT.inline | PRESENTATION.inline
        | FORMS.inline | IMAGE.inline]
    type inline_sans_label =
        [ TEXT.inline | HYPERTEXT.inline | PRESENTATION.inline
        | FORMS.inline_sans_label | IMAGE.inline]
    
    type heading = TEXT.heading
    
(** {1 Elements} *)
    
    type 'a elt
    
(** {2 Element Constructor Types} *)

    type ('a, 'b) nullary = ?a:('a attrib list) -> unit -> 'b elt
    type ('a, 'b, 'c) unary = ?a:('a attrib list) -> 'b elt -> 'c elt
    type ('a, 'b, 'c, 'd) binary = ?a:('a attrib list) -> 'b elt -> 'c elt -> 'd elt
    
    type ('a, 'b, 'c) star = ?a:('a attrib list) -> 'b elt list -> 'c elt
(** Star '*' denotes any number of children, uncluding zero. *)

    type ('a, 'b, 'c) plus = ?a:('a attrib list) -> 'b elt -> 'b elt list -> 'c elt
(** Plus '+' requires at least one child.  *)
    
(** {2 Structure} *)
    
    type html = [`Html] elt
    
    val html : ?a:([< i18n | `Version | `XMLns ] attrib list) ->
      [< `Head ] elt -> [< `Body | `Frameset ] elt -> html
    val head : ([< i18n | `Profile ],
                [< `Title | `Meta | `Link | `Style | `Base ], [>`Head]) plus
    val title : ([< i18n ], [< `PCDATA ], [>`Title]) unary
    val body : ([< common ], [< heading | block | LIST.list ], [>`Body]) star
    
(** {2 Data} *)
    
    val pcdata : string -> [>`PCDATA] elt
    val entity : string -> [>`PCDATA] elt
    val space : unit -> [>`PCDATA] elt
    
(** {2 Text} *)
    
    val h1 : ([< common ], [< `PCDATA | inline ], [>`H1]) star
    val h2 : ([< common ], [< `PCDATA | inline ], [>`H2]) star
    val h3 : ([< common ], [< `PCDATA | inline ], [>`H3]) star
    val h4 : ([< common ], [< `PCDATA | inline ], [>`H4]) star
    val h5 : ([< common ], [< `PCDATA | inline ], [>`H5]) star
    val h6 : ([< common ], [< `PCDATA | inline ], [>`H6]) star
    
    val address : ([< common ], [< `PCDATA | inline ], [>`Address]) star
    val blockquote : ([< common | `Cite ],
                      [< `PCDATA | heading | block | LIST.list ], [>`Blockquote]) star
    val div : ([< common ], [< `PCDATA | flow ], [>`Div]) star
    val p : ([< common ], [< `PCDATA | inline ], [>`P]) star
    val pre : ([< common | `XML_space ], [< `PCDATA | inline ], [>`Pre]) star
    
    val abbr : ([< common ], [< `PCDATA | inline ], [>`Abbr]) star
    val acronym : ([< common ], [< `PCDATA | inline ], [>`Acronym]) star
    val br :  ([< core ], [>`Br]) nullary
    val cite : ([< common ], [< `PCDATA | inline ], [>`Cite]) star
    val code : ([< common ], [< `PCDATA | inline ], [>`Code]) star
    val dfn : ([< common ], [< `PCDATA | inline ], [>`Dfn]) star
    val em : ([< common ], [< `PCDATA | inline ], [>`Em]) star
    val kbd : ([< common ], [< `PCDATA | inline ], [>`Kbd]) star
    val q : ([< common | `Cite ], [< `PCDATA | inline ], [>`Q]) star
    val samp : ([< common ], [< `PCDATA | inline ], [>`Samp]) star
    val span : ([< common ], [< `PCDATA | inline ], [>`Span]) star
    val strong : ([< common ], [< `PCDATA | inline ], [>`Strong]) star

    val magic_tag : 
      ([< common | `XML_space ], [< `PCDATA | inline ], [>`Pre]) star
    
(** {2 Hypertext} *)
    
    val a : ([< common | `Accesskey | `Charset | `Href | `Hreflang
             | `Name_01_00 | `Rel | `Rev | `Tabindex | `Target | `Type ],
             [< `PCDATA | inline_sans_a ], [>`A]) star
    
(** {2 List} *)
    
    val dl : ([< common ], [< `Dt | `Dd ], [>`Dl]) plus
    val ol : ([< common ], [< `Li ], [>`Ol]) plus
    val ul : ([< common ], [< `Li ], [>`Ul]) plus
    val dd : ([< common ], [< `PCDATA | flow ], [>`Dd]) star
    val dt : ([< common ], [< `PCDATA | inline ], [>`Dt]) star
    val li : ([< common ], [< `PCDATA | flow ], [>`Li]) star
    
(** {2 Presentation} *)
    
    val hr : ([< common ], [>`Hr]) nullary
    val b : ([< common ], [< `PCDATA | inline ], [>`B]) star
    val big : ([< common ], [< `PCDATA | inline ], [>`Big]) star
    val i : ([< common ], [< `PCDATA | inline ], [>`I]) star
    val small : ([< common ], [< `PCDATA | inline ], [>`Small]) star
    val sub : ([< common ], [< `PCDATA | inline ], [>`Sub]) star
    val sup : ([< common ], [< `PCDATA | inline ], [>`Sup]) star
    val tt : ([< common ], [< `PCDATA | inline ], [>`Tt]) star
    
(** {2 Forms} *)
    
(** {3 Basic Forms} *)
    
(** One can use [open Basic_Forms] to enable basic forms. *)

    module Basic_Forms :
        sig
          val form : action:uri ->
            ([< common | `Enctype | `Method | `Name_01_00 | `Target ],
             [< `PCDATA | heading | LIST.list | block_sans_form ], [>`Form]) star
          val input : ([< common | `Accesskey | `Checked | `Maxlength | `Name | `Size
                       | `Src | `Tabindex | `Input_Type | `Value ], [>`Input]) nullary
          val label : ([< common | `Accesskey | `For ],
                       [< `PCDATA | inline_sans_label ], [>`Label]) star
          val option : ([< common | `Selected | `Value ],
                        [< `PCDATA ], [>`Option]) unary
          val select : ([< common | `Multiple | `Name | `Size | `Tabindex ],
                        [< `Option ], [>`Select]) plus
          val textarea : rows:number -> cols:number ->
            ([< common | `Accesskey | `Name | `Tabindex ],
             [< `PCDATA ], [>`Textarea]) unary
        end

(** {3 Forms} *)
    
(** General forms are not implemented yet, but one can use [open Basic_Forms]
    to enable basic forms. *) 

(** {2 Tables} *)
    
(** {3 Basic Tables} *)

(** One can use [open Basic_Tables] to switch globally to basic tables. *)

    module Basic_Tables :
      sig
        val a_align : [< `Left | `Center | `Right ] -> [>`Align] attrib
        val a_scope : [< `Row | `Col ] -> [>`Scope] attrib
        val a_valign : [< `Top | `Middle | `Bottom ] -> [>`Valign] attrib

        val caption : ([< common ], [< `PCDATA | inline ], [>`Caption]) star
        val table : ?caption:([< `Caption ] elt) ->
            ([< common | `Summary | `Width ], [< `Tr ], [>`Table]) plus
        val td : ([< common | `Abbr | `Align | `Axis | `Colspan | `Headers | `Rowspan
                  | `Scope | `Valign ], [< `PCDATA | flow_sans_table ], [>`Td]) star
        val th : ([< common |  `Abbr | `Align | `Axis | `Colspan | `Headers | `Rowspan
                  | `Scope | `Valign ], [< `PCDATA | flow_sans_table ], [>`Th]) star
        val tr : ([< common | `Align | `Valign ], [< `Td | `Th ], [>`Tr]) plus
      end

(** {3 Tables} *)
    
    val caption : ([< common ], [< `PCDATA | inline ], [>`Caption]) star

    val table : ?caption:([< `Caption ] elt) ->
      ?columns:([< `Cols of ([< `Col ] elt list)
                | `Colgroups of ([< `Colgroup ] elt list) ]) ->
        ([< common | `Border | `Cellpadding | `Cellspacing | `Datapagesize
         | `Frame | `Rules | `Summary | `Width ], [< `Tr ], [>`Table]) plus

    val tablex : ?caption:([< `Caption ] elt) ->
      ?columns:([< `Cols of ([< `Col ] elt list)
                | `Colgroups of ([< `Colgroup ] elt list) ]) ->
        ?thead:([< `Thead ] elt) -> ?tfoot:([< `Tfoot ] elt) ->
          ([< common | `Border | `Cellpadding | `Cellspacing | `Datapagesize
           | `Frame | `Rules | `Summary | `Width ], [< `Tbody ], [>`Table]) plus

    val td : ([< common |  `Abbr | `Align | `Axis | `Char | `Charoff
              | `Colspan | `Headers | `Rowspan | `Scope | `Valign ],
              [< `PCDATA | flow ], [>`Td]) star
    val th : ([< common |  `Abbr | `Align | `Axis | `Char | `Charoff
              | `Colspan | `Headers | `Rowspan | `Scope | `Valign ],
              [< `PCDATA | flow ], [>`Th]) star
    val tr : ([< common | `Align | `Char | `Charoff | `Valign ],
              [< `Td | `Th ], [>`Tr]) plus

    val col : ([< common | `Align | `Char | `Charoff
               | `Span | `Valign | `Width ], [>`Col]) nullary
    val colgroup : ([< common | `Align | `Char | `Charoff
                    | `Span | `Valign | `Width ], [< `Col ], [>`Colgroup]) star
    val thead : ([< common | `Align | `Char | `Charoff | `Valign ],
                 [< `Tr ], [>`Thead]) plus
    val tbody : ([< common | `Align | `Char | `Charoff | `Valign ],
                 [< `Tr ], [>`Tbody]) plus
    val tfoot : ([< common | `Align | `Char | `Charoff | `Valign ],
                 [< `Tr ], [>`Tfoot]) plus

(** {2 Image} *)
    
    val img : src:uri -> alt:text ->
      ([< common | `Height | `Longdesc | `Name_01_00 | `Width ], [>`Img]) nullary

(** {2 Frames} *)

    val frameset : ?noframes:([< `Noframes ] elt) ->
      ([< core | `FS_Rows | `FS_Cols ], [< `Frameset | `Frame ], [>`Frameset]) plus

    val frame : src:uri ->
      ([< core | `Frameborder | `Longdesc | `Marginheight | `Marginwidth
       | `Name_01_00 | `Noresize | `Scrolling ], [>`Frame]) nullary

    val noframes : ([< common ], [< `Body ], [>`Noframes]) unary

(** {2 Meta} *)

    val meta : content:cdata ->
      ([< i18n | `Http_equiv | `Name | `Scheme ], [>`Meta]) nullary

(** {2 Style Sheets} *)

    val style : contenttype:contenttype ->
      ([< i18n | `Media | `Title | `XML_space ], [< `PCDATA ], [>`Style]) star

(** {2 Link} *)

    val link : ([< common | `Charset | `Href | `Hreflang | `Media
                | `Rel | `Rev | `Target | `Type ], [>`Link]) nullary
    
(** {2 Base} *)

    val base : href:uri -> unit -> [>`Base] elt

(** {1 Output} *)
    
(** [?encode] maps strings to HTML and {e must} encode the unsafe characters
    ['<'], ['>'], ['"'], ['&'] and the control characters 0-8, 11-12, 14-31, 127
    to HTML entities.  [XML.encode_unsafe] is the default for [?encode] in [output]
    and [pretty_print] below.  Other implementations are provided by the module
    [Netencoding] in the
    {{:http://www.ocaml-programming.de/programming/ocamlnet.html}OcamlNet} library, e.g.:
    [let encode = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ~out_enc:`Enc_usascii ()],
    Where national characters are replaced by HTML entities.
    The user is of course free to write her own implementation.
    @see <http://www.ocaml-programming.de/programming/ocamlnet.html> OcamlNet *)

(** [~encoding] is the official name of the external character set encoding that
    is used by [outs : string -> unit]. *)

    val output : ?encode:(string -> string) -> ?encoding:string -> 
      (string -> unit) -> html -> unit

    val pretty_print : ?width:int ->
      ?encode:(string -> string) -> ?encoding:string ->
	(string -> unit) -> html -> unit
(*
    val pretty_print_elt : ?width:int ->
      ?encode:(string -> string) -> (string -> unit) -> 'a elt -> unit
*)  
(** {1 Tools} *)

    val version : string
    val standard : uri
    val validator : uri
    val validator_icon : unit -> [>`A] elt
(** A hyperlink to the W3C validator, including the logo.
    @see <http://validator.w3.org> Validator *)

    val addto_class : string -> 'a elt -> 'a elt
(** Add the element and all its subelements to a class.  Note that this
   is only almost typesafe, because a few elements from the structure class
   do not support the class attribute.   On the other hand, listing all
   allowed elements would be too tedious right now.  *)

    val addto_class1 : string -> 'a elt -> 'a elt
(** Add the element to a class. *)

    val set_rowspan : int -> ([< `Th | `Td ] as 'a) elt -> 'a elt
(** Set the rowspan attribute for the element. *)

    val rewrite_hrefs : (string -> string) -> 'a elt -> 'a elt
    val all_hrefs : 'a elt -> uri list
    val all_anchors : 'a elt -> id list

(*
    val amap : (string -> 'a attribs -> 'a attribs) -> 'b elt -> 'b elt
    val amap1 : (string -> 'a attribs -> 'a attribs) -> 'b elt -> 'b elt

    val rm_attrib : (string -> bool) -> 'a attribs -> 'a attribs
    val rm_attrib_from_list :
	(string -> bool) -> (string -> bool) -> 'a attribs -> 'a attribs

(** Exporting the following will drive a hole through the type system,
   because they allow to add any attribute to any element. *)
    val add_int_attrib : string -> int -> 'a attribs -> 'a attribs
    val add_string_attrib : string -> string -> 'a attribs -> 'a attribs
    val add_comma_sep_attrib : string -> string -> 'a attribs -> 'a attribs
    val add_space_sep_attrib : string -> string -> 'a attribs -> 'a attribs
*)

  end

(** An alias for XHTML 1.1 (for symmetry):
    @see <http://www.w3.org/TR/xhtml11/> XHTML 1.1 - Module-based XHTML *)
module type T_01_01 = T

(** XHTML 1.0 includes some deprecated features that since
    have been removed from XHTML 1.1:
    @see <http://www.w3.org/TR/xhtml11/changes.html#a_changes> Changes from XHTML 1.0 Strict
    @see <http://www.w3.org/TR/2000/REC-xhtml1-20000126/> XHTML 1.0: The Extensible HyperText Markup Language *)
module type T_01_00 =
  sig
    include T

(** XHTML 1.1 has removed the name attribute from several elements: *)
    val a_name_01_00 : cdata -> [>`Name_01_00] attrib
  end


module M : T
module M_01_01 : T_01_01
module M_01_00 : T_01_00

