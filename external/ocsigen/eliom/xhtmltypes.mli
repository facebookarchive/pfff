(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(** XHTML types with variants. (See also {!XHTML.M}) *)

(** The elements, attributes, attribute types and data types are given names
    that match the names in the W3C recommendation as closely as allowed by
    a strict typing discipline and the lexical conventions of O'Caml:
    {ul
      {- {e elements} are implemented as O'Caml consigors with the same name as
         in the W3C recommendation.  The domain and codomain are specified as ['a elt],
         where ['a] is a concrete phantom type build out of polymorphic variants.}
      {- {e attributes} are implemented as O'Caml consigors with [a_] prefixed to the
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
    the corresponding consigor.  *)
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
(** {2 Data Types} *)
type character = char

(** A single character from ISO 10646. *)
type charset = string

(** A character encoding, as per RFC2045 (MIME).
    @see <http://www.ietf.org/rfc/rfc2045.txt> RFC2045 *)
type charsets = charset list

(** A space-separated list of character encodings, as per RFC2045 (MIME).
    @see <http://www.ietf.org/rfc/rfc2045.txt> RFC2045 *)
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
type length = [ | `Pixels of int | `Percent of int ]

(** The value may be either in pixels or a percentage of the available
    horizontal or vertical space. Thus, the value [`Percent 50] means half of
    the available space. *)
type linktypes =
  [
    | `Alternate
    | `Appendix
    | `Bookmark
    | `Chapter
    | `Contents
    | `Copyright
    | `Glossary
    | `Help
    | `Index
    | `Next
    | `Prev
    | `Section
    | `Start
    | `Stylesheet
    | `Subsection
    | `Other of string ] list

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
         document.}
      {- [`Other]:
         refers to any other type (for example [icon] or [shortcut]).
         }} *)
type mediadesc =
  [
    | `All
    | `Aural
    | `Braille
    | `Embossed
    | `Handheld
    | `Print
    | `Projection
    | `Screen
    | `Speech
    | `TTY
    | `TV ] list

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
    (e.g., the [CSS \@media] consig). In such cases it may be appropriate
    to use ["media=all"]. *)
type multilength = [ | length | `Relative of int ]

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
type multilengths = multilength list

(* comma-separated *)
(** A comma separated list of items of type MultiLength. *)
type number = int

(** One or more digits. *)
type pixels = int

(** The value is an integer that represents the number of pixels of
    the canvas (screen, paper). Thus, the value ["50"] means fifty
    pixels. For normative information about the definition of a pixel,
    please consult CSS2.
    @see <http://www.w3.org/TR/1998/REC-CSS2-19980512> CSS2 *)
type text = string

(** Arbitrary textual data, likely meant to be human-readable. *)
(** Attributes *)
type core = [ | `Class | `Id | `Title | `XML_space ]

type i18n = [ | `XML_lang | `Dir ]

type events =
  [
    | `OnClick
    | `OnDblClick
    | `OnMouseDown
    | `OnMouseUp
    | `OnMouseOver
    | `OnMouseMove
    | `OnMouseOut
    | `OnKeyPress
    | `OnKeyDown
    | `OnKeyUp
  ]

type common = [ | core | i18n | events | `Style_Attr ]

 type uri = Uri.uri 
 type uris = Uri.uris 
 val string_of_uri: uri -> string 
 val uri_of_string: string -> uri 
(** A space-separated list of Uniform Resource Identifiers, as per RFC2396.
    @see <http://www.ietf.org/rfc/rfc2396.txt> RFC2396 *)


module HYPERTEXT : 
sig 
  type inline = [ | `A ]
  type flow = inline
end
  
module LIST :
  sig
    type list = [ | `Dl | `Ol | `Ul ]
    
    type t = [ | `Dd | `Dt | `Li ]
    
    type flow = list
    
  end
  
module PRESENTATION :
  sig
    type block = [ | `Hr ]
    
    type inline = [ | `B | `Big | `I | `Small | `Sub | `Sup | `Tt ]
    
    type flow = [ | inline | block ]
    
  end
  
module FORMS :
  sig
    type t = [ | `Option ]
    
    type form = [ | `Form ]
    
    type formctrl_sans_label = [ | `Input | `Select | `Textarea | `Button ]
    
    type formctrl = [ | formctrl_sans_label | `Label ]
    
    type block = form
    
    type inline_sans_label = formctrl_sans_label
    
    type inline = formctrl
    
    type flow_sans_label = [ | block | inline_sans_label ]
    
    type flow = [ | block | inline ]
    
  end
  
module TABLES :
  sig
    type t = [ | `Caption | `Td | `Th | `Tr ]
    
    type block = [ | `Table ]
    
    type flow = block
    
  end
  
  
  
(** {2 5.20. Base Module} *)
  
module TEXT :
  sig
    type heading = [ | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]
    
    type block = [ | `Address | `Blockquote | `Div | `P | `Pre ]
    
    type inline =
      [
        | `Abbr
        | `Acronym
        | `Br
        | `Cite
        | `Code
        | `Dfn
        | `Em
        | `Kbd
        | `Q
        | `Samp
        | `Span
        | `Strong
        | `Var
      ]
    
    type flow = [ | heading | block | inline ]
    
  end
  
type edit = [ | `Ins | `Del ]

type scripttag = [ | `Script | `Noscript ]

type misc = [ | edit | scripttag ]

module SPECIAL :
  sig
    type inline = [ | `Img | `Map | `Object ]
    
    type block = [ | `Table | `Form | `Fieldset ]
    
    type flow = [ | inline | block ]
    
  end
  
type i18nclass = [ | `Bdo ]

module RUBY :
  sig
    type inline = [ | `Ruby_simple1 | `Ruby_simple2 | `Ruby_complex ]
    
    type flow = inline
    
  end
  
type no_ruby_inline =
  [
    | TEXT.
    inline
    | PRESENTATION.
    inline
    | HYPERTEXT.
    inline
    | SPECIAL.
    inline
    | FORMS.
    inline
    | i18nclass
  ]

type no_ruby_content = [ | `PCDATA | no_ruby_inline | misc ]

(** {1 Combined Element Sets:} *)
type block =
  [
    | TEXT.
    block
    | PRESENTATION.
    block
    | FORMS.
    block
    | TABLES.
    block
    | SPECIAL.
    block
    | TEXT.
    heading
    | LIST.
    list
    | misc
  ]

type block_sans_form =
  [
    | TEXT.
    block
    | PRESENTATION.
    block
    | TABLES.
    block
    | TEXT.
    heading
    | LIST.
    list
    | misc
  ]

type flow =
  [
    | TEXT.
    flow
    | HYPERTEXT.
    flow
    | LIST.
    flow
    | FORMS.
    flow
    | TABLES.
    flow
    | PRESENTATION.
    flow
    | SPECIAL.
    flow
    | i18nclass
    | misc
    | RUBY.
    flow
  ]

type flow_sans_table =
  [
    | TEXT.
    flow
    | HYPERTEXT.
    flow
    | LIST.
    flow
    | FORMS.
    flow
    | PRESENTATION.
    flow
    | SPECIAL.
    flow
    | i18nclass
    | misc
    | RUBY.
    flow
  ]

type inline =
  [
    | TEXT.
    inline
    | HYPERTEXT.
    inline
    | PRESENTATION.
    inline
    | FORMS.
    inline
    | SPECIAL.
    inline
    | i18nclass
    | misc
    | RUBY.
    inline
  ]

type inline_sans_a_mix =
  [
    | TEXT.
    inline
    | PRESENTATION.
    inline
    | FORMS.
    inline
    | SPECIAL.
    inline
    | i18nclass
    | misc
    | RUBY.
    inline
  ]

type buttoncontent =
  (* VB *)
  [
    | TEXT.
    inline
    | PRESENTATION.
    inline
    | SPECIAL.
    inline
    | i18nclass
    | block_sans_form
  ]

type precontent = inline

type inline_sans_label =
  [
    | TEXT.
    inline
    | HYPERTEXT.
    inline
    | PRESENTATION.
    inline
    | FORMS.
    inline_sans_label
    | SPECIAL.
    inline
    | i18nclass
    | misc
  ]

type heading = TEXT.heading



type xhtml = [ | `Html ]

type pcdata = [ | `PCDATA ]

type notag

type html = [ | `Html ]

type html_attrib = [ | i18n | `Version | `XMLns | `Id ]

type html_content = [ | `Body | `Head ]

type head = [ | `Head ]

type head_content =
  [ | `Title | `Base | `Meta | `Link | `Style | `Object | `Script
  ]

type head_attrib = [ | i18n | `Profile | `Id ]

type table = [ `Table ]
type table_attrib = [ common | `Summary | `Width | `Tr ]
type table_content = [ `Tr ]
type inlinemix = [ | `PCDATA | inline ]
(* File autogenerated by xhtml5typesgenerator from xhtml_orig.ml *)
(* You should not edit this file. *)
(* NAME: title - KIND: nary - ATTRIB: [= i18n | `Id ] - CONTENT: [= `PCDATA ] - OUT: [=`Title] *)
type title = [ | `Title ]

type title_attrib = [ | i18n | `Id ]

type title_content = [ | `PCDATA ]

(* NAME: body - KIND: star - ATTRIB: [= common |`OnLoad |`OnUnload ] - CONTENT: [= block ] - OUT: [=`Body] *)
type body = [ | `Body ]

type body_attrib = [ | common | `OnLoad | `OnUnload ]

type body_content = [ | block ]

(* NAME: h1 - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`H1] *)
type h1 = [ | `H1 ]

type h1_attrib = [ | common ]

type h1_content = [ | `PCDATA | inline ]

(* NAME: h2 - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`H2] *)
type h2 = [ | `H2 ]

type h2_attrib = [ | common ]

type h2_content = [ | `PCDATA | inline ]

(* NAME: h3 - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`H3] *)
type h3 = [ | `H3 ]

type h3_attrib = [ | common ]

type h3_content = [ | `PCDATA | inline ]

(* NAME: h4 - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`H4] *)
type h4 = [ | `H4 ]

type h4_attrib = [ | common ]

type h4_content = [ | `PCDATA | inline ]

(* NAME: h5 - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`H5] *)
type h5 = [ | `H5 ]

type h5_attrib = [ | common ]

type h5_content = [ | `PCDATA | inline ]

(* NAME: h6 - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`H6] *)
type h6 = [ | `H6 ]

type h6_attrib = [ | common ]

type h6_content = [ | `PCDATA | inline ]

(* NAME: address - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Address] *)
type address = [ | `Address ]

type address_attrib = [ | common ]

type address_content = [ | `PCDATA | inline ]

(* NAME: div - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | flow ] - OUT: [=`Div] *)
type div = [ | `Div ]

type div_attrib = [ | common ]

type div_content = [ | `PCDATA | flow ]

(* NAME: p - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`P] *)
type p = [ | `P ]

type p_attrib = [ | common ]

type p_content = [ | `PCDATA | inline ]

(* NAME: abbr - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Abbr] *)
type abbr = [ | `Abbr ]

type abbr_attrib = [ | common ]

type abbr_content = [ | `PCDATA | inline ]

(* NAME: acronym - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Acronym] *)
type acronym = [ | `Acronym ]

type acronym_attrib = [ | common ]

type acronym_content = [ | `PCDATA | inline ]

(* NAME: br - KIND: nullary - ATTRIB: [= core ] - CONTENT: notag - OUT: [=`Br] *)
type br = [ | `Br ]

type br_attrib = [ | core ]

type br_content = notag

(* NAME: cite - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Cite] *)
type cite = [ | `Cite ]

type cite_attrib = [ | common ]

type cite_content = [ | `PCDATA | inline ]

(* NAME: code - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Code] *)
type code = [ | `Code ]

type code_attrib = [ | common ]

type code_content = [ | `PCDATA | inline ]

(* NAME: dfn - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Dfn] *)
type dfn = [ | `Dfn ]

type dfn_attrib = [ | common ]

type dfn_content = [ | `PCDATA | inline ]

(* NAME: em - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Em] *)
type em = [ | `Em ]

type em_attrib = [ | common ]

type em_content = [ | `PCDATA | inline ]

(* NAME: kbd - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Kbd] *)
type kbd = [ | `Kbd ]

type kbd_attrib = [ | common ]

type kbd_content = [ | `PCDATA | inline ]

(* NAME: q - KIND: star - ATTRIB: [= common | `Cite ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Q] *)
type q = [ | `Q ]

type q_attrib = [ | common | `Cite ]

type q_content = [ | `PCDATA | inline ]

(* NAME: samp - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Samp] *)
type samp = [ | `Samp ]

type samp_attrib = [ | common ]

type samp_content = [ | `PCDATA | inline ]

(* NAME: span - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Span] *)
type span = [ | `Span ]

type span_attrib = [ | common ]

type span_content = [ | `PCDATA | inline ]

(* NAME: strong - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Strong] *)
type strong = [ | `Strong ]

type strong_attrib = [ | common ]

type strong_content = [ | `PCDATA | inline ]

(* NAME: var - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Var] *)
type var = [ | `Var ]

type var_attrib = [ | common ]

type var_content = [ | `PCDATA | inline ]

(* NAME: a - KIND: star - ATTRIB: [= common | `Accesskey | `Charset | `Href | `Hreflang | `Name_01_00 | `Rel | `Rev | `Tabindex | `Target | `Type | `Shape | `Coords | `OnBlur |`OnFocus] - CONTENT: [= `PCDATA | inline_sans_a_mix ] - OUT: [=`A] *)
type a = [ | `A ]

type a_attrib =
  [
    | common
    | `Accesskey
    | `Charset
    | `Href
    | `Hreflang
    | `Name_01_00
    | `Rel
    | `Rev
    | `Tabindex
    | `Target
    | `Type
    | `Shape
    | `Coords
    | `OnBlur
    | `OnFocus
  ]

type a_content = [ | `PCDATA | inline_sans_a_mix ]

(* NAME: dl - KIND: plus - ATTRIB: [= common ] - CONTENT: [= `Dt | `Dd ] - OUT: [=`Dl] *)
type dl = [ | `Dl ]

type dl_attrib = [ | common ]

type dl_content = [ | `Dt | `Dd ]

(* NAME: ol - KIND: plus - ATTRIB: [= common ] - CONTENT: [= `Li ] - OUT: [=`Ol] *)
type ol = [ | `Ol ]

type ol_attrib = [ | common ]

type ol_content = [ | `Li ]

(* NAME: ul - KIND: plus - ATTRIB: [= common ] - CONTENT: [= `Li ] - OUT: [=`Ul] *)
type ul = [ | `Ul ]

type ul_attrib = [ | common ]

type ul_content = [ | `Li ]

(* NAME: dd - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | flow ] - OUT: [=`Dd] *)
type dd = [ | `Dd ]

type dd_attrib = [ | common ]

type dd_content = [ | `PCDATA | flow ]

(* NAME: dt - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Dt] *)
type dt = [ | `Dt ]

type dt_attrib = [ | common ]

type dt_content = [ | `PCDATA | inline ]

(* NAME: li - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | flow ] - OUT: [=`Li] *)
type li = [ | `Li ]

type li_attrib = [ | common ]

type li_content = [ | `PCDATA | flow ]

(* NAME: hr - KIND: nullary - ATTRIB: [= common ] - CONTENT: notag - OUT: [=`Hr] *)
type hr = [ | `Hr ]

type hr_attrib = [ | common ]

type hr_content = notag

(* NAME: b - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`B] *)
type b = [ | `B ]

type b_attrib = [ | common ]

type b_content = [ | `PCDATA | inline ]

(* NAME: big - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Big] *)
type big = [ | `Big ]

type big_attrib = [ | common ]

type big_content = [ | `PCDATA | inline ]

(* NAME: i - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`I] *)
type i = [ | `I ]

type i_attrib = [ | common ]

type i_content = [ | `PCDATA | inline ]

(* NAME: small - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Small] *)
type small = [ | `Small ]

type small_attrib = [ | common ]

type small_content = [ | `PCDATA | inline ]

(* NAME: sub - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Sub] *)
type sub = [ | `Sub ]

type sub_attrib = [ | common ]

type sub_content = [ | `PCDATA | inline ]

(* NAME: sup - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Sup] *)
type sup = [ | `Sup ]

type sup_attrib = [ | common ]

type sup_content = [ | `PCDATA | inline ]

(* NAME: tt - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Tt] *)
type tt = [ | `Tt ]

type tt_attrib = [ | common ]

type tt_content = [ | `PCDATA | inline ]

(* NAME: bdo - KIND: star - ATTRIB: [= core | `XML_lang ] - CONTENT: [= `PCDATA | inline ] - OUT: [= `Bdo ] *)
type bdo = [ | `Bdo ]

type bdo_attrib = [ | core | `XML_lang ]

type bdo_content = [ | `PCDATA | inline ]

(* NAME: area - KIND: nullary - ATTRIB: [= common | `Href | `Shape | `Coords | `Nohref | `Tabindex | `Accesskey |`OnBlur |`OnFocus] - CONTENT: notag - OUT: [=`Area] *)
type area = [ | `Area ]

type area_attrib =
  [
    | common
    | `Href
    | `Shape
    | `Coords
    | `Nohref
    | `Tabindex
    | `Accesskey
    | `OnBlur
    | `OnFocus
  ]

type area_content = notag

(* NAME: map - KIND: plus - ATTRIB: [= events | core | `XMLns | `Class | `Title | i18n ] - CONTENT: [= block | `Area ] - OUT: [=`Map] *)
type map = [ | `Map ]

type map_attrib = [ | events | core | `XMLns | `Class | `Title | i18n ]

type map_content = [ | block | `Area ]

(* NAME: del - KIND: star - ATTRIB: [= common | `Cite | `Datetime ] - CONTENT: [= `PCDATA | flow ] - OUT: [=`Del] *)
type del = [ | `Del ]

type del_attrib = [ | common | `Cite | `Datetime ]

type del_content = [ | `PCDATA | flow ]

(* NAME: ins - KIND: star - ATTRIB: [= common | `Cite | `Datetime ] - CONTENT: [= `PCDATA | flow ] - OUT: [=`Ins] *)
type ins = [ | `Ins ]

type ins_attrib = [ | common | `Cite | `Datetime ]

type ins_content = [ | `PCDATA | flow ]

(* NAME: script - KIND: nary - ATTRIB: [= `XMLns | `Id | `Charset | `Src | `Defer | `XML_space ] - CONTENT: [= `PCDATA ] - OUT: [=`Script] *)
type script = [ | `Script ]

type script_attrib =
  [ | `XMLns | `Id | `Charset | `Src | `Defer | `XML_space
  ]

type script_content = [ | `PCDATA ]

(* NAME: noscript - KIND: plus - ATTRIB: [= common ] - CONTENT: [= block ] - OUT: [=`Noscript] *)
type noscript = [ | `Noscript ]

type noscript_attrib = [ | common ]

type noscript_content = [ | block ]

(* NAME: form - KIND: plus - ATTRIB: [= common | `Enctype | `Method | `Name_01_00 | `Target | `Accept_charset | `Accept |`OnReset | `OnSubmit] - CONTENT: [= block_sans_form | `Fieldset ] - OUT: [=`Form] *)
type form = [ | `Form ]

type form_attrib =
  [
    | common
    | `Enctype
    | `Method
    | `Name_01_00
    | `Target
    | `Accept_charset
    | `Accept
    | `OnReset
    | `OnSubmit
  ]

type form_content = [ | block_sans_form | `Fieldset ]

(* NAME: input - KIND: nullary - ATTRIB: [= common | `Accesskey | `Checked | `Maxlength | `Name | `Size | `Src | `Tabindex | `Input_Type | `Value | `Disabled | `Readonly | `Alt | `Accept | `Usemap |`Ismap |`OnBlur |`OnChange |`OnFocus | `OnSelect] - CONTENT: notag - OUT: [=`Input] *)
type input = [ | `Input ]

type input_attrib =
  [
    | common
    | `Accesskey
    | `Checked
    | `Maxlength
    | `Name
    | `Size
    | `Src
    | `Tabindex
    | `Input_Type
    | `Value
    | `Disabled
    | `Readonly
    | `Alt
    | `Accept
    | `Usemap
    | `Ismap
    | `OnBlur
    | `OnChange
    | `OnFocus
    | `OnSelect
  ]

type input_content = notag

(* NAME: label - KIND: star - ATTRIB: [= common | `Accesskey | `For |`OnBlur |`OnFocus] - CONTENT: [= `PCDATA | inline_sans_label ] - OUT: [=`Label] *)
type label = [ | `Label ]

type label_attrib = [ | common | `Accesskey | `For | `OnBlur | `OnFocus ]

type label_content = [ | `PCDATA | inline_sans_label ]

(* NAME: optgroup - KIND: plus - ATTRIB: [= common | `Disabled ] - CONTENT: [= `Option ] - OUT: [=`Optgroup] *)
type optgroup = [ | `Optgroup ]

type optgroup_attrib = [ | common | `Disabled ]

type optgroup_content = [ | `Option ]

(* NAME: option - KIND: nary - ATTRIB: [= common | `Selected | `Value | `Disabled | `Label ] - CONTENT: [= `PCDATA ] - OUT: [=`Option] *)
type selectoption = [ | `Option ]

type option_attrib = [ | common | `Selected | `Value | `Disabled | `Label ]

type option_content = [ | `PCDATA ]

(* NAME: select - KIND: plus - ATTRIB: [= common | `Multiple | `Name | `Size | `Tabindex | `Disabled |`OnBlur |`OnChange |`OnFocus ] - CONTENT: [= `Option | `Optgroup ] - OUT: [=`Select] *)
type select = [ | `Select ]

type select_attrib =
  [
    | common
    | `Multiple
    | `Name
    | `Size
    | `Tabindex
    | `Disabled
    | `OnBlur
    | `OnChange
    | `OnFocus
  ]

type select_content = [ | `Option | `Optgroup ]

(* NAME: textarea - KIND: nary - ATTRIB: [= common | `Accesskey | `Name | `Tabindex | `Disabled | `Readonly |`OnBlur |`OnChange |`OnFocus | `OnSelect] - CONTENT: [= `PCDATA ] - OUT: [=`Textarea] *)
type textarea = [ | `Textarea ]

type textarea_attrib =
  [
    | common
    | `Accesskey
    | `Name
    | `Tabindex
    | `Disabled
    | `Readonly
    | `OnBlur
    | `OnChange
    | `OnFocus
    | `OnSelect
  ]

type textarea_content = [ | `PCDATA ]

(* NAME: fieldset - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | `Legend | flow ] - OUT: [=`Fieldset] *)
type fieldset = [ | `Fieldset ]

type fieldset_attrib = [ | common ]

type fieldset_content = [ | `PCDATA | `Legend | flow ]

(* NAME: legend - KIND: star - ATTRIB: [= common | `Accesskey ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Legend] *)
type legend = [ | `Legend ]

type legend_attrib = [ | common | `Accesskey ]

type legend_content = [ | `PCDATA | inline ]

(* NAME: button - KIND: star - ATTRIB: [= common | `Name | `Value | `Button_Type | `Disabled | `Accesskey | `Tabindex |`OnBlur |`OnFocus] - CONTENT: [= `PCDATA | buttoncontent ] - OUT: [=`Button] *)
type button = [ | `Button ]

type button_attrib =
  [
    | common
    | `Name
    | `Value
    | `Button_Type
    | `Disabled
    | `Accesskey
    | `Tabindex
    | `OnBlur
    | `OnFocus
  ]

type button_content = [ | `PCDATA | buttoncontent ]

(* NAME: caption - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA | inline ] - OUT: [=`Caption] *)
type caption = [ | `Caption ]

type caption_attrib = [ | common ]

type caption_content = [ | `PCDATA | inline ]

(* NAME: td - KIND: star - ATTRIB: [= common | `Abbr | `Align | `Axis | `Char | `Charoff | `Colspan | `Headers | `Rowspan | `Scope | `Valign ] - CONTENT: [= `PCDATA | flow ] - OUT: [=`Td] *)
type td = [ | `Td ]

type td_attrib =
  [
    | common
    | `Abbr
    | `Align
    | `Axis
    | `Char
    | `Charoff
    | `Colspan
    | `Headers
    | `Rowspan
    | `Scope
    | `Valign
  ]

type td_content = [ | `PCDATA | flow ]

(* NAME: th - KIND: star - ATTRIB: [= common | `Abbr | `Align | `Axis | `Char | `Charoff | `Colspan | `Headers | `Rowspan | `Scope | `Valign ] - CONTENT: [= `PCDATA | flow ] - OUT: [=`Th] *)
type th = [ | `Th ]

type th_attrib =
  [
    | common
    | `Abbr
    | `Align
    | `Axis
    | `Char
    | `Charoff
    | `Colspan
    | `Headers
    | `Rowspan
    | `Scope
    | `Valign
  ]

type th_content = [ | `PCDATA | flow ]

(* NAME: tr - KIND: plus - ATTRIB: [= common | `Align | `Char | `Charoff | `Valign ] - CONTENT: [= `Td | `Th ] - OUT: [=`Tr] *)
type tr = [ | `Tr ]

type tr_attrib = [ | common | `Align | `Char | `Charoff | `Valign ]

type tr_content = [ | `Td | `Th ]

(* NAME: col - KIND: nullary - ATTRIB: [= common | `Align | `Char | `Charoff | `Span | `Valign | `Width ] - CONTENT: notag - OUT: [=`Col] *)
type col = [ | `Col ]

type col_attrib =
  [ | common | `Align | `Char | `Charoff | `Span | `Valign | `Width
  ]

type col_content = notag

(* NAME: colgroup - KIND: star - ATTRIB: [= common | `Align | `Char | `Charoff | `Span | `Valign | `Width ] - CONTENT: [= `Col ] - OUT: [=`Colgroup] *)
type colgroup = [ | `Colgroup ]

type colgroup_attrib =
  [ | common | `Align | `Char | `Charoff | `Span | `Valign | `Width
  ]

type colgroup_content = [ | `Col ]

(* NAME: thead - KIND: plus - ATTRIB: [= common | `Align | `Char | `Charoff | `Valign ] - CONTENT: [= `Tr ] - OUT: [=`Thead] *)
type thead = [ | `Thead ]

type thead_attrib = [ | common | `Align | `Char | `Charoff | `Valign ]

type thead_content = [ | `Tr ]

(* NAME: tbody - KIND: plus - ATTRIB: [= common | `Align | `Char | `Charoff | `Valign ] - CONTENT: [= `Tr ] - OUT: [=`Tbody] *)
type tbody = [ | `Tbody ]

type tbody_attrib = [ | common | `Align | `Char | `Charoff | `Valign ]

type tbody_content = [ | `Tr ]

(* NAME: tfoot - KIND: plus - ATTRIB: [= common | `Align | `Char | `Charoff | `Valign ] - CONTENT: [= `Tr ] - OUT: [=`Tfoot] *)
type tfoot = [ | `Tfoot ]

type tfoot_attrib = [ | common | `Align | `Char | `Charoff | `Valign ]

type tfoot_content = [ | `Tr ]

(* NAME: img - KIND: nullary - ATTRIB: [= common | `Height | `Longdesc | `Name_01_00 | `Width | `Usemap |`Ismap ] - CONTENT: notag - OUT: [=`Img] *)
type img = [ | `Img ]

type img_attrib =
  [ | common | `Height | `Longdesc | `Name_01_00 | `Width | `Usemap | `Ismap
  ]

type img_content = notag

(* NAME: object_ - KIND: star - ATTRIB: [= common | `Declare | `Classid | `Codebase | `Data | `Type | `Codetype | `Archive | `Standby | `Height | `Width | `Name | `Tabindex | `Usemap] - CONTENT: [= `PCDATA | flow | `Param ] - OUT: [= `Object ] *)
type object_ = [ | `Object ]

type object__attrib =
  [
    | common
    | `Declare
    | `Classid
    | `Codebase
    | `Data
    | `Type
    | `Codetype
    | `Archive
    | `Standby
    | `Height
    | `Width
    | `Name
    | `Tabindex
    | `Usemap
  ]

type object__content = [ | `PCDATA | flow | `Param ]

(* NAME: param - KIND: nullary - ATTRIB: [= `XMLns |`Id | `Value | `Value_Type | `Type ] - CONTENT: notag - OUT: [= `Param ] *)
type param = [ | `Param ]

type param_attrib = [ | `XMLns | `Id | `Value | `Value_Type | `Type ]

type param_content = notag

(* NAME: frameset - KIND: plus - ATTRIB: [= core | `FS_Rows | `FS_Cols |`OnLoad |`OnUnload] - CONTENT: [= `Frameset | `Frame ] - OUT: [=`Frameset] *)
type frameset = [ | `Frameset ]

type frameset_attrib = [ | core | `FS_Rows | `FS_Cols | `OnLoad | `OnUnload ]

type frameset_content = [ | `Frameset | `Frame ]

(* NAME: frame - KIND: nullary - ATTRIB: [= core | `Frameborder | `Longdesc | `Marginheight | `Marginwidth | `Name_01_00 | `Noresize | `Scrolling ] - CONTENT: notag - OUT: [=`Frame] *)
type frame = [ | `Frame ]

type frame_attrib =
  [
    | core
    | `Frameborder
    | `Longdesc
    | `Marginheight
    | `Marginwidth
    | `Name_01_00
    | `Noresize
    | `Scrolling
  ]

type frame_content = notag

(* NAME: noframes - KIND: nary - ATTRIB: [= common ] - CONTENT: [= `Body ] - OUT: [=`Noframes] *)
type noframes = [ | `Noframes ]

type noframes_attrib = [ | common ]

type noframes_content = [ | `Body ]

(* NAME: iframe - KIND: star - ATTRIB: [= core | `Frameborder | `Longdesc | `Marginheight | `Marginwidth | `Src | `Scrolling | `Name_01_00 | `Width | `Height ] - CONTENT: [= `PCDATA | flow ] - OUT: [=`Iframe] *)
type iframe = [ | `Iframe ]

type iframe_attrib =
  [
    | core
    | `Frameborder
    | `Longdesc
    | `Marginheight
    | `Marginwidth
    | `Src
    | `Scrolling
    | `Name_01_00
    | `Width
    | `Height
  ]

type iframe_content = [ | `PCDATA | flow ]

(* NAME: meta - KIND: nullary - ATTRIB: [= i18n |`Id | `Http_equiv | `Name | `Scheme ] - CONTENT: notag - OUT: [=`Meta] *)
type meta = [ | `Meta ]

type meta_attrib = [ | i18n | `Id | `Http_equiv | `Name | `Scheme ]

type meta_content = notag

(* NAME: style - KIND: star - ATTRIB: [= i18n |`XMLns |`Id | `Media | `Title | `XML_space ] - CONTENT: [= `PCDATA ] - OUT: [=`Style] *)
type style = [ | `Style ]

type style_attrib = [ | i18n | `XMLns | `Id | `Media | `Title | `XML_space ]

type style_content = [ | `PCDATA ]

(* NAME: link - KIND: nullary - ATTRIB: [= common | `Charset | `Href | `Hreflang | `Media | `Rel | `Rev | `Target | `Type ] - CONTENT: notag - OUT: [=`Link] *)
type link = [ | `Link ]

type link_attrib =
  [
    | common
    | `Charset
    | `Href
    | `Hreflang
    | `Media
    | `Rel
    | `Rev
    | `Target
    | `Type
  ]

type link_content = notag

(* NAME: base - KIND: nullary - ATTRIB: [= `XMLns | `Target ] - CONTENT: notag - OUT: [=`Base] *)
type base = [ | `Base ]

type base_attrib = [ | `XMLns | `Target ]

type base_content = notag

(* NAME: rbc - KIND: plus - ATTRIB: [= common ] - CONTENT: [= `Rb ] - OUT: [=`Rbc] *)
type rbc = [ | `Rbc ]

type rbc_attrib = [ | common ]

type rbc_content = [ | `Rb ]

(* NAME: rtc - KIND: plus - ATTRIB: [= common ] - CONTENT: [= `Rt ] - OUT: [=`Rtc] *)
type rtc = [ | `Rtc ]

type rtc_attrib = [ | common ]

type rtc_content = [ | `Rt ]

(* NAME: rtc_complex - KIND: plus - ATTRIB: [= common ] - CONTENT: [= `Rt_complex ] - OUT: [=`Rtc] *)
type rtc_complex = [ | `Rtc ]

type rtc_complex_attrib = [ | common ]

type rtc_complex_content = [ | `Rt_complex ]

(* NAME: rb - KIND: star - ATTRIB: [= common ] - CONTENT: [= no_ruby_content ] - OUT: [=`Rb] *)
type rb = [ | `Rb ]

type rb_attrib = [ | common ]

type rb_content = [ | no_ruby_content ]

(* NAME: rt - KIND: star - ATTRIB: [= common ] - CONTENT: [= no_ruby_content ] - OUT: [=`Rt] *)
type rt = [ | `Rt ]

type rt_attrib = [ | common ]

type rt_content = [ | no_ruby_content ]

(* NAME: rt_complex - KIND: star - ATTRIB: [= common | `Rbspan] - CONTENT: [= no_ruby_content ] - OUT: [=`Rt] *)
type rt_complex = [ | `Rt ]

type rt_complex_attrib = [ | common | `Rbspan ]

type rt_complex_content = [ | no_ruby_content ]

(* NAME: rp - KIND: star - ATTRIB: [= common ] - CONTENT: [= `PCDATA ] - OUT: [=`Rp] *)
type rp = [ | `Rp ]

type rp_attrib = [ | common ]

type rp_content = [ | `PCDATA ]


