val reset_input :
  ?a:[< `Accept
      | `Accesskey
      | `Alt
      | `Checked
      | `Class
      | `Disabled
      | `Id
      | `Input_Type
      | `Maxlength
      | `Name
      | `Readonly
      | `Size
      | `Src
      | `Tabindex
      | `Title
      | `Usemap
      | `Value
      | `XML_lang
      > `Input_Type `Value ]
     XHTML.M.attrib list ->
  XHTML.M.cdata -> [> `Input ] XHTML.M.elt

