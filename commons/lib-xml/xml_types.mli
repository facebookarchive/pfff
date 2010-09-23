(*
 * Xml Light, an small Xml parser/printer with DTD support.
 * Copyright (C) 2003 Nicolas Cannasse (ncannasse@motion-twin.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** {6 Xml Data Structure} *)

(** An Xml node is either
	[Element (tag-name, attributes, children)] or [PCData text] *)

type xml =
    Element of (string * (string * string) list * xml list)
  | PCData of string

(** {6:exc Xml Exceptions} *)

(** Several exceptions can be raised when parsing an Xml document : {ul
	{li {!Xml.Error} is raised when an xml parsing error occurs. the
		{!Xml.error_msg} tells you which error occured during parsing
		and the {!Xml.error_pos} can be used to retreive the document
		location where the error occured at.}
	{li {!Xml.File_not_found} is raised when and error occured while
		opening a file with the {!Xml.parse_file} function or when a
		DTD file declared by the Xml document is not found {i (see the
		{!XmlParser} module for more informations on how to handle the
		DTD file loading)}.}
	}
	If the Xml document is containing a DTD, then some other exceptions
	can be raised, see the module {!Dtd} for more informations.
 *)

type error_pos = { eline : int; eline_start : int; emin : int; emax : int; }

type error_msg =
    UnterminatedComment
  | UnterminatedString
  | UnterminatedEntity
  | IdentExpected
  | CloseExpected
  | NodeExpected
  | AttributeNameExpected
  | AttributeValueExpected
  | EndOfTagExpected of string
  | EOFExpected

type error = error_msg * error_pos

exception Error of error
exception File_not_found of string
