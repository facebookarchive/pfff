(* Graph viewer
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

{

type graph_type = Graph | Digraph | Unknown
let graph_type = ref Unknown

let set_graph_type v = if !graph_type = Unknown then graph_type := v

let buffer = Buffer.create 256

let html_nest = ref 1

let reset () = graph_type := Unknown

}

let letter = ['A'-'Z' 'a'-'z' '_' '\128'-'\255']
let digit = ['0'-'9']
let name = letter (letter | digit) *
let number = '-'? ((digit+ ('.' digit*)?) | ('.' digit+))
let id = name | number

rule token =
  parse
    "/*"
      { comment lexbuf }
  | "//" [^ '\n']*
  | "#" [^ '\n']*
  | [' ' '\t' '\r' '\n']+
      { token lexbuf }
  | "->"
      { if !graph_type = Digraph then
          Dot_parser.EDGEOP
        else
          exit 1 (*XXX*) }
  | "--"
      { if !graph_type = Graph then
          Dot_parser.EDGEOP
        else
          exit 1 (*XXX*) }
  | name
      {
        let s = Lexing.lexeme lexbuf in
        match String.lowercase s with
          "node"     -> Dot_parser.NODE
        | "edge"     -> Dot_parser.EDGE
        | "graph"    -> set_graph_type Graph; Dot_parser.GRAPH
        | "digraph"  -> set_graph_type Digraph; Dot_parser.DIGRAPH
        | "strict"   -> Dot_parser.STRICT
        | "subgraph" -> Dot_parser.SUBGRAPH
        | _          -> Dot_parser.ATOM s }
  | number
      { Dot_parser.ATOM (Lexing.lexeme lexbuf) }
  | '"'
      { qstring lexbuf }
  | '<'
      { Buffer.add_char buffer '<';
        html_nest := 1;
        hstring lexbuf }
  | '{'
      { Dot_parser.LBRACE }
  | '}'
      { Dot_parser.RBRACE }
  | '['
      { Dot_parser.LBRACKET }
  | ']'
      { Dot_parser.RBRACKET }
  | '='
      { Dot_parser.EQUAL }
  | ':'
      { Dot_parser.COLON }
  | ';'
      { Dot_parser.SEMI }
  | ','
      { Dot_parser.COMMA }
  | '+'
      { Dot_parser.PLUS }
  | _
      { Format.eprintf "%s@." (Lexing.lexeme lexbuf); exit 1 (*XXX*) }
  | eof
      { Dot_parser.EOF }

and comment =
  parse
    [^'*']*
      { comment lexbuf }
  | '*'+ [^ '*' '/']*
      { comment lexbuf }
  | '*'+ '/'
      { token lexbuf }
(*
  | eof
*)

and qstring =
  parse
    '"'
      { let s = Buffer.contents buffer in
        Buffer.clear buffer;
        Dot_parser.QATOM s }
  | "\\\""
      { Buffer.add_string buffer "\\\"";
        qstring lexbuf }
  | "\\\n"
      { qstring lexbuf }
  | '\n'
      { Buffer.add_char buffer '\n';
        qstring lexbuf }
  | [^ '"' '\\']+ | '\\' [^'\n']
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        qstring lexbuf }
(*
   | _
   | eof
*)

and hstring =
  parse
    '>'
      { decr html_nest;
        if !html_nest > 0 then hstring lexbuf else begin
          Buffer.add_char buffer '>';
          let s = Buffer.contents buffer in
          Buffer.clear buffer;
          Dot_parser.QATOM s
        end }
  | '<'
      { incr html_nest;
        Buffer.add_char buffer '<';
        hstring lexbuf }
  | [^ '<' '>'] +
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        hstring lexbuf }
