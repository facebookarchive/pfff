/* Graph viewer
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
 */

%{

let attrlist = ref []

(*XXX*)
let attrstmt kind =
  ()

let subgraph _ _ =
  ()

let create_graph strict typ name = ()

let add_node_list st l = (st, l)

%}

%token EOF
%token NODE
%token EDGE
%token GRAPH
%token DIGRAPH
%token STRICT
%token SUBGRAPH
%token EDGEOP
%token <string> ATOM
%token <string> QATOM

%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token SEMI
%token COMMA
%token COLON
%token EQUAL
%token PLUS

%start graph
%type <Dot_file.t> graph

%%

graph: optstrict graphtype optgraphname body EOF
     { { Dot_file.kind = $2; strict = $1;
         graph = { Dot_file.graph_name = $3; body = $4 } } }
;

body: LBRACE optstmtlist RBRACE
     { $2 }
;

optgraphname:
    atom
      { Some $1 }
  | /* empty */
      { None }
;

optstrict:
    STRICT
      { true }
  | /* empty */
      { false }
;

graphtype:
    GRAPH
      { `Graph }
  | DIGRAPH
      { `Digraph }
;

optstmtlist:
    stmtlist
      { List.rev $1 }
  | /* empty */
      { [] }

stmtlist:
    stmtlist stmt
      { $2 :: $1 }
  | stmt
      { [$1] }
;

optsemi:
    SEMI
      { }
  | /* empty */
      { }
;

stmt:
    attrstmt optsemi
      { `Attributes $1 }
  | compound optsemi
      { `Compound $1 }
;

compound: simple rcompound optattr
      { ($1 :: $2, List.flatten (List.rev $3)) }
;

simple:
    node
      { `Node $1 }
  | subgraph
      { `Graph $1 }
;

rcompound:
    EDGEOP simple rcompound
      { $2 :: $3 }
  | /* empty */
      { [] }
;

node:
    atom
      { { Dot_file.name = $1; port = None } }
  | atom COLON atom
      { { Dot_file.name = $1; port = Some $3 } }
  | atom COLON atom COLON atom
      { { Dot_file.name = $1; port = Some ($3 ^ ":" ^ $5) } }
;

attrstmt:
    attrtype attrlist
      { ($1, List.flatten (List.rev $2)) }
  | graphattrdefs
      { (`Graph, [$1]) }
;

attrtype:
    GRAPH
      { `Graph }
  | NODE
      { `Node }
  | EDGE
      { `Edge }
;

optattr:
    attrlist
      { $1 }
  | /* empty */
      { [] }
;

attrlist: optattr LBRACKET optattrdefs RBRACKET
      { List.rev $3 :: $1 }
;

optattrdefs:
    optattrdefs attrdefs
      { $2 :: $1 }
  | /* empty */
      { [] }
;

attrdefs: attritem optseparator
      { $1 }
;

attritem:
    attrassignment
      { $1 }
;

attrassignment: atom EQUAL atom
      { ($1, $3) }
  | atom
      { ($1, "true") }
;

graphattrdefs: atom EQUAL atom
      { ($1, $3) }
;

subgraph: optsubghdr body
      { {Dot_file.graph_name = $1; body = $2} }
;

optsubghdr:
    SUBGRAPH atom
      { Some $2 }
  | SUBGRAPH
      { None }
  | /* empty */
      { None }
;

optseparator:
  | COMMA
      { }
  | /*empty*/
      { }
;

atom:
    ATOM
      { $1 }
  | qatom
      { $1 }
;

qatom:
    QATOM
      { $1 }
  | qatom PLUS QATOM
      { $1 ^ $3 }
;
