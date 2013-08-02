**************************************************************************
*                                                                        *
*  Ocamlgraph: a generic graph library for OCaml                         *
*  Copyright (C) 2004-2010                                               *
*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *
*                                                                        *
*  This software is free software; you can redistribute it and/or        *
*  modify it under the terms of the GNU Library General Public           *
*  License version 2.1, with the special exception on linking            *
*  described in file LICENSE.                                            *
*                                                                        *
*  This software is distributed in the hope that it will be useful,      *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
*                                                                        *
**************************************************************************


Ocamlgraph is a graph library for Ocaml. Its contribution is three-fold:

1. It provides an easy-to-use graph implementation together with several
   operations and algorithms over graphs, in Graph.Pack.Digraph.
   It is a reasonably efficient imperative data structure for directed graphs 
   with vertices and edges labeled with integers.

   Have a look at this module first in order to get an overview of what
   this library provides. See also `demo.ml'.

2. Then ocamlgraph provides several other graph implementations for those
   not satisfied with the one above. Some are persistent (imutable) and other
   imperative (mutable). Some are directed and other are not.
   Some have labels for vertices, or labels for edges, or both. 
   Some have abstract types for vertices. etc.
 
   See interface Sig for the graph signatures and modules Persistent and 
   Imperative for the implementations.

   These implementations are written as functors: you give the types of 
   vertices labels, edge labels, etc. and you get the data structure as a
   result.

3. Finally, ocamlgraph provides several classic operations and algorithms
   over graphs. They are also written as functors i.e. independently of the 
   data structure for graphs. One consequence is that you can define your own
   data structure for graphs and yet re-use all the algorithms from this 
   library -- you only need to provide a few operations such as iterating over
   all vertices, over the successors of a vertex, etc.


How to link with ocamlgraph
---------------------------

ocamlgraph is packaged as a single module `Graph'. Link is done as follows:

- bytecode

	ocamlc graph.cma <other files>

- native code

	ocamlopt graph.cmxa <other files>


Examples
--------

You'll find examples of ocamlgraph use in demo.ml, demo_planar.ml and color.ml 
(you can compile these programs with "make demo.opt", "make demo_planar.opt"
and "make color.opt" respectively).


Bug reports
-----------

Bug reports can be sent to 

  Sylvain Conchon <sylvain.conchon@lri.fr>
  Jean-Christophe Filliatre <filliatr@lri.fr>
  Julien Signoles <julien.signoles@cea.fr>
