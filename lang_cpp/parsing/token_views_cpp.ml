(* Yoann Padioleau
 * 
 * Copyright (C) 2007, 2008 Ecole des Mines de Nantes
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

open Common

module TH = Token_helpers_cpp

open Parser_cpp

(*****************************************************************************)
(* Some debugging functions  *)
(*****************************************************************************)

let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_cpp.verbose_parsing 
