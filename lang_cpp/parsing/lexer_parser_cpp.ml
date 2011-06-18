(* Yoann Padioleau
 * 
 * Copyright (C) 2002 Yoann Padioleau
 * Copyright (C) 2006-2007 Ecole des Mines de Nantes
 * Copyright (C) 2008-2009 University of Urbana Champaign
 * Copyright (C) 2010 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * This module used to contain some hacks to handle typedefs.
 * The parser while analyzing a file would recognize certain constructs
 * like typedefs declarations and modify some globals in this file
 * so that the lexer would know that the next time a certain ident
 * is lexed, it may actually correspond to a typedef. 
 * Knowing that an ident is a typedef is important because the
 * C and C++ grammar are not context-free grammars. 
 * They need to have this information and so they need two different
 * tokens (TIdent and TIdent_Typedef).
 * 
 * This file is now empty because having yet another hack around
 * the tokens was too much. First it was useful only for the
 * typedefs that were declared in the file. Because we needed
 * a general solution on this problem, having this special case
 * was not helping. Moreover even handling this special case
 * was tricky because one can write 'acpi acpi;' in which case
 * the ident->typedef mechanism must be disabled just
 * after having recognized a type. This was leading to many
 * et/dt (for enable/disable typedef translation) in the
 * grammar which was ugly.
 * 
 * Everything is now handled in parsing_hack.ml
 * by using the different views in token_views_cpp.ml and by
 * looking ahead as much as we want.
 * 
 * Here is the old doc:
 * 
 * "This file is one of the many to provide hacks around the
 * grammar. This module contains tricks to handle the ambiguity 
 * in the grammar with the typedef which impose a cooperation 
 * between the lexer and the parser. The C (and C++) grammar is
 * not context free.
 * 
 * An example by Hughes Casse: 
 * 
 *   "in the symbol table, local definitions must replace type 
 *    definitions in order to correctly parse local variable in 
 *    functions body. This is the only way to correctly
 *    handle this kind of exception, that is,
 * 
 *       typedef ... ID; 
 *       int f(int *p) {
 *          int ID; 
 *          return (ID) * *p;
 *       } 
 *
 *    If ID isn't overload, the last expression is parsed as a type cast. 
 *    If it isn't, this a multiplication."
 * "
 *)
