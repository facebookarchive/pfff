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

module Flag = Flag_parsing_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* This module is one of the many to provide hacks around the
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
 *)

(*****************************************************************************)
(* Typedef fix *)
(*****************************************************************************)

type identkind = TypeDefI | IdentI

(* Ca marche ce code ? on peut avoir un typedef puis un ident puis
 * un typedef nested ? oui car Hashtbl (dans scoped_h_env) gere l'historique. 
 * 
 * oldsimple:  but slow,  take 2 secondes on some C files 
 *    let (typedef: typedef list list ref) = ref [[]]
 *)
let (_typedef : (string, identkind) Common.scoped_h_env ref) = 
  ref (Common.empty_scoped_h_env ())


(* Why those enable/disable ? When we introduce a new variable, for
 * instance when we declare parameters for a function such as 'int var_t',
 * then the 'var_t' must not be lexed as a typedef, so we must disable
 * temporaly the typedef mechanism to allow variable with same name as
 * a typedef. 
 *)
(* parse_typedef_fix2 *)
let _handle_typedef = ref true
let enable_typedef ()  = _handle_typedef := true
let disable_typedef () = _handle_typedef := false
let is_enabled_typedef () = !_handle_typedef

let is_typedef s  = if !_handle_typedef then
  (match (Common.optionise (fun () -> Common.lookup_h_env s !_typedef)) with
  | Some TypeDefI -> true
  | Some IdentI -> false
  | None -> false
  )
  else false

let new_scope() = Common.new_scope_h _typedef
let del_scope() = Common.del_scope_h _typedef

let add_typedef  s = Common.add_in_scope_h _typedef (s, TypeDefI)
let add_ident s    = Common.add_in_scope_h _typedef (s, IdentI)

let add_typedef_root s = 
  if !Flag.add_typedef_root
  then 
    Hashtbl.add !_typedef.scoped_h s TypeDefI
  else add_typedef s (* have far more .failed without this *)


(* Used by parse_cpp when we do some error recovery. The parse error may
 * have some bad side effects on typedef hash, so recover this.
 *)
let _old_state = ref (Common.clone_scoped_h_env !_typedef)

let save_typedef_state () = 
  _old_state := Common.clone_scoped_h_env !_typedef

let restore_typedef_state () = 
  _typedef := !_old_state
  

(*****************************************************************************)
(* Context help *)
(*****************************************************************************)

(* In addition to adjusting a symbol table for typedefs while parsing,
 * we also adjust some contextual information that right now
 * is used in parsing_hacks.ml.
 * 
 * todo? could this be a view or some info processed only by the lexer?
 * do we really need the lexer/parser cooperation here?
 *
 * c++ext: must have a stack of Class/Struct or Func/Method 
 * as can nest them arbitrarily 
 *)
type context = 
  | InTopLevel
  | InFunction
  | InClassStruct of string
  | InStructAnon
  | InTemplateParam
  | InParameter
  | InInitializer
  | InEnum

let is_top_or_struct = function
  | InTopLevel
  | InClassStruct _ 
  | InStructAnon
      -> true
  | _ -> false

type lexer_hint = { 
  mutable context_stack: context Common.stack;
 }

let default_hint () = { 
  context_stack = [InTopLevel];
}

let _lexer_hint = ref (default_hint())

let current_context () = List.hd !_lexer_hint.context_stack 
let push_context ctx = 
  !_lexer_hint.context_stack <- ctx::!_lexer_hint.context_stack
let pop_context () = 
  !_lexer_hint.context_stack <- List.tl !_lexer_hint.context_stack


(*****************************************************************************)
(* Reset state *)
(*****************************************************************************)

let lexer_reset_state () = 
  begin
  _handle_typedef := true;
  _typedef := Common.empty_scoped_h_env ();
  _lexer_hint := (default_hint ());
  end

