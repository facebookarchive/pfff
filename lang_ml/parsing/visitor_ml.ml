(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

open Common

open Ocaml

open Ast_ml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* hooks *)
type visitor_in = {
  kinfo: info vin;
  kexpr: expr vin;
}
and visitor_out = {
(*
  vexpr: expr  -> unit;
  vst: st -> unit;
  vtop: toplevel -> unit;
  vinfo: info -> unit;
  vprogram: program -> unit;
*)
  vtoplevel: toplevel vout;
  vprogram: program vout;
}
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit
and 'a vout = 'a -> unit

let default_visitor = { 
  kinfo   = (fun (k,_) x -> k x);
  kexpr   = (fun (k,_) x -> k x);
}


let (mk_visitor: visitor_in -> visitor_out) = fun vin ->

(* start of auto generation *)

let rec v_info x =
  let k x = match x with { Parse_info.
     token = v_pinfox; comments = v_comments; transfo = v_transfo 
    } ->
    let _arg = Parse_info.v_pinfo v_pinfox in
    let _arg = v_unit v_comments in 
    let _arg = Parse_info.v_transformation v_transfo in 
    ()
  in
  vin.kinfo (k, all_functions) x

and v_tok v = v_info v


and v_toplevel =
  function
  | TODO v1 -> let v1 = v_info v1 in ()
  | NotParsedCorrectly v1 -> let v1 = v_list v_info v1 in ()
  | FinalDef v1 -> let v1 = v_info v1 in ()
and v_program v = v_list v_toplevel v

 and all_functions =   
    {
      vprogram = v_program;
      vtoplevel = v_toplevel;
    }
  in
  all_functions
