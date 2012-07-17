open OUnit
open Typing_infer_array_php.Array_typer

module SSet = Set.Make (String)
module SMap = Map.Make (String)
module ETP = Env_typing_php

let unittest = 
  "array_inference_php" >::: [
   
    "make typer" >:: (fun () ->
      let at = make_array_typer "filename" in
      assert_equal at.file "filename";
    );

    "make container evidence" >:: (fun() ->
      let ce = make_container_evidence in
      let {supporting = s; opposing = o} = ce in
      assert_equal s [];
      assert_equal o [];
    );

    "compose ce" >:: (fun() ->
      let s = [SingleLine(None)] in 
      let o = [] in
      let ce = compose_ce s o in
      let {supporting = s1; opposing = o1} = ce in
      assert_equal s s1;
      assert_equal o o1;
    );

    "add evidence" >:: (fun() ->
      let ce = make_container_evidence in 
      let e = SingleLine(None) in 
      let ce = add_evidence ce Supporting e in
      let {supporting = s; opposing = o} = ce in 
      assert_equal s [e];
      assert_equal o [];
      let ce = add_evidence ce Opposing e in 
      let {supporting = s; opposing = o} = ce in 
      assert_equal s [e];
      assert_equal o [e];
      let ce = add_evidence ce Opposing e in 
      let {supporting = s; opposing = o} = ce in 
      assert_equal s [e];
      assert_equal o [e;e];
    );

    "make inferred container" >:: (fun() -> 
      let empty_ce = make_container_evidence in
      let ic = make_inferred_container in
      let {map = m; tuple = t; vector = ve; guess = g; confused = c;
      mixed_val_ty = mt; key_t = k; value_t = va; declaration = d} = ic in
      assert_equal m empty_ce;
      assert_equal t empty_ce;
      assert_equal ve empty_ce;
      assert_equal g NoData;
      assert_equal c false;
      assert_equal mt false;
      assert_equal k None;
      assert_equal va None;
      assert_equal d NoDec;
    );

    "compose ic" >:: (fun() ->
      let empty_ce = make_container_evidence in
      let ic = compose_ic empty_ce empty_ce empty_ce Map true false
        (Some(ETP.Tsum[ETP.Tabstr "int"])) None DValue in
      let {map = m; tuple = t; vector = ve; guess = g; confused = c;
      mixed_val_ty = mt; key_t = k; value_t = va; declaration = d} = ic in
      assert_equal m empty_ce;
      assert_equal t empty_ce;
      assert_equal ve empty_ce;
      assert_equal g Map;
      assert_equal c true;
      assert_equal mt false;
      assert_equal k (Some(ETP.Tsum[ETP.Tabstr "int"]));
      assert_equal va None;
      assert_equal d DValue;
    );

    "string type equivalences" >:: (fun() -> 
      let tabstr = ETP.Tsum[ETP.Tabstr "string"] in
      let strset1 = SSet.empty in
      let strset1 = SSet.add "str1" strset1 in
      let tsstring = ETP.Tsum[ETP.Tsstring(strset1)] in
      assert_bool "Tabsrt and Tsstring are not equal types" (string_equ tabstr
        tsstring);
    );

  ]
