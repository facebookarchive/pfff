open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse_minic file =
  let _ast = 
    Parse_minic.parse file 
  in
  ()

let test_dataflow_minic file =
  let ast = Parse_minic.parse file in
  let facts = Datalog_minic.generate_facts ast in

  (* debug *)
  facts +> List.iter pr2_gen;
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  
  let facts_file = "/tmp/facts.dl" in
  Common.with_open_outfile facts_file (fun (pr_no_nl, _chan) ->
    let pr s = pr_no_nl (s ^ ".\n") in
    facts +> List.iter pr;
  );
  
  let logic_file = 
    Filename.concat Config_pfff.path "h_program-lang/datalog_code.dl" in
  
  let final_file = "/tmp/datalog.dl" in
  let cmd = spf "cat %s %s > %s" facts_file logic_file final_file in
  Common.command2 cmd;
  let cmd = spf "datalog %s | sort" final_file in
  Common.command2 cmd;
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
    "-parse_minic", "   <file>", 
    Common.mk_action_1_arg test_parse_minic;
    "-dataflow_minic", "   <file>", 
    Common.mk_action_1_arg test_dataflow_minic;
]
