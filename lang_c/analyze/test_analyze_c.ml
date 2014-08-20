open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)


let test_dataflow_c file =

  let _g = Graph_code_c.build ~verbose:true "/" [file] in
  let facts = [] in

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
    "-dataflow_c", "   <file>", 

    Common.mk_action_1_arg test_dataflow_c;
]
