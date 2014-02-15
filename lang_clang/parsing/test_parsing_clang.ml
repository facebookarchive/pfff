open Common

open Ast_clang
module Ast = Ast_clang
module Flag = Flag_parsing_clang
module J = Json_type

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_clang file = 
  if not (file =~ ".*\\.clang") 
  then pr2 "warning: seems not a clang file";

  let toks = Parse_clang.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_clang xs =
  let fullxs = Lib_parsing_clang.find_source_files_of_dir_or_files xs in
  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);
    let _ast = Parse_clang.parse file in
    ()
  );
  ()

(*****************************************************************************)
(* Other actions *)
(*****************************************************************************)

(* see also clang -cc1 -ast-dump *)
let clang_check =
 (*  "/home/pad/local/clang_ast/clang-llvm/llvm/Debug+Asserts/bin/clang-check"*)
  "clang-check"

(* take compile_commands.json file as a parameter *)
let gen_clang jsonfile =
  let json = Json_in.load_json jsonfile in
  (match json with
  | J.Array xs ->
      let hdone = Hashtbl.create 101 in
      xs +> List.iter (fun json ->
        (match json with
        | J.Object ([
            "directory", _;
            "command", _;
            "file", J.String filename;
          ]) ->
            pr2 (spf "processing %s" filename);
            if Hashtbl.mem hdone filename
            then failwith ("already processed" ^ filename);
            Hashtbl.add hdone filename true;

            let (d,b,e) = Common2.dbe_of_filename filename in
            (match e with
            | "c" | "m" | "cpp" -> ()
            | _ -> failwith ("wierd extension for a clang input file: " ^ e)
            );
            let output = Common2.filename_of_dbe (d,b,"clang") in
            let cmd = spf "%s --ast-dump '%s' > '%s'" 
              clang_check filename output in
            Common.command2 cmd;
        | _ -> failwith "wrong compile_commands.json format"
        )
      )
  | _ -> failwith "wrong compile_commands.json format"
  )


(* dead *)
let split_dump file =
  let chan = open_in file in

  let chan_out = open_out "/dev/null" in
  let re_color = (Str.regexp ("\027\\[[^m]+m")) in
  
  let rec aux chan_out =
    let s, eof = 
      try 
        input_line chan, false
      with End_of_file -> "", true
    in
    if eof then ()
    else 
      if s =~ "^Processing: /Users/mathieubaudet/git/fbobjc/\\(.*\\)\\.$"
      then begin
        let file = Common.matched1 s in
        let file = Str.global_replace (Str.regexp " ") "___" file in
        let (d,b,_e) = Common2.dbe_of_filename file in
        pr2 file;
        Common.command2 (spf "mkdir -p %s" d);
        close_out chan_out;
        let file = Common2.filename_of_dbe (d,b,"clang") in
        let chan_out = open_out file in
        aux chan_out
      end
      else begin
        let s = Str.global_replace re_color  "" s in
        output_string chan_out s;
        output_string chan_out "\n" ;
        aux chan_out
      end
  in
  aux chan_out

let stat_clang_constructors xs =
  let fullxs = Lib_parsing_clang.find_source_files_of_dir_or_files xs in
  let h = Common2.hash_with_default (fun () -> 0) in
  
  fullxs +> Console.progress (fun k ->
    List.iter (fun file ->
      k();
      let ast = Parse_clang.parse file in
      ast +> Visitor_clang.visit (fun k x ->
        match x with
        | Paren (s, _, _) ->
            h#update s (fun x -> x + 1);
            k x
        | _ -> k x
      )
  ));
  h#to_list 
  +> Common.sort_by_val_highfirst 
  +> List.iter (fun (k, v) -> pr2 (spf "%s: %d" (Parse_clang.str_of_enum k) v))

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_clang", "   <file>", 
  Common.mk_action_1_arg test_tokens_clang;
  "-parse_clang", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_clang;
  "-dump_clang2", "   <file>", 
  Common.mk_action_1_arg (fun file ->
    let o = Common2.get_value file in
    let v = Meta_ast_clang.vof_program o in
    pr (Ocaml.string_of_v v);
  );

  "-gen_clang", " <>",
  Common.mk_action_1_arg (gen_clang);
  "-split_clang_dump", " <>",
  Common.mk_action_1_arg (split_dump);
  "-stat_clang_constructors", " <files or dirs>",
  Common.mk_action_n_arg stat_clang_constructors;
]
