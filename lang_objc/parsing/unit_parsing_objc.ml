open Common
open OUnit

module Flag = Flag_parsing_cpp

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let parse file = 
  Common.save_excursion Flag.error_recovery false (fun () ->
  Common.save_excursion Flag.show_parsing_error false (fun () ->
  Common.save_excursion Flag.verbose_parsing false (fun () ->
    Parse_objc.parse file
  )))

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
 "parsing_objc" >::: [

   (*-----------------------------------------------------------------------*)
   (* Parsing *)
   (*-----------------------------------------------------------------------*)
   "regression files" >:: (fun () ->
     let dir = Filename.concat Config_pfff.path "/tests/objc/parsing" in
     let files = 
       Common2.glob (spf "%s/*.m" dir) @ Common2.glob (spf "%s/*.h" dir) in
     files +> List.iter (fun file ->
       try
         let _ast = parse file in
         ()
       with Parse_cpp.Parse_error _ ->
         assert_failure (spf "it should correctly parse %s" file)
     )
   );

 ]
