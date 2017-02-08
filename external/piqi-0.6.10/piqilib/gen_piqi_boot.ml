(*
   Copyright 2014 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


let load_file fname =
  let ch = open_in fname in
  let len = in_channel_length ch in
  let res = String.create len in
  really_input ch res 0 len;
  close_in ch;
  res


let main () =
  let piqi_lang_binobj = load_file "piqi-lang.piqi.pb" in
  let piqi_spec_binobj = load_file "piqi.piqi.pb" in

  Printf.printf "let parse_piqi_binobj x = Piqi_piqirun.parse_binobj Piqi_impl_piqi.parse_piqi x\n\n";

  Printf.printf "let piqi_lang =\n";
  Printf.printf "  let piqi_lang_binobj =\n";
  Printf.printf "    \"%s\"\n" (String.escaped piqi_lang_binobj);
  Printf.printf "  in parse_piqi_binobj piqi_lang_binobj\n\n";

  Printf.printf "let piqi_spec =\n";
  Printf.printf "  let piqi_spec_binobj =\n";
  Printf.printf "    \"%s\"\n" (String.escaped piqi_spec_binobj);
  Printf.printf "  in parse_piqi_binobj piqi_spec_binobj\n\n";
  ()


let _ =
  main ()

