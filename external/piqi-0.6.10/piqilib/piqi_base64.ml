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


(* TODO: add more base64 validation; the base64 library doesn't do any
 * validation *)
let decode x =
  try
    Base64.decode x
  with _ ->
    invalid_arg "Piqi_base64.decode"


let encode x =
  Base64.encode x

