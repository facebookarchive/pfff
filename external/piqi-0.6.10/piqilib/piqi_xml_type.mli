(*
   Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik

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

type xml =
  [ `Elem of xml_elem
  | `Data of string
  ]
and xml_elem = string * xml list (* (name, [xml]) *)


(* NOTE: the actual type sould be more like the one below, but the Xmlm parsing
 * library that we use won't give us such representation out the box, because it
 * is a generic xml parsing library

type xml_elem = string * xml_elem_content
and xml_elem_content = [ `Data of string | `Elements of xml_elem list ]
*)

