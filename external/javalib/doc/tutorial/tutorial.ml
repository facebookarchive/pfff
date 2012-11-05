(* CAUTION: this file is only for testing purpose, it must contain
   all code given in sawja_tutorial.md and allows to test that tutorial
   text corresponds to actual version of Sawja*)

open Javalib_pack
open JBasics
let aname = make_cn "A"
let java_lang_string = make_cn "java.lang.String"
let ms =
make_ms "m" [TObject (TClass java_lang_string)] None
let fs = make_fs "f" (TBasic `Int)


open Javalib
let class_path = class_path "./" (* for instance *)

let a = get_class class_path aname
let m = get_method a ms
let f = get_field a fs

open Javalib_pack
open JBasics
open JCode
open Javalib

let get_accessed_fields (class_path : class_path)
 (cn : class_name) =
 (* We first recover the interface or class associated to the
    class name cn. *)
 let c = get_class class_path cn in
 (* Then, we get all the methods of c. *)
 let methods : jcode jmethod MethodMap.t = get_methods c in
 (* For each method of c, we associate a field set containing
    all the accessed fields. *)
   MethodMap.map
    (fun m ->
      match m with
      (* A method can be abstract or concrete. *)
       | AbstractMethod _ ->
         (* An abstract method has no code to parse. *)
          FieldSet.empty
       | ConcreteMethod cm ->
          (match cm.cm_implementation with
          (* A concrete method can be native so that we don't
             know its behaviour. In this case we suppose that
             no fields have been accessed which is not safe. *)
            | Native -> FieldSet.empty
            | Java code ->
              (* The code is stored in a lazy structure, for
                 performance purposes. Indeed when loading a
                 class the Javalib does not parse its methods. *)
               let jcode = Lazy.force code in
               (* We iter on the array of opcodes, building our
                  field set at the same time. *)
                 Array.fold_left
                  (fun s op ->
                    match op with
                     | OpGetField (_, fs)
                     | OpGetStatic (_, fs) ->
                  (* We add the field signature in our field set.
                     In this example, we ignore the classes in
                     which the fields are defined. *)
                        FieldSet.add fs s
                     | _ -> s
                  ) FieldSet.empty jcode.c_code
          )
    ) methods

let rec next_instruction (code : jopcodes) (pp : int)
  : (jopcode * int) option =
 try
   match code.(pp+1) with
    | OpInvalid -> next_instruction code (pp+1)
    | op -> Some (op,pp+1)
 with _ -> None


let get_equals_calls (class_path : class_path)
  (cn : class_name) =
 (* We first recover the interface or class associated to the
    class name cn. *)
 let java_lang_string = make_cn "java.lang.String" in
 let equals_ms =
   make_ms "equals" [TObject (TClass java_lang_object)]
    (Some (TBasic `Bool)) in
 let c = get_class class_path cn in
 (* Then, we get all the concrete methods of c. *)
 let methods : jcode concrete_method MethodMap.t =
   get_concrete_methods c in
 (* For each concrete method of c, we associate a (int*string) list
    containing all the strings passed as parameters to
    String.equals method, associated to the program point where the
    call occurs. *)
   MethodMap.map
    (fun m ->
     (match m.cm_implementation with
       (* A concrete method can be native so that we don't
          know its behaviour. In this case we suppose that
          no call to String.equals which is not safe. *)
       | Native -> []
       | Java code ->
         (* The code is stored in a lazy structure, for
            performance purposes. Indeed when loading a
            class the Javalib does not parse its methods. *)
          let jcode = Lazy.force code in
          let code = jcode.c_code in
          let l = ref [] in
          (* We iter on the array of opcodes, building
              our list of (int*string) at the same time. *)
            Array.iteri
             (fun pp op ->
               match op with
                | OpConst (`String s) ->
                  (* We detect that a string s is pushed on the
                     stack. The next instruction might be an
                     invokevirtual on String.equals. *)
                   (match (next_instruction code pp) with
                     | Some (inst,ppi) ->
                        (match inst with
                          | OpInvoke (`Virtual (TClass cn), ms)
                             when cn = java_lang_string
                               && ms = equals_ms ->
                            (* We add the program point of the
                               invokevirtual and the pushed string
                               in our list. *)
                             l := (ppi, s) :: !l
                          | _ -> ()
                        )
                     | None -> ()
                   )
                | _ -> ()
             ) code;
          (* We simply return our list, in the reverse order so that
             the program points appear in ascending order. *)
            List.rev !l
     )
    ) methods


let cp = class_path "."
let cn = make_cn "TestString"
let mmap = get_equals_calls cp cn
let l = 
    let sk = List.map (ms_name) (MethodMap.key_elements mmap)
    and sv = MethodMap.value_elements mmap in
      List.combine sk sv
let () = close_class_path cp

