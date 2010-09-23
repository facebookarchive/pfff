(* ********************************************************************** *)
(* Constructing Records *)
(* ********************************************************************** *)
let pleac_Constructing_Records () = 
  #load "str.cma";;
  
  type record = { name : string;
                  empno : int;
                  mutable title : string;
                  mutable age : int;
                  mutable salary : float;
                  mutable pals : string list;
                }
  
  let record = { name = "Jason";
                 empno = 132;
                 title = "deputy peon";
                 age = 23;
                 salary = 37000.00;
                 pals = [ "Norbert"; "Rhys"; "Phineas" ]
               }
  
  let () =
    Printf.printf "I am %s, and my pals are %s.\n"
      record.name
      (String.concat ", " record.pals)
  
  let byname = Hashtbl.create 0
  
  let () =
    (* store record *)
    Hashtbl.replace byname record.name record;
  
    (* later on, look up by name *)
    begin
      try
        let rp = Hashtbl.find byname "Aron" in
        Printf.printf "Aron is employee %d\n" rp.empno
      with Not_found ->
        (* raised if missing *)
        ()
    end;
  
    (* give jason a new pal *)
    let jason = Hashtbl.find byname "Jason" in
    jason.pals <- "Theodore" :: jason.pals;
    Printf.printf "Jason now has %d pals\n" (List.length jason.pals);
  
    Hashtbl.iter
      (fun name record ->
         Printf.printf "%s is employee number %d\n" name record.empno)
      byname
  
  let employees = Hashtbl.create 0
  
  let () =
    (* store record *)
    Hashtbl.replace employees record.empno record;
  
    (* lookup by id *)
    begin
      try
        let rp = Hashtbl.find employees 132 in
        Printf.printf "employee number 132 is %s\n" rp.name
      with Not_found ->
        ()
    end;
  
    let jason = Hashtbl.find byname "Jason" in
    jason.salary <- jason.salary *. 1.035
  
  (* Return true if the string s contains the given substring. *)
  let contains s substring =
    try ignore (Str.search_forward (Str.regexp_string substring) s 0); true
    with Not_found -> false
  
  let () =
    (* A filter function for hash tables, written as a fold. *)
    let grep f hash =
      Hashtbl.fold
        (fun key value result ->
           if f value then value :: result else result)
        hash [] in
  
    (* Select records matching criteria. *)
    let peons =
      grep (fun employee -> contains employee.title "peon") employees in
    let tsevens =
      grep (fun employee -> employee.age = 27) employees in
  
    (* Go through all records. *)
    let records = Hashtbl.fold (fun _ v a -> v :: a) employees [] in
    List.iter
      (fun rp ->
         Printf.printf "%s is age %d.\n" rp.name rp.age)
      (List.sort (fun r1 r2 -> compare r1.age r2.age) records)
  
  (* Create an array of lists of records by age. *)
  let byage = Array.create 150 []
  let () =
    Hashtbl.iter
      (fun _ employee ->
         byage.(employee.age) <- employee :: byage.(employee.age))
      employees
  
  (* Print all employees by age. *)
  let () =
    Array.iteri
      (fun age emps ->
         match emps with
           | [] -> ()
           | _ ->
               Printf.printf "Age %d: " age;
               List.iter (fun emp -> Printf.printf "%s " emp.name) emps;
               print_newline ())
      byage
  
  (* Similar approach using List.map and String.concat. *)
  let () =
    Array.iteri
      (fun age emps ->
         match emps with
           | [] -> ()
           | _ ->
               Printf.printf "Age %d: %s\n" age
                 (String.concat ", " (List.map (fun r -> r.name) emps)))
      byage
  

