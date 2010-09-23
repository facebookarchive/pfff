(* ********************************************************************** *)
(* Overloading Operators *)
(* ********************************************************************** *)
let pleac_Overloading_Operators () = 
  (* Create a class with "compare_to" and "to_string" methods. *)
  class klass name idnum = object (self)
    val name = (name : string)
    val idnum = (idnum : int)
  
    method name = name
    method idnum = idnum
  
    method compare_to (other : klass) =
      compare
        (String.uppercase self#name)
        (String.uppercase other#name)
  
    method to_string =
      Printf.sprintf "%s (%05d)"
        (String.capitalize self#name)
        self#idnum
  end
  
  (* Define a comparison operator that invokes a "compare_to" method. *)
  let ( <=> ) o1 o2 = (o1 #compare_to o2 : int)
  
  (* Demonstrate these two methods. *)
  let () =
    let a = new klass "test1" 5 in
    let b = new klass "TEST2" 10 in
    Printf.printf "%d\n" (a <=> b);
    Printf.printf "%s\n%s\n" a#to_string b#to_string
  
  (* Define a module to contain our time type. *)
  module TimeNumber = struct
  
    (* TimeNumber.t contains the time values. *)
    class t hours minutes seconds = object (self)
      val mutable hours = (hours : int)
      val mutable minutes = (minutes : int)
      val mutable seconds = (seconds : int)
  
      method hours = hours
      method minutes = minutes
      method seconds = seconds
  
      method set_hours hours' = hours <- hours'
      method set_minutes minutes' = minutes <- minutes'
      method set_seconds seconds' = seconds <- seconds'
  
      (* TimeNumber.t#add adds two times together. *)
      method add (other : t) =
        let answer = new t
          (self#hours + other#hours)
          (self#minutes + other#minutes)
          (self#seconds + other#seconds) in
        if answer#seconds >= 60
        then (answer#set_seconds (answer#seconds mod 60);
              answer#set_minutes (answer#minutes + 1));
        if answer#minutes >= 60
        then (answer#set_minutes (answer#minutes mod 60);
              answer#set_hours (answer#hours + 1));
        answer
    end
  
    (* TimeNumber.Operators is a submodule that is designed to be
       imported using "open". It redefines the built-in arithmetic
       operators to work on TimeNumber.t values. *)
    module Operators = struct
      let ( + ) (t1 : t) (t2 : t) = t1 #add t2
      (* let ( - ) (t1 : t) (t2 : t) = t1 #sub t2 *)
      (* let ( * ) (t1 : t) (t2 : t) = t1 #mult t2 *)
      (* let ( / ) (t1 : t) (t2 : t) = t1 #div t2 *)
    end
  
  end
  
  (* Globally import the custom operators. This will make them work on
     TimeNumber.t values *only* - to do regular integer addition, you
     will now have to use Pervasives.( + ) and so on. *)
  open TimeNumber.Operators
  let () =
    let t1 = new TimeNumber.t 2 59 59 in
    let t2 = new TimeNumber.t 1 5 6 in
    let t3 = t1 + t2 in
    Printf.printf "%02d:%02d:%02d\n" t3#hours t3#minutes t3#seconds
  
  (* Locally import the custom operators using a "let module". The
     operators will only be redefined within the "Local" module. *)
  let () =
    let t1 = new TimeNumber.t 2 59 59 in
    let t2 = new TimeNumber.t 1 5 6 in
    let t3 =
      let module Local = struct
        open TimeNumber.Operators
        let result = t1 + t2
      end in Local.result in
    Printf.printf "%02d:%02d:%02d\n" t3#hours t3#minutes t3#seconds
  
  (* The openin syntax extension can simplify the above technique.
     openin is available at http://alain.frisch.fr/soft.html#openin *)
  let () =
    let t1 = new TimeNumber.t 2 59 59 in
    let t2 = new TimeNumber.t 1 5 6 in
    let t3 = open TimeNumber.Operators in t1 + t2 in
    Printf.printf "%02d:%02d:%02d\n" t3#hours t3#minutes t3#seconds
  
  (*-----------------------------*)
  
  (* show_strnum - demo operator overloading *)
  
  class strnum value = object
    method value = value
    method spaceship (other : strnum) = compare value (other#value)
    method concat (other : strnum) = new strnum (value ^ other#value)
    method repeat n = new strnum (String.concat ""
                                    (Array.to_list (Array.make n value)))
  end
  
  let (  +  ) a b = a #concat b
  let (  *  ) a b = a #repeat b
  let ( <=> ) a b = a #spaceship b
  let (  <  ) a b = a <=> b < 0
  let ( <=  ) a b = a <=> b <= 0
  let (  =  ) a b = a <=> b = 0
  let ( >=  ) a b = a <=> b >= 0
  let (  >  ) a b = a <=> b > 0
  
  (*-----------------------------*)
  
  let x = new strnum "Red"
  let y = new strnum "Black"
  let z = x + y
  let r = z * 3
  
  let () =
    Printf.printf "values are %s, %s, %s, and %s\n"
      x#value y#value z#value r#value;
    Printf.printf "%s is %s %s\n"
      x#value (if x < y then "LT" else "GE") y#value
  
  (*
    values are Red, Black, RedBlack, and RedBlackRedBlackRedBlack
    Red is GE Black
  *)
  
  (*-----------------------------*)
  
  (* demo_fixnum - show operator overloading *)
  
  module FixNum = struct
    let default_places = ref 0
    class t ?places (value : float) = object
      val mutable places =
        match places with
          | Some n -> n
          | None -> !default_places
      val value = value
      method places = places
      method set_places n = places <- n
      method value = value
      method to_string = Printf.sprintf "FixNum.t: %.*f" places value
    end
  end
  
  let ( + ) a b =
    new FixNum.t ~places:(max a#places b#places) (a#value +. b#value)
  let ( - ) a b =
    new FixNum.t ~places:(max a#places b#places) (a#value -. b#value)
  let ( * ) a b =
    new FixNum.t ~places:(max a#places b#places) (a#value *. b#value)
  let ( / ) a b =
    new FixNum.t ~places:(max a#places b#places) (a#value /. b#value)
  
  (*-----------------------------*)
  
  (* let () = FixNum.default_places := 5 *)
  
  let x = new FixNum.t 40.
  let y = new FixNum.t 12.
  
  let () =
    Printf.printf "sum of %s and %s is %s\n"
      x#to_string y#to_string (x + y)#to_string;
    Printf.printf "product of %s and %s is %s\n"
      x#to_string y#to_string (x * y)#to_string
  
  let z = x / y
  
  let () =
    Printf.printf "%s has %d places\n" z#to_string z#places;
    if z#places = 0 then z#set_places 2;
    Printf.printf "div of %s by %s is %s\n"
      x#to_string y#to_string z#to_string;
    Printf.printf "square of that is %s\n" (z * z)#to_string
  
  (*
    sum of FixNum.t: 40 and FixNum.t: 12 is FixNum.t: 52
    product of FixNum.t: 40 and FixNum.t: 12 is FixNum.t: 480
    FixNum.t: 3 has 0 places
    div of FixNum.t: 40 by FixNum.t: 12 is FixNum.t: 3.33
    square of that is FixNum.t: 11.11
  *)
  

