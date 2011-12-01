(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008              *) 
(* Maas-Maarten Zeeman.                                                *)

(*
The package OUnit is copyright by Maas-Maarten Zeeman.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this document and the OUnit software ("the Software"), to
deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons
to whom the Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

The Software is provided ``as is'', without warranty of any kind,
express or implied, including but not limited to the warranties of
merchantability, fitness for a particular purpose and noninfringement.
In no event shall Maas-Maarten Zeeman be liable for any claim, damages
or other liability, whether in an action of contract, tort or
otherwise, arising from, out of or in connection with the Software or
the use or other dealings in the software.
*)
(***********************************************************************)

(** The OUnit library can be used to implement unittests

    To uses this library link with
      [ocamlc oUnit.cmo]
    or 
      [ocamlopt oUnit.cmx]
 
    @author Maas-Maarten Zeeman
*)

(** {5 Assertions} 

    Assertions are the basic building blocks of unittests. *)

(** Signals a failure. This will raise an exception with the specified
    string. 

    @raise Failure to signal a failure *)
val assert_failure : string -> 'a

(** Signals a failure when bool is false. The string identifies the 
    failure.
    
    @raise Failure to signal a failure *)
val assert_bool : msg:string -> bool -> unit

(** Shorthand for assert_bool 

    @raise Failure to signal a failure *)
val ( @? ) : string -> bool -> unit

(** Signals a failure when the string is non-empty. The string identifies the
    failure. 
    
    @raise Failure to signal a failure *) 
val assert_string : string -> unit


(** Compares two values, when they are not equal a failure is signaled.
    The cmp parameter can be used to pass a different compare function. 
    This parameter defaults to ( = ). The optional printer can be used 
    to convert the value to string, so a nice error message can be 
    formatted. When msg is also set it can be used to identify the failure. 

    @raise Failure description *)
val assert_equal : ?cmp:('a -> 'a -> bool) ->  ?printer:('a -> string) -> 
                   ?msg:string -> 'a -> 'a -> unit

(** Asserts if the expected exception was raised. When msg is set it can 
    be used to identify the failure

    @raise Failure description *)
val assert_raises : ?msg:string -> exn -> (unit -> 'a) -> unit

(** {5 Skipping tests } 
  
   In certain condition test can be written but there is no point running it, because they
   are not significant (missing OS features for example). In this case this is not a failure
   nor a success. Following function allow you to escape test, just as assertion but without
   the same error status.
  
   A test skipped is counted as success. A test todo is counted as failure.  *)

(** [skip cond msg] If [cond] is true, skip the test for the reason explain in [msg].
  * For example [skip_if (Sys.os_type = "Win32") "Test a doesn't run on windows"].
  *)
val skip_if : bool -> string -> unit

(** The associated test is still to be done, for the reason given.
  *)
val todo : string -> unit

(** {5 Compare Functions} *)

(** Compare floats up to a given relative error. *)
val cmp_float : ?epsilon: float -> float -> float -> bool

(** {5 Bracket}

    A bracket is a functional implementation of the commonly used
    setUp and tearDown feature in unittests. It can be used like this:

    "MyTestCase" >:: (bracket test_set_up test_fun test_tear_down) *)

(** *)
val bracket : (unit -> 'a) -> ('a -> 'b) -> ('a -> 'c) -> unit -> 'c

(** {5 Constructing Tests} *)

(** The type of test function *)
type test_fun = unit -> unit

(** The type of tests *)
type test =
    TestCase of test_fun
  | TestList of test list
  | TestLabel of string * test

(** Create a TestLabel for a test *)
val (>:) : string -> test -> test

(** Create a TestLabel for a TestCase *)
val (>::) : string -> test_fun -> test

(** Create a TestLabel for a TestList *)
val (>:::) : string -> test list -> test

(** Some shorthands which allows easy test construction.

   Examples:

   - ["test1" >: TestCase((fun _ -> ()))] =>  
   [TestLabel("test2", TestCase((fun _ -> ())))]
   - ["test2" >:: (fun _ -> ())] => 
   [TestLabel("test2", TestCase((fun _ -> ())))]

   - ["test-suite" >::: ["test2" >:: (fun _ -> ());]] =>
   [TestLabel("test-suite", TestSuite([TestLabel("test2", TestCase((fun _ -> ())))]))]
*)

(** [test_decorate g tst] Apply [g] to test function contains in [tst] tree. *)
val test_decorate : (test_fun -> test_fun) -> test -> test

(** [test_filter paths tst] Filter test based on their path string representation. *)
val test_filter : string list -> test -> test option

(** {5 Retrieve Information from Tests} *)

(** Returns the number of available test cases *)
val test_case_count : test -> int

(** Types which represent the path of a test *)
type node = ListItem of int | Label of string
type path = node list (** The path to the test (in reverse order). *)

(** Make a string from a node *)
val string_of_node : node -> string

(** Make a string from a path. The path will be reversed before it is 
    tranlated into a string *)
val string_of_path : path -> string

(** Returns a list with paths of the test *)
val test_case_paths : test -> path list

(** {5 Performing Tests} *)

(** The possible results of a test *)
type test_result =
    RSuccess of path
  | RFailure of path * string
  | RError of path * string
  | RSkip of path * string
  | RTodo of path * string

(** Events which occur during a test run *)   
type test_event =
    EStart of path 
  | EEnd of path
  | EResult of test_result

(** Perform the test, allows you to build your own test runner *)
val perform_test : (test_event -> 'a) -> test -> test_result list

(** A simple text based test runner. It prints out information
    during the test. *)
val run_test_tt : ?verbose:bool -> test -> test_result list

(** Main version of the text based test runner. It reads the supplied command 
    line arguments to set the verbose level and limit the number of test to run
  *)
val run_test_tt_main : test -> test_result list
