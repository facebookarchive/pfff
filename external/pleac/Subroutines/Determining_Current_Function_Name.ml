(* ********************************************************************** *)
(* Determining Current Function Name *)
(* ********************************************************************** *)
let pleac_Determining_Current_Function_Name () = 
  (* The names of functions are not available at runtime. However, using the
     camlp4 preprocessor, we can expose various pieces of static information.
  
     The "macro" parser provides the current file and location as __FILE__ and
     __LOCATION__, respectively. With a bit more customization, we can expose
     the current function name as well.
  
     To do this, we'll make a copy of camlp4/Camlp4Filters/Camlp4Profiler.ml
     from the OCaml sources and rename it to "Camlp4FuncNamer.ml". Then,
     we'll change the definition of "decorate_this_expr" to the following: *)
  
  (*---------------------------*)
  
  value decorate_this_expr e id =
    let _loc = Ast.loc_of_expr e in
    <:expr< let __FUNC__ = $`str:id$ in $e$ >>;
  
  (*---------------------------*)
  
  (* This has the effect of exposing the current function name as the
     string, __FUNC__, which we can use just like __FILE__. To build this
     syntax extension, use a command like the following: *)
  
  ocamlc -c -pp camlp4rf -I /usr/lib/ocaml/3.10.2/camlp4 Camlp4FuncNamer.ml
  
  (*---------------------------*)
  
  (* Now, we'll write a simple test program called "main.ml": *)
  
  (* Comment out this line to silence log messages. *)
  DEFINE DEBUG
  
  (* Default function name if Camlp4FuncNamer doesn't provide one. *)
  let __FUNC__ = "<toplevel>"
  
  (* Log macro with Printf formatting. *)
  DEFINE LOG =
    IFDEF DEBUG THEN
      Printf.kprintf
        (Printf.eprintf "%s[%s]: %s\n%!" __FUNC__ __FILE__)
    ELSE
      Printf.kprintf (fun _ -> ())
    END
  
  (* An example named function. *)
  let test_function () =
    let str = "Hello, world!" in
    let num = 42 in
    LOG "str=\"%s\", num=%d" str num;
    print_endline "test complete"
  
  (* Some code to run at the toplevel. *)
  let () =
    LOG "not in a function";
    test_function ()
  
  (*---------------------------*)
  
  (* We can compile this program as follows: *)
  
  ocamlc -pp "camlp4of Camlp4FuncNamer.cmo" \
      -I /usr/lib/ocaml/3.10.2/camlp4 \
      -o main main.ml
  
  (* Running it, we get this output: *)
  
  <toplevel>[main.ml]: not in a function
  test_function[main.ml]: str="Hello, world!", num=42
  test complete
  

