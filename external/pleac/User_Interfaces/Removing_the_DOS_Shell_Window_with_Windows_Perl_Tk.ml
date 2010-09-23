(* ********************************************************************** *)
(* Removing the DOS Shell Window with Windows Perl/Tk *)
(* ********************************************************************** *)
let pleac_Removing_the_DOS_Shell_Window_with_Windows_Perl_Tk () = 
  (* Use Harry Chomsky's mkwinapp.ml from the OCaml-Win32 project:
  
     http://ocaml-win32.sourceforge.net/
  
     Compile your program using the native compiler and run mkwinapp.exe
     on the result. *)
  
  C:\MyProg> ocamlopt myprog.ml -o myprog.exe
  C:\MyProg> ocamlopt unix.cmxa mkwinapp.ml -o mkwinapp.exe
  C:\MyProg> mkwinapp myprog.exe
  
  (* Now you can run "myprog" and you won't get a console window. *)
  

