(* ********************************************************************** *)
(* Using h2ph to Translate C #include Files *)
(* ********************************************************************** *)
let pleac_Using_h2ph_to_Translate_C_#include_Files () = 
  (* There are several tools for translating C header files to OCaml
     bindings, many of which can be found at The Caml Hump:
  
     http://caml.inria.fr/cgi-bin/hump.en.cgi?sort=0&browse=42
  
     Of the available tools, "ocamlffi" (also known as simply "FFI") seems
     to work best at accomplishing the task of parsing header files, but
     it has not been maintained in many years and cannot handle the deep
     use of preprocessor macros in today's Unix headers. As a result, it is
     often necessary to create a header file by hand, and so long as this
     is required, better results can be achieved with Xavier Leroy's
     CamlIDL tool. CamlIDL can be found here:
  
     http://caml.inria.fr/pub/old_caml_site/camlidl/
  
     The following recipes will use CamlIDL. First, we'll wrap the Unix
     "gettimeofday" system call by writing the following to a file named
     "time.idl": *)
  
  quote(C,"#include <sys/time.h>");
  
  struct timeval {
      [int32] int tv_sec;
      [int32] int tv_usec;
  };
  
  struct timezone {
      int tz_minuteswest;
      int tz_dsttime;
  };
  
  int gettimeofday([out] struct timeval *tv, [in] struct timezone *tz);
  
  (* We can now build three files, "time.ml", "time.mli", and
     "time_stubs.c", corresponding to the OCaml implementation, OCaml
     interface, and OCaml-to-C stubs, by running the following command: *)
  
  $ camlidl -no-include time.idl
  
  (* CamlIDL automatically translates the two structs defined in the IDL
     into OCaml record types and builds an external function reference
     for "gettimeofday", resulting in the following generated OCaml
     implementation in "time.ml": *)
  
  (* File generated from time.idl *)
  
  type timeval = {
    tv_sec: int32;
    tv_usec: int32;
  }
  and timezone = {
    tz_minuteswest: int;
    tz_dsttime: int;
  }
  
  external gettimeofday : timezone option -> int * timeval
  	= "camlidl_time_gettimeofday"
  
  (* Now, we can use "ocamlc -c" as a front-end to the C compiler to build
     the stubs, producing time_stubs.o. *)
  
  $ ocamlc -c time_stubs.c
  
  (* The OCaml source can be built and packed into a library along with
     the compiled stubs using "ocamlc -a": *)
  
  $ ocamlc -a -custom -o time.cma time.mli time.ml time_stubs.o \
  		-cclib -lcamlidl
  
  (* Finally, we can write a simple test program to use our newly-exposed
     "gettimeofday" function. *)
  
  (* test.ml *)
  let time () =
    let res, {Time.tv_sec=seconds; tv_usec=microseconds} =
      Time.gettimeofday None in
    Int32.to_float seconds +. (Int32.to_float microseconds /. 1_000_000.)
  let () = Printf.printf "%f\n" (time ())
  
  (* Compiling this test program is straightforward. *)
  
  $ ocamlc -o test time.cma test.ml
  
  (* Running it produces the current time with millisecond precision. *)
  
  $ ./test
  1217173408.931277
  
  (*---------------------------*)
  
  (* The next two recipes will wrap the Unix "ioctl" function, allowing
     us to make a few low-level I/O system calls. To make things easier,
     we'll use the following Makefile (make sure you use tabs, not spaces,
     if you cut and paste this code): *)
  
  all: jam winsz
  
  jam: ioctl.cma jam.ml
  	ocamlc -o jam ioctl.cma jam.ml
  
  winsz: ioctl.cma winsz.ml
  	ocamlc -o winsz ioctl.cma winsz.ml
  
  ioctl.cma: ioctl.mli ioctl.ml ioctl_stubs.o
  	ocamlc -a -custom -o ioctl.cma ioctl.mli ioctl.ml ioctl_stubs.o \
  		-cclib -lcamlidl
  
  ioctl_stubs.o: ioctl_stubs.c
  	ocamlc -c ioctl_stubs.c
  
  ioctl.mli ioctl.ml ioctl_stubs.c: ioctl.idl
  	camlidl -no-include ioctl.idl
  
  clean:
  	rm -f *.cma *.cmi *.cmo *.c *.o ioctl.ml ioctl.mli jam winsz
  
  (*---------------------------*)
  
  (* ioctl.idl: *)
  
  quote(C,"#include <sys/ioctl.h>");
  
  enum ioctl {
      TIOCSTI,
      TIOCGWINSZ,
  };
  
  int ioctl([in] int fd,
            [in] enum ioctl request,
            [in, out, string] char *argp);
  
  (*---------------------------*)
  
  (* jam - stuff characters down STDIN's throat *)
  
  (* Simulate input on a given terminal. *)
  let jam ?(tty=0) s =
    String.iter
      (fun c -> ignore (Ioctl.ioctl tty Ioctl.TIOCSTI (String.make 1 c))) s
  
  (* Stuff command-line arguments into STDIN. *)
  let () = jam (String.concat " " (List.tl (Array.to_list (Sys.argv))))
  
  (*---------------------------*)
  
  (* winsz - find x and y for chars and pixels *)
  
  (* Decode a little-endian short integer from a string and offset. *)
  let decode_short s i =
    Char.code s.[i] lor Char.code s.[i + 1] lsl 8
  
  (* Read and display the window size. *)
  let () =
    let winsize = String.make 8 '\000' in
    ignore (Ioctl.ioctl 0 Ioctl.TIOCGWINSZ winsize);
    let row = decode_short winsize 0 in
    let col = decode_short winsize 2 in
    let xpixel = decode_short winsize 4 in
    let ypixel = decode_short winsize 6 in
    Printf.printf "(row,col) = (%d,%d)" row col;
    if xpixel <> 0 || ypixel <> 0
    then Printf.printf "  (xpixel,ypixel) = (%d,%d)" xpixel ypixel;
    print_newline ()
  

