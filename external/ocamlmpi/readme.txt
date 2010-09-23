This is OcamlMPI version 1.01, a Caml binding to the Message Passing
Interface (MPI).


WHAT IS MPI?

MPI is a popular library for distributed-memory parallel programming
in SPMD (single program, multiple data) style.

MPI offers both point-to-point message passing and group communication
operations (broadcast, scatter/gather, etc).

Several implementations of MPI are available, both for networks of
Unix workstations and for supercomputers with specialized communication
networks.

More info on MPI is available at:

     http://www.mcs.anl.gov/mpi/index.html

MPICH is a popular, free implementation of MPI for network of
Unix workstations.  It is available at:

     http://www.mcs.anl.gov/mpi/mpich/index.html


THE OCAMLMPI INTERFACE:

OCamlMPI provides Caml bindings for a large subset of MPI functions.
I omitted a number of MPI functions for which I had no use, though.
The file mpi.mli in this directory lists the MPI functions provided,
along with short documentation.  See the MPI docs at the URLs above
for more detailed info.

Most communication functions come in five flavors:
- one generic function operating on any data type  (e.g. Mpi.send)
- four specialized functions for the following types:
              int         (Mpi.send_int)
              float       (Mpi.send_float
              int array   (Mpi.send_int_array)
              float array (Mpi.send_float_array)
The generic function is simpler to use, and more general, but involves
more overhead than the specialized functions.

The data types that can be transmitted using the "generic"
communication functions are those that can be marshaled by the
Marshal.to_channel function (q.v.) with the Marshal.Closures option.
That is:
  - all concrete data structures (base types, arrays, records, variant types)
  - function closures
  - but not objects
  - nor certain abstract types (in_channel, out_channel, Graphics.image).


BUILDING OCAMLMPI:

Edit the Makefile and set the MPIINCDIR, MPILIBDIR and DESTDIR
variables to the right directories:

MPIINCDIR    directory containing the MPI include file <mpi.h>
MPILIBDIR    directory containing the MPI library -lmpi
DESTDIR      where to install the OcamlMPI library

Also adjust CC and CFLAGS if necessary.  Then do "make all".

For final installation: become super-user and do "make install".


TESTING OCAMLMPI:

There are two test programs included:
      testmpi is a regression test
      test_mandel is a parallel Mandelbrot set plotter


USING OCAMLMPI:

Bytecode:
        ocamlc -I +ocamlmpi mpi.cma <your files>

Native-code:
        ocamlopt -I +ocamlmpi mpi.cmxa <your files>


LICENSING:

The OCamlMPI library is copyright 1998 INRIA and distributed under the
terms of the GNU Library General Public License version 2, with a
special exception on clause 6 described in the file LICENSE.


AUTHOR AND SUPPORT:

Written by Xavier Leroy <Xavier.Leroy@inria.fr>.  Bug reports are
welcome, but I don't guarantee any support for this code.  It is
provided "as is".

Questions about the following issues should be directed to MPI
newsgroups, mailing-lists or implementation vendors, but not to me:
semantics of MPI functions, how to program with MPI, finding and
installing implementations of MPI, performance tuning of MPI
applications, etc.


