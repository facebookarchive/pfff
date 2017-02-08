[![Build Status](https://travis-ci.org/alavrik/piqi-ocaml.png)](https://travis-ci.org/alavrik/piqi-ocaml)


Piqi is a multi-format data serialization system for OCaml. It provides a
uniform interface for serializing OCaml data structures to JSON, XML and
Protocol Buffers formats.


A typical Piqi usage scenario involves the following steps:

**1. Install piqi-ocaml** -- see installation instructions below.


**2. Describe data structures using the Piqi data definition language or
Protocol Buffers `.proto` files**

The [Piqi](http://piqi.org/doc/piqi/) data definition language can describe many
OCaml types, both primitive and user-defined. This includes integers, floats,
booleans, strings, binaries, lists, records and polymorphic variants.

`.piqi` modules can be converted to and from Protocol Buffers `.proto` files:

    piqi to-proto X.piqi
    piqi of-proto X.proto


**3. Call the Piqi compiler (piqic-ocaml) to generate OCaml type definitions and
serialization code**

    piqic-ocaml X.piqi


**4. Use generated serializes/deserializers in a user's program** -- the desired
serialization format can be specified at runtime. For
[examples](examples/addressbook/io_json_xml_pb.ml):


    % deserialize a data structure from Protocol Buffers
    let buf = Piqirun.init_from_string bytes in
    let addressbook = Addressbook_piqi.parse_address_book buf in ...

    % serialize it as JSON
    let json = Addressbook_piqi_ext.gen_address_book addressbook `json in ...

    % serialize it as pretty-printed JSON
    let json_pretty = Addressbook_piqi_ext.gen_address_book addressbook `json_pretty in ...

    % serialize it as XML
    let xml = Addressbook_piqi_ext.gen_address_book addressbook `xml in ...


Examples
--------

See [examples/addressbook](examples/addressbook/) and other projects in the
[examples](examples/) directory.


Installation
------------

### Installing using OPAM

In order to install Piqi using [OPAM](http://opam.ocamlpro.com/), run the
following command:

    opam install piqi

This command will install the latest stable version of Piqi that includes `piqi`
and `piqic-ocaml` executables and runtime libraries for OCaml.

To install the latest development version of Piqi, follow the instructions at
https://github.com/piqi/piqi-opam-repo


### Installing from source code
-------------------------------

1. Download and install [piqi and piqilib](http://github.com/alavrik/piqi)

  Follow general build and installation instructions from the INSTALL file.

  After that, build and install the `piqilib` OCaml library by running

      make ocaml
      make ocaml-install


2. Build and install `piqi-ocaml`

  ```
  make
  make install
  ```

To uninstall:

    make uninstall


Documentation
-------------

Piqi OCaml documentation is available at http://piqi.org/doc/ocaml/

The master copy is located in this repository:
[doc/piqi-ocaml.md](doc/piqi-ocaml.md)


Bugs
----

Please report found problems using [GitHub
issues](http://github.com/alavrik/piqi-ocaml/issues).


Mailing list
------------

http://groups.google.com/group/piqi


Contributing
------------

Your contributions are always welcome. Just open a pull request. Check [TODO
list](TODO) for ideas.


Some useful commands:

    make -C tests
    make -C tests clean


License
-------

[Apache License Version 2.0](LICENSE)

