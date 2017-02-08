[![Build Status](https://travis-ci.org/alavrik/piqi.png)](https://travis-ci.org/alavrik/piqi)

[Piqi](http://piqi.org) is a universal schema language and a collection of tools
built around it.

[The Piqi language](http://piqi.org/doc/piqi/) can be used to define schemas for
JSON, XML, [Google Protocol Buffers](http://code.google.com/p/protobuf/) and
some other data formats.

[`piqi`](http://piqi.org/doc/tools/) is a command-line program that exposes some
of the tools:

- for validating, pretty-printing and converting data between JSON, XML,
  Protocol Buffers and Piq formats.

- for working with the schemas, such as converting definitions between Piqi
  (`.piqi`) and Protocol Buffes (`.proto`), and "compiling" Piqi definitions
  into one of the supported portable data representation formats (JSON, XML,
  Protocol Buffers).

Other Piqi sub-projects include:

- A multi-format (JSON, XML, Protocol Buffers) data serialization system for
  [Erlang](https://github.com/alavrik/piqi-erlang) and
  [OCaml](https://github.com/alavrik/piqi-ocaml).

- [Piq](http://piqi.org/doc/piq/) -- a human-friendly typed data representation
  language. It is designed to be more convenient for viewing and editing data
  compared to JSON, XML, CSV, S-expressions and other formats.

- [Piqi-RPC](https://github.com/alavrik/piqi-rpc/) -- an RPC-over-HTTP system
  for Erlang. It provides a simple way to expose Erlang services via JSON, XML
  and Protocol Buffers over HTTP.

The Piqi project was inspired by Google Protocol Buffers and designed to be
largely compatible with it. Like Protocol Buffers, Piqi relies on type
definitions and supports schema evolution. The main differences is that Piqi has
a richer data model, high-level modules, standard mappings to JSON and XML, and
comes with a powerful data representation format (Piq). Also, Piqi is a lot more
extensible.

Full project description and documentation can be found at
[http://piqi.org](http://piqi.org)


Installation
------------

See [INSTALL](INSTALL) for the installation instructions.


Bug tracker
-----------

If you found a bug or have any suggestions please submit a GitHub issue:
[https://github.com/alavrik/piqi/issues](https://github.com/alavrik/piqi/issues)


Contributing
------------

Participation and patches are very welcome! Check [TODO list](TODO) for ideas.

The best way to submit a contribution is to open a pull request on GitHub
against the `master` branch.


Mailing list
------------

For discussions about the usage, development, and future of Piqi there is the
Piqi Google Group:
[http://groups.google.com/group/piqi](http://groups.google.com/group/piqi)


License
-------

[Apache License Version 2.0](LICENSE)


Files
-----

    src/                  "piqi" command-line utility
    piqilib/              common library used by piqi command-line tool and piqi-ocaml
    piqi/                 Piqi self-specification
    doc/                  project documentation

    make/                 makefiles and build scripts
    deps/                 third-party dependencies downloaded and built by `make deps`

