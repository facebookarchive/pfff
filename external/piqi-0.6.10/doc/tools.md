# NAME

piqi - a collection of tools for working with structured data and Piqi schemas

# SYNOPSIS

piqi [*command*] [*options*]

# DESCRIPTION

Piqi is a universal schema language and a collection of tools built around it.

The Piqi language can be used to define schemas for JSON, XML, Google Protocol
Buffers and some other data formats.

`piqi` is a command-line program that implements a set of sub-commands:

- for validating, pretty-printing and converting data between JSON, XML,
  Protocol Buffers and Piq formats.

- for working with the schemas, such as converting definitions between Piqi
  (`.piqi`) and Protocol Buffes (`.proto`), and "compiling" Piqi definitions
  into one of the supported portable data representation formats (JSON, XML,
  Protocol Buffers).

### Common options

This is the list of common options which is supported by most of `piqi`
subcommands.

`-I <dir>`
:   Add directory to the list of Piqi search paths. Search path specifies where
    to search for included or imported modules defined in `.piqi` or
    `.proto.piqi` files.

    This option can be specified several times.

    The list of search paths can be also specified using the `PIQI_PATH`
    environment variable. See the ENVIRONMENT section below.

`--no-warnings`
:   Don't print warnings.

`--trace`
:   Turn on tracing.

`--debug <level>`
:   Specify debug level; any number greater than 0 turns on debug messages.

`--no-builtin-types`
:   Don't include built-in types while processing loaded Piqi modules

`--help`
:   Display the list of options.

`--version`
:   Print Piqi version and exit.

`--`
:   This sequence delimits the list of options, the remaining arguments will be
    treated as positional arguments.

    For instance, if placed after `--` argument, `-` can be used for
    specifying `stdin/stdout` as input or output files.


### piqi convert

Usage: `piqi convert [options] [input file] [output file]`

Converts structured data between `piq`, `json`, `xml`, `pb` (Protocol Buffers)
formats.

It also can be used to convert `.piqi` to `piq`, `json`, or `pib` formats by
embedding Piqi specification into the output data stream.

Piq encodings are described [here](/doc/encodings/).

If both input and output files are not specified `stdin` and `stdout` will be
used respectively.

However, if an input file is specified without specifying the output file, the
output file name will be implicitly set to `<input file name>.<encoding name>`.

Options:

`-o <output file>`
:   Alternative method for specifying output file; use `-` for `stdout`.

`-f pb|json|xml|piq|pib`
:   Specify input encoding. If not specified, input encoding will be chosen
    based on input file's extension.

`-t pb|json|xml|piq|pib`
:   Specify output encoding. `piq` encoding is used by default.

`--type <typename>`
:   Specify the type of converted object when converting from `pb` or `json`
    encodings, as these formats do not contain information about types. For
    other input formats, this parameter defines the default object type.

    `<typename>` should be a fully qualified Piqi typename of the form
    `<module name>/<type name>`.

    If an input `pb` or `json` stream contains embedded Piqi module(s), a
    special `--type piqi` value should be used.

`--add-defaults`
:   Add field default values while converting records.

    If an optional field specifies default value and the field value in the
    input record is unspecified, the output field will be set to the default
    value.

`--embed-piqi`
:   Include data definitions in a form of embedded Piqi modules into the data
    stream.

    Piq data streams represented in `piq`, `json` and `pib` formats can contain
    Piqi modules embedded in the data stream.

    `--embed-piqi` flag tells `piqi convert` to embed all Piqi modules, which
    the input data depends on, into the output stream. Such dependencies include
    all Piqi modules that define types for the data contained in the stream and
    all the modules which the first-level dependencies include or import.

    This way, using this flag, one can produce a self-describing data bundle
    that includes full description of the data contained in the stream.

    If the flag is used when converting from `.piqi`, all dependencies of the
    converted module will be included in the stream.

`--json-omit-missing-fields true|false`
:   Whether to omit missing optional and empty repeated fields from JSON output
    instead of representing them as {"field_name": null} and {"field_name", []}
    JSON fields. Default setting is `true`.

`--strict`
:   Treat unknown and duplicate fields as errors when parsing JSON, XML and Piq
    formats.

`-e <extension-name>`
:   Automatically include [extension modules](/doc/piqi#extensionmodules)
    `<extension-name>` when loading .piqi files.

### piqi check

Usage: `piqi check [options] <.piqi|.piq file>`

Checks .piq and .piqi validity.

Returns 0 if the file is valid.

`--type <typename>`
:   Specify the default object type when reading data from .piq files.

    `<typename>` should be a fully qualified Piqi typename of the form
    `<module name>/<type name>`.

`--strict`
:   Treat unknown and duplicate fields as errors when parsing JSON, XML and Piq
    formats.

`-e <extension-name>`
:   Automatically include [extension modules](/doc/piqi#extensionmodules)
    `<extension-name>` when loading .piqi files.

`--piq-relaxed-parsing true|false`
:   Parse Piq format using "relaxed" mode (default=false)

    See `piqi convert` for more details.


### piqi pp

Usage: `piqi pp [options] [<.piqi|.piq file>] [output file]`

Pretty-prints .piq and .piqi files.

**NOTE:** this command doesn't check type validity.

If input or output file are not specified `stdin` and `stdout` will be used
respectively.

Options:

`-o <output file>`
:   Alternative method for specifying output file; use `-` for `stdout`.

`--normalize-words`
:   Normalize all words while pretty-printing: convert all "CamelCase" Piq words
    to "camel-case" format.

`--expand-abbr`
:   Expand built-in syntax abbreviations. See Piq documentation for details.

`--piq-relaxed-parsing true|false`
:   Parse Piq format using "relaxed" mode (default=false)

    See `piqi convert` for more details.


### piqi json-pp

Usage: `piqi json-pp [options] [<.json file>] [output file]`

Pretty-prints JSON files. Input file may contain several properly formated JSON
objects represented as UTF-8 text as defined by [RFC
4627](http://www.ietf.org/rfc/rfc4627.txt).

**NOTE:** this command doesn't check type validity.

If input or output file are not specified `stdin` and `stdout` will be used
respectively.

Options:

`-o <output file>`
:   Alternative method for specifying output file; use `-` for `stdout`.

`--indent`
:   Use indentation instead of pretty-printing


### piqi expand

Usage: `piqi expand [options] <.piqi file> [output file]`

Include all included `.piqi` and, by default, apply all extensions in order to
get a single `.piqi` specifications from several dependent `.piqi` modules.

`-o <output file>`
:   Alternative method for specifying output file; use `-` for `stdout`.

`--includes-only`
:   Expand only includes (don't expand extensions).

`--functions`
:   Removes embedded typedefs from function parameters and turns them into
    correspondent top-level definitions.

`--extensions`
:   Only expand extensions and includes (this is the default behavior).

`--all`
:   Equivalent to specifying both `--extensions` and `--functions`.

`--add-module-name`
:   Add module name if it wasn't originally present

`-e <extension-name>`
:   Automatically include [extension modules](/doc/piqi#extensionmodules)
    `<extension-name>` when loading .piqi files.


### piqi to-proto

Usage: `piqi to-proto [options] <.piqi file>`

Converts `.piqi` file to `.piqi.proto`

The conversion rules are specified [here](/doc/protobuf/#piqitoprotomapping).

Options:

`-o <output file>`
:   Specify an alternative output file name instead of `%.piqi.proto`.

### piqi of-proto

Usage: `piqi of-proto [options] <.proto file>`

Converts `.proto` file to `.proto.piqi`

The conversion rules are specified [here](/doc/protobuf/#prototopiqimapping).

Options:

`-o <output file>`
:   Specify an alternative output file name instead of `%.proto.piqi`.

`--normalize`
:   Convert "CamelCase" identifiers in Proto specification into "camel-case"
    format.

`--convert-groups`
:   Convert Protocol Buffers Group definitions to Piqi records definitions.

    The resulting Piqi specification will be valid, but not compatible with the
    initial Proto specification.

    Groups are deprecated in Protocol Buffers and not supported by Piqi.

`--leave-tmp-files`
:   Don't delete temporary files created during command execution. This option
    is useful for debugging.

`--strict`
:   Treat unknown and duplicate fields as errors when parsing the Piqi spec


### piqi light

Usage: `piqi light [options] [<.piqi file>] [output-file]`

Prints `.piqi` file using [Piqi-light syntax](/doc/piqi/#piqilightsyntax).

Options:

`-o <output file>`
:   Alternative method for specifying output file; use `-` for `stdout`.

### piqi getopt

Usage: `piqi getopt [options] -- [<data arguments>]`

Interprets command-line arguments as typed data, and outputs it in various
formats.

For description of command-line argument syntax and the way how arguments are
parsed see correspondent [section](/doc/getopt/) of the current documentation.

Options:

`-o <output file>`
:   Specify output file; use `-` for `stdout`. If no `-o` option is given,
    `stdout` is used by default.

`-t pb|json|xml|piq|pib`
:   Specify output encoding. `piq` encoding is used by default.

    Requires `--type` option.

    If `-t` option is not used, Piq AST will be produced instead of the
    converted data object. This mode is useful for debugging and understanding
    how Piqi parses command-line arguments.

`--type <typename>`
:   Specify the name of the expected data type.

    `<typename>` should be a fully qualified Piqi typename of the form
    `<module name>/<type name>`.

    (This option is applied only when `-t` option is used.)

`--add-defaults`
:   Add field default values while converting records.

    If an optional field specifies default value and the field value in the
    input record is unspecified, the output field will be set to the default
    value.

    (This option is applied only when `-t` option is used.)


`--gen-extended-piqi-any`
:   Use extended representation of `piqi-any` values in XML and JSON output.

    When specified, an extended version of `piqi-any` representation is used in
    the conversion result. In addition to the original JSON or XML value, it
    includes Piqi type name (if known), Protobuf representation (if known or can
    be derived), and a special marker indicating that this is an extended
    piqi-any representation.

    For example, this flag changes relevant portion of "piqi convert -t json
    piqi.piqi" output from

       "default": "required",

    to

       "default": {
          "piqi_type": "piqi-any",
          "type": "piqi/field-mode",
          "protobuf": "CN+iipMB",
          "json": "required"
        },

`--strict`
:   Treat unknown and duplicate options as errors

`--piq-frameless-output true|false`
:   Print a frame (i.e. :<typename> []) around a single output Piq object
    (default=false)

`--piq-frameless-input true|false`
:   Expect a frame around a single input Piq object (default=false)

`--piq-relaxed-parsing true|false`
:   Parse Piq format using "relaxed" mode (default=false)

    For instance, when set to `true`, single-word string literals don't have to
    be quoted.


### piqi call

Usage: piqi call [options] \<URL\> -- [call arguments]

Piqi-RPC native client.

It interprets command-line arguments as input parameters for a remote function,
converts them into a Protobuf-encoded data object and executes a Piqi-RPC remote
function call.

In addition to calling a remote function, it can fetch Piqi specifications of
the remote service and print them in several formats: Piqi (`--piqi` flag),
Piqi-light (`-p` flag) and getopt-style help for remote functions (`-h` flag).

`<URL>` is either an HTTP URL or a path to a local executable. HTTP URL must
start with `http://` or `https://`. Everything else will be considered as a path
to a local command, i.e. *local URL*.

In case of HTTP URL, a remote call will be performed by sending an HTTP `POST`
request that contains input arguments in the request's body.

In case of a *local \<URL\>*, the correspondent program will be started, the
function will be called using Piqi-RPC-over-pipe protocol, and the program will
be shut down. This mode is intended mainly for debugging low-level Piqi-RPC
services that run locally.

Remote function's output can be printed in a variety of different formats: JSON,
XML, Protobuf, Piq (see `-t` option). Returned application errors (i.e. *error*
function parameter) will be printed to `stderr` in the requested format.

More details can be found in Piqi-RPC [documentation](/doc/piqi-rpc/).

Options:

`-o <output file>`
:   Specify output file; use `-` for `stdout`. If no `-o` option is given,
    `stdout` is used by default.

    However, `stderr` is always used for printing all kinds of errors.

`-t pb|json|xml|piq|pib`
:   Specify encoding for the function's output parameters. `piq` encoding is
    used by default.

`--piqi`
:   Instead of calling a function, only print the Piqi module that defines the
    service.

`--piqi-all`
:   Similar to `--piqi`, but print the Piqi module that defines the service and
    all its dependencies.

`p` | `--piqi-light`
:   Similar to `--piqi`, but print the Piqi module using Piqi-light syntax.

`-h`
:   Similar to `--piqi`, but print command-line usage help for remote Piqi-RPC
    functions. Printed help is automatically generated from the Piqi
    specification.

`--strict`
:   Treat unknown and duplicate options as errors


# ENVIRONMENT

`PIQI_TRACE`
:   Definition of this environment variable has the same effect as specifying
    `--trace` command-line option.

`PIQI_PATH`
:   Specifies directory paths where to search for `.piqi` or `.proto.piqi`
    specifications. Several paths can be specified separated by `:`.

    You can also specify search paths using the `-I` command-line option.


# KNOWN PROBLEMS

-   Currently there are no checks for integer overflows while reading and
    writing Piq data in various formats. If an integer value doesn't fit into
    the range of the specified integer type, it will be silently stripped down.

-   `piqi of-proto` doesn't work correctly on Google Protobuf specifications
    which rely on groups (groups are deprecated in Protocol Buffers and not
    supported by Piqi).

    For example, Piqi fails to convert the following `.proto` file from Google
    Protocol Buffers source distribution:

        piqi of-proto google/protobuf/unittest_custom_options.proto


# EXAMPLES

For examples of Piqi tools usage, visit [Examples](/examples/) page or take a
look at `test_piq`, `test_piqi` and `test_piqi_proto` shell scripts from
[examples directory](http://github.com/alavrik/piqi/tree/master/examples/) in
Piqi source distribution.
