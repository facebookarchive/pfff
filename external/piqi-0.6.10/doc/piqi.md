Overview
--------

Piqi is a universal high-level data definition language.

At the moment, it works with 4 different [data formats](/doc/encodings/)
including JSON, XML, Protocol Buffers and [Piq](/doc/piq/) and has mappings to
[OCaml](/doc/ocaml/), [Erlang](/doc/erlang/) and [Protocol
Buffers](/doc/protobuf/) `.proto` definitions.


Below is a brief overview of Piqi features.

-   Piqi supports a rich set of types.

    primitive types:
    :   boolean, integer, single- and double-precision floating point number,
        string (Unicode string), binary (byte array)

    user defined types:
    :   record, variant ([tagged
        union](http://en.wikipedia.org/wiki/Tagged_union)), enum, list and
        alias.

-   Piqi has high-level modules.

    Piqi modules provide several mechanisms for reusing type definitions from
    other modules:

    imports
    :   A module can *import* another module. It allows to use types defined in
        the imported module in local type definitions.

    includes
    :   A module can *include* another module to reuse all of its type
        definitions, imports and extensions as if they where defined locally.


-   Piqi supports type extensions and data schema evolution.

    All Piqi types can be extended either directly or through *extensions*
    mechanism:

    -   more fields can be added to a `record` definition

    -   more options can be added to a `variant` or `enum` definition

    -   more properties can be assigned to an `alias` type

    Backward and forward schema compatibility can be maintained by using Google
    Protocol Buffers approach when a unique integer number is assigned to each
    field in a record (read more about it in Google Protocol Buffers
    documentation).

    If schema compatibility is not needed (e.g. during prototyping stage), Piqi
    can assign field numbers automatically. Piqi can also automatically assign
    `enum` values.


-   Piqi is self-defined and portable.

    Piqi module data structure is [defined](/self-spec/) using the Piqi
    language itself.

    As a result, a Piqi module can be converted into a portable JSON, XML or
    Protocol Buffers representation.

    One of interesting Piqi properties is that the language implementation takes
    its own high-level specification, written in Piqi, and parses the language
    into AST without any hand-written parsing rules -- it is all fully
    automated.


-   The Piqi language is extensible.

    The Piqi language is based on a general-purpose data representation language
    called [Piq](/doc/piq/).

    Now, let's look at the following three properties.

    - Piqi module is just a data structure represented in Piq format (or any
      other supported portable formats such as JSON), meaning that there's no
      concrete language syntax.
    - the Piqi module data structure is described using the same language
    - the Piqi data definition language supports schema extensions in a
      backward-compatible way

    This means that once we extend the Piqi self-specification, we instantly
    have those extensions in the language: there is no need to design new syntax
    elements, update parsing code and transform AST into intermediate language.
    Moreover, the new extensions are fully transparent from the core Piqi
    implementation standpoint, because extensions are backward-compatible.


Piqi borrows many concepts from Google Protocol Buffers which, at the moment, is
much better documented than Piqi. It may be useful to get familiar with Protocol
Buffers along with reading Piqi documentation.

For those who are familiar with Google Protocol Buffers, information about its
compatibility with Piqi can be found on [this page](/doc/protobuf/).

Some examples of Piqi specifications can be found [here](/examples/). The most
complex Piqi specification example is the Piqi
[self-specification](/self-spec/).


Lexical conventions
-------------------

The Piqi language described in the remaining part of the document is based on
Piq syntax which is specified [here](/doc/piq/).

In addition to general Piq rules, Piqi relies on some extra syntax elements,
such as identifiers that are used for type names, field name, option names and
so on.

Piqi identifier has the following format:

    <identifier> ::= ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-']*

Piqi identifiers are case-sensitive.

**NOTE:** use of underscore (`_`) characters in Piqi identifiers is not allowed.
Hyphens (`-`) should be used instead.

Two or more consecutive `-` characters are now allowed. Also, identifiers can
not begin and end with `-`.

\`true\` and \`false\` are reserved for boolean literals -- they can not be used
as identifiers. As you will see in the following sections, this makes them the
only keywords in the language.


Modules
-------

Piqi modules are defined as non-empty files with `.piqi` extensions. A `.piqi`
file represents one Piqi module.

Piqi modules converted from Google Protocol Buffer specification have
`.proto.piqi` file extension.

`.piqi` and `.proto.piqi` are the only file extensions allowed for Piqi modules.
Other extensions are not recognized by [Piqi tools](/doc/tools/). (When the Piqi
implementation resolves Piqi types and Piqi module names it searches for files
with `.piqi` or `.proto.piqi` extensions using module search paths.)

There are also several restrictions to `.piqi` file names, because Piqi module
names are usually derived from the file names. See the next section for details.

Each Piqi module can contain the following entries:

-   module name

-   import directives

-   include directives

-   type definitions

-   type extension directives

-   function definitions


In addition, Piqi module can include one or more `custom-field` top-level
properties. They are used to prevent "Unknown field" warning messages about
fields that are not natively supported by the Piqi implementation. For example,
including

    .custom-field ocaml-name

will tell [Piqi tools](/doc/tools/) to ignore properties like `.ocaml-name
"foo"` as they are only meaningful to `piqic-ocaml`.


### Module names

Piqi module names consist of two parts: *module path* and *local module name*.
These parts directly correspond to where `.piqi` files are located in the
directory hierarchy.

For example, module named (or referred as)

    foo/bar

would usually by defined in file

    foo/bar.piqi

As you can see, the *local module name* is derived from the file name by
stripping the `.piqi` extension.

A module can explicitly specify its name. For example:

    .module foo/bar

However, explicitly defined module names are rarely used in practice. Most of
time, they will be automatically derived from the location of the `.piqi` file.

Piqi module names can be formally described as follows:

    <module-name> ::= (<path> '/')? <local-module-name>

    <local-module-name> ::= ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']*

    <path> ::=   <path-element>
               | <path> '/' <path-element>

    <path-element ::= ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '.']+

    NOTE: a <local-module-name> can not contain both `-` and `_` characters.

When Piqi looks for included or imported `.piqi` module by its module name, it
tris one or more directories until it finds the matching file. The lookup order
is defined as follows:

- directory from which the referring module was loaded
- search paths explicitly provided by using the `-I ..` command-line parameter,
  which can be specified more than once
- current working directory (`.`)
- search paths listed in the `PIQI_PATH` environment variable, with several
  paths separated by `:`.

For instance, if module `a` is imported from module `b`, Piqi will first look in
the directory from which module `b` was loaded, then in paths specified using
`-I`, then in `.`, and finally, in paths from `$PIQI_PATH`.

When locating an imported or included module named `<path>/<local-module-name>`
in one of the search paths, Piqi looks for the module's file in the following
order:

1.  `<search-path>/<path>/<local-module-name>.piqi`
2.  `<search-path>/<path>/<local-module-name>.proto.piqi`
3.  `<search-path>/<path>/<local-module-name>.piqi`

    with `-` replaced with `_` in <local-module-name>

4.  `<search-path>/<path>/<local-module-name>.proto.piqi`

    with `-` replaced with `_` in <local-module-name>

5.  same as 1 but `_` replaced with `-` in <path>
6.  same as 2 but `_` replaced with `-` in <path>
7.  same as 3 but `_` replaced with `-` in <path>
8.  same as 4 but `_` replaced with `-` in <path>

Rules 1--4 mean that Piqi allows interchangeable use of `_` and `-` in local
module names. The recommended style is to use `_` in `.piqi` file names and `-`
in imported and included module names. See also the [Style
guidelines](#styleguidelines) section below.

Rules 5--6 account for additional module path normalization, during which path
is changed to include only `-` characters. This may be useful, for example, for
using module names as a part of URL strings.


### Module imports

*Imports* provides a way to use types defined in other Piqi modules in local
type definitions.

In order to use other module's types, the module must be first "imported" by
using the `import` directive.

Import directive defines the following properties:

-   module name

    Name of the imported module.

-   import name (optional)

    This is a local shorthand for referring types defined in the imported module
    as `<import-name>/<type-name>`.

    When not specified, it is implicitly defined as the *local module name*
    (i.e. the last path segment) of the imported module.

**Examples:**

    .import [
        .module example.com/foo
    ]

    % overriding implicitly derived import name "foo" with "bar"
    .import [
        .module example.com/foo
        .name bar
    ]


### Module includes

*Include* mechanism provide a way to reuse type definitions, imports, extensions
and all other top-level entries from another module as if they where defined
locally.

A module can include several other modules to combine their contents together.

The `include` directive is used to specify module inclusion. It has only one
property which the name of the included module.

**Example:**

    .include [
        .module example.com/foo
    ]


### Extension modules

Extension module is a Piqi module that has a second extension in its file name.
For example, "m.ocaml.piqi" is an extension module for a regular Piqi module
"m.piqi".

All operations applicable to regular Piqi modules are also supported for
extension modules. The difference is that extension modules can be included
automatically in the modules which they extend.

For instance, `piqic-ocaml` and `piqic-erlang` Piqi compilers try to
automatically include `<m>.ocaml.piqi` and `<m>.erlang.piqi` respectively for
every loaded module `<m>.piqi`.

Extension modules are useful for working with third-party Piqi or Protocol
Buffers definitions which, for example, may not define necessary OCaml- or
Erlang-specific properties in the first place.

By using this mechanism, it is possible to take any set of Piqi modules and
write custom extensions for them without modifying the original files. After
that, extensions can be loaded automatically for all recursively included and
imported Piqi modules.


Primitive types
---------------

`bool`
:   Boolean type.

`int`
:   `int` type represents signed integers. Supported range for this type is
    implementation-specific (i.e. depends on a certain Piqi mapping) but
    normally it should be capable for holding at least 32-bit signed integers.

    The maximum supported range for `int` type is defined as `(min(signed 64-bit
    integer), max(unsigned 64-bit integer))`.

    In addition to `int`, there are some other variations of integer types
    supported by default. They are defined as *aliases* of `int` type in Piqi
    self-definition:
    [piqi.piqi](http://github.com/alavrik/piqi/blob/master/piqi/piqi.piqi). Each
    Piqi mapping should provide support for these types.

    Below is the full list of Piqi integer types. Their names reflect some
    properties associated with their binary encoding and language mappings. See
    for example, [Piqi--Protocol Buffers mapping](/doc/protobuf/).

    -   `int`

    -   `uint`

    -   `int32`

    -   `uint32`

    -   `int64`

    -   `uint64`

    -   `int32-fixed`

    -   `uint32-fixed`

    -   `int64-fixed`

    -   `uint64-fixed`

    If unsure which integer type to use, it is recommended to use `int`.

    `uint` can be a little bit more efficient compared to `int` when serialized
    to binary encoding.

    `int32`, `uint32`, `int64`, `uint64` are similar to `int` and `uint` but
    they guarantee the integer ranges associated with these types (32-bit and
    64-bit).

`float` and `float64`
:   IEEE 754 double-precision floating point.

`float32`
:   IEEE 754 single-precision floating point.

`string`
:   Unicode string.

`binary`
:   Byte array (sequence of bytes).


Special built-in types
----------------------

There are two special built-in types:

-   `piqi-any`

     This type represents dynamically typed values in one of the supported
     portable data format (Protobuf, JSON, XML) or completely untyped JSON and
     XML.

     At low level, `piqi-any` maps to the `any` record defined in the Piqi
     [self-specification](/self-spec/).

     You can find some Piq examples [here](/examples/#piqi-any_piq).

-   `piqi`

     Values of this type are embedded Piqi modules. This type can not be used as
     a valid *type name* inside type definitions (at least yet). However, it is
     recognized by Piqi [command-line tools](/doc/tools/) and used in Piq
     format.


User-defined types
------------------

A user-defined defines a new type name that must be unique within the module's
local namespace and must not override names of the Piqi built-in primitive types
(e.g. `int`, `float`, `string`).

Type name must be a valid `identifier`.


### Record

Record is a composite data type that contains zero or more *fields* or *flags*.

Fields can have the following properties:

-   `name` (optional)

    Field name is represented as a valid Piqi `identifier`.

    Field name can be omitted. In such case it will be implicitly derived from
    the field's type name.

    Field names must be unique across all fields for a given record.

-   `type` (optional)

    Field type name refer to one of the following:

    -   Built-in type. E.g. `int`, `string`, `bool`, etc.

    -   Another type defined within the current module or included module.

    -   Type imported from another module. In this case *type name* will have
        the following format: `<import-name>/<type-name>`.

    Field that doesn't specify *type* is called *flag*. Since instance of such
    field doesn't have associated value, its presence in the record is
    meaningful by itself. Therefore the name -- *flag*.

    Flags must be defined with `optional` field *mode*.

-   `mode` (optional)

    Field mode can be specified as one of `required`, `optional` or `repeated`.
    As follows from their names:

    -   `required` means that exactly one field instance must be present in the
        record. This is the default.

    -   `optional` field *may* be present or missing in the record.

    -   `repeated` means that *zero or more* instances of the field may be
        present in the record.

-   `default` (optional)

    For `optional` fields it is possible to specify *default value* that can be
    used as a default field's value when the field is missing in the record
    representation. The actual use of default value depends on a particular Piqi
    mapping.

    Default values are represented as Piq values of the field's type, e.g.
    integer literals for `int` type.

-   `code` (optional, used with Protocol Buffer binary format)

    Usually small integer \> 1 uniquely identifying a field within a record. It
    must either defined for all fields or for none of the fields. When not
    defined, Piqi implicitly generates codes by enumerating the fields in the
    order they are defined in the record. See [Piqi--Protocol Buffers
    mapping](/doc/protobuf/) for details.

-   `protobuf-packed` (optional, used with Protocol Buffer binary format)

    Repeated fields of primitive numeric types (integers, floats, booleans and
    enums) can be represented in so called "packed" format. This format was
    introduced in Protocol Buffers 2.3.0 and provides a more compact encoding
    for repeated primitive types by basically omitting field tags for all fields
    but the first one and stacking fields values one after another. This is
    possible because primitive numeric types are self-delimited.

    In order to use such "packed" representation for a repeated field, specify
    `.protobuf-packed` as a field property.

-   `json-name` (optional, used with JSON format)

    Field name used when data is encoded in JSON. When not specified explicitly,
    it is derived from Piqi identifiers by replacing `-` characters with `_`.

-   `deprecated` (optional, experimental)

    Use this flag to mark fields that are no longer used or supported. Usually,
    such *deprecated* fields are kept around in the spec because of various
    considerations related to backward compatibility.

**Examples:**

    % this is a record definition
    .record [
        .name r     % record name

        % required integer field
        .field [
            .name i
            .type int

            % NOTE: fields are "required" by default; one can specify it explicitly:
            %.required
        ]

        % optional string field
        .field [
            .name s
            .type string
            .optional
        ]

        % optional binary field with default value (NOTE: default values may only be
        % specified for optional fields)
        .field [
            .name b
            .type binary
            .optional
            .default "abc \xff\x00"
        ]

        % repeated floating point field
        .field [
            .name f
            .type float
            .repeated
        ]

        % this is a flag, its presence in the record is meaningful by itself, it
        % doesn't carry any additional value
        .field [
            .name flag

            % NOTE: flags must be defined as .optional; obviously, .default value
            % doesn't make any sense for flags and thus not allowed
            .optional
        ]

        % optional "self"
        .field [
            .name self
            .type r     % here referencing the record we're defining now
            .optional
        ]

        % another optional filed which references type defined below
        .field [
            % NOTE: if field name and type name are the same, field name may
            % be omitted
            .type v
            .optional
        ]

        % required field of type "t" imported from module "mod"
        .field [
            .type mod/t
        ]

        % repeated integer field represented using "packed" format in binary
        % encoding
        .field [
            .name p
            .type int
            .repeated
            .protobuf-packed
        ]
    ]


### Variant

The *Variant* type, also known as [tagged
union](http://en.wikipedia.org/wiki/Tagged_union), specifies a set of *options*.
Only one *option* instance can form a variant value at a time.

For example, a well-known *enum* type is a simple example of variant type.

Options define *name* and *type name* properties in the same manner as fields
for the *record type*. The same rules and considerations apply for *option name*
and *option type name* as for *field name* and *field type name* (see above).

**Examples:**

    % definition of a variant
    .variant [
        .name v

        .option [
            .name i
            .type int
        ]

        .option [
            % NOTE: if option name and option type are the same, field name may
            % be omitted
            .type r
        ]

        .option [
            .type e
        ]

        % options may not be associated with any types, such options are similar to
        % those used in enums
        .option [
            .name flag
        ]

        % those used in enums
        .option [
            .name l
            .type v-list    % see below
        ]
    ]


### Enum

Enum is a degenerated case of the variant type. Enum defines options similarly
to variant, but enum options don't have types and can't hold values.

**Examples:**

    .enum [
        .name e
        .option [ .name a ]
        .option [ .name b ]
        .option [ .name c ]
    ]

    .enum [
        .name months
        .option [ .name jan ]
        .option [ .name feb ]
        .option [ .name mar ] % ...
    ]


### List

*List type* represents a list of elements where all elements have the same type.

**Examples:**

    % list of v
    .list [
        % NOTE: "-list" suffix is conventional and not strictly required
        .name v-list
        .type v
    ]

    % list of built-in type
    .list [
        .name int-list
        .type int
    ]

    .list [
        .name int-list-list
        .type int-list
    ]


### Alias

*Alias* defines an alias for some other user-defined, built-in or imported type.

**Examples:**

    % an alias
    .alias [
        .name a
        .type v
    ]

    % just to give an idea of how it can be used
    .alias [
        .name uuid
        .type binary
    ]

    .alias [
        .name epoch-seconds
        .type uint64
    ]

In Piqi, aliases are also used to assign static properties for types. For
instance, all Piqi integer types other than `int` itself are defined as aliases
of the built-in `int` type. For example, this is the definition of `int64`:

    .alias [
        .name int64
        .type.int
        .protobuf-type "sint64"            % correspondent Protocol Buffers type
        .protobuf-wire-type.zigzag-varint  % type of binary (wire) encoding
    ]

At the moment, there aren't many properties implemented by Piqi, but the concept
itself is very powerful. For example, this is how custom formatting functions
could be defined using aliases:

    .alias [
        .name epoch-seconds
        .type uint64

        .format.date-time
    ]

    .alias [
        .name uuid
        .type binary

        .format.uuid
    ]

    .alias [
        .name sha1sum
        .type binary

        .format.sha1
    ]


Extensions
----------

The extensions mechanism allows to add more components and properties to Piqi
entries.

Extensions can be applied to user-defined types (including records, variants,
enums, lists and aliases), fields, options, functions, function parameters and
imports.

Each extension can have the following properties:

-   extension target

    Extension target specifies which Piqi entry to extend. It is a combination
    of entry type followed by distinct entry name.

    This is a full list of supported extension targets:

    -   `.typedef <type name>`

    -   `.field <record name>.<field-name>`

    -   `.option <variant or enum name>.<option-name>`

    -   `.import <import name>`

    -   `.function <function name>`

    Targets can be specified more than once. In such case, several target
    entries will be extended using the same extension entries.

    Target must refer to a locally defined Piqi entry or entries included from
    other modules via `include`.

    Extensions of imported definitions are not supported.

-   actual extension entry

    Extension entry specifies an object that will be added to the *extended*
    entry as if it was defined there natively.

    For example, a *field* definition would be a typical extension entry for
    *record* type. Similarly, *option* entries would be typically used for
    extending *enum* or *variant* type.

For example, we can add a field to a previously defined record:

    .record [
        .name r
        .field [
            .name i
            .type int
        ]
    ]

    .extend [
        .typedef r
        .with.field [
            .name s
            .type string
        ]
    ]

Or we could use extension to add an enum clause:

    .enum [
        .name e
        .option [ a ]
    ]

    .extend [
        .typedef e
        .with.option [ .name b ]
    ]

Extending fields, options or function parameters requires a slightly different
target specification:

    .extend [
        .field r.i
        .with.erlang-name "erlang_i"
    ]

    .extend [
        .option e.b
        .with.ocaml-name "ocaml_b"
    ]

In the same manner we can add arbitrary properties to variants, lists, aliases,
functions and imports.

There is a good example that demonstrates the power of Piqi extensions. The Piqi
implementation uses this mechanism to extend its own specification with extra
features. For example, the following specification extends Piqi to support
Protocol Buffers properties:

[piqi.protobuf.piqi](/self-spec/#piqi_protobuf_piqi)

Similarly, support for OCaml-specific Piqi properties is provided by these two
specifications:

[piqi.ocaml.piqi](https://github.com/alavrik/piqi-ocaml/blob/master/piqic-ocaml/piqi.ocaml.piqi),
[piqi.piqi-ocaml.piqi](https://github.com/alavrik/piqi-ocaml/blob/master/piqic-ocaml/piqi.piqi-ocaml.piqi)


Functions
---------

The `function` directive provides a way to define abstract functions. Functions
were originally introduced for [Piqi-RPC](/doc/piqi-rpc/) that relies on
high-level function definitions.

Each defined function has a name and 3 types of parameters: *input*, *output*
and *error*, all of which are optional.

The *error* parameter is a special type of output parameter. It is assumed that
when the function is called, only one of *output* or *error* parameters will be
returned.

It is possible for a function to not have any input or output parameters at all.
Such function represents a named synchronous call where the call is meaningful
by itself and no parameters are passed in any direction.

Input and output function parameters correspond to an arbitrary (primitive or
composite) named Piqi data types. That is, Piqi function takes data structure as
input parameter, and returns a data structure as output parameter.

Examples:

    % function with no input and output parameters
    .function [
        .name foo
    ]

    % function with input, output and error parameters
    .function [
        .name foo

        .input int
        .output some-user-defined-type-name
        .error string
    ]

For extra convenience, function may define an input, output or error types
inline without having to define them separately:

    % function with a record input parameter and primitive output and error
    .function [
        .name bar

        .input [
            .field [
                .type int
                .optional
            ]
        ]
        .output int
        .error float
    ]

    % function with a record input that has a default value
    .function [
        .name baz

        .input [
            .field.record [
                .type int
                .optional
                .default 10
            ]
        ]
    ]

    % function with a variant input parameter
    .function [
        .name v

        .output.variant [
            .option [
                .name i
                .type int
            ]
            .option [
                .name f
                .type float
            ]
        ]
    ]

    % function with an enum error parameter
    .function [
        .name e

        .error.enum [
            .option [ .name a ]
            .option [ .name b ]
        ]
    ]

For each defined *input*, *output* or *error* parameter, a correspondent Piqi
alias or a composite type gets implicitly defined. Records, variants, lists and
enums are generated for inline parameter definitions, aliases are generated for
all other types that are referred by name.

In case of *input* parameters, the name of the alias or the user-defined type
becomes `<function-name>-input`. Similarly, names for *output* and *error*
parameters become `<function-name>-output` and `<function-name>-error`
respectively.


Piqi-light syntax
-----------------

Piqi-light syntax is an experimental lightweight EBNF-like read-only notation
for Piqi type definitions. It provides a compact way of displaying type
definitions while omitting all non-significant properties that may be present in
the original Piqi specification.

The original Piqi syntax is based on Piq that is optimized for editing
convenience and extensibility. But the same properties that make Piqi/Piq such a
great format for editing and representing all the features, also make it
substantially verbose and uniform. Both verbosity and uniformity make it harder
to consume Piqi for informational purposes. Piqi takes a lot of display space
and doesn't provide more prominent syntax for important properties such as field
names and types which, in the absence of concrete syntax, get the same visual
treatment as other less important language properties.

On the other hand, in practice, type definitions are rarely modified once
initially written. Therefore it is feasible to have a type definition syntax
that is optimized for reading.

These considerations lead to the idea that, maybe, it is practical to have two
highly expressive syntaxes: one being optimized for reading, and another one --
for writing and extensibility.

After having both syntaxes implemented, benefits of such division are becoming
more obvious. It would be extremely hard to provide an efficient unified
language solution for both of these use-cases, especially considering how
different the current Piqi and Piqi-light notations are.

There is one very important feature that Piqi-light is missing at the moment. It
is hand-written comments from the original Piqi specification. Unfortunately, it
will remain this way until Piqi obtains a uniform method of writing
documentation sections that can be reliably represented and passed to
Piqi-light.

`.piqi` files can be printed in Piqi-light syntax using `piqi light`
[command](/doc/tools/#piqilight) (for the lack of a better name).

For examples of Piqi-light syntax visit [Examples](/examples/) and
[Self-definition](/self-spec/) pages. All `.piqi` files there have a tab
where they are displayed in Piqi-light syntax.


Style guidelines
----------------

### Type, field and option names

Although Piqi doesn't enforce certain naming style, it is recommended to use
lowercase identifiers instead of "CamelCase"-style identifiers.

This way they are more readable and will retain readability while being combined
with some future Piqi features.

The guiding principle for this rule is that high-level Piqi type definitions
should resemble grammar rules.

Piqi pretty-printer from [Piqi tools](/doc/tools/) can be used to convert
"CamelCase" identifiers to "camel-case" (`piqi pp --normalize <.piqi> file`
command).


### Names for `list` type

It is recommended to name `list` types by appending `-list` suffix to the
original type name.

For example, if we wanted to define a list of type `t`, we would name the type
`t-list`.

Using `*-list` names for non-list types should be avoided.

One of possible Piqi future features can rely on that: piqi would recognize
`*-list` type names as list types automatically removing the need for defining
`list` types manually.


### Naming of `.piqi` files

for naming `.piqi` files, it is better use lowercase and `_` as word separator.

As with identifiers, the choice of this convention is determined by the fact
that is is more universal and popular among various programming languages (and
URLs!) than "MixedCase" naming schemes.


### A note about directory paths

It is typical for `.piqi` modules to be located in nested directory hierarchies.
When this is the case, directory paths become parts of module names. For
example, if a Piqi module `bar.piqi` is defined inside directory `foo`, other
modules may refer to it as `foo/bar`.

Piqi doesn't impose any restriction on how directories should be named. However,
future normalization schemes will likely automatically turn directory names to
lowercase with `-` being a word separators.

Considering future normalization, it is recommenced to name directories using
lowercase characters with `-` character as a word separator. This way directory
name normalization won't be needed.


### Code formatting

Since Piqi is based on Piq syntax, general [Piq](/doc/piq/#styleguidelines)
formatting rules apply.


Miscellaneous Design Notes
--------------------------

-   For the sake of better readability and portability Piqi doesn't support
    nested definitions.

-   As you may noticed, Piqi type definitions are strictly monomorphic. As with
    nested definitions, this design decision was driven by portability and
    simplicity of the Piqi language and its potential mappings. There are many
    programming languages that don't support parametric polymorphism. On the
    other hand, the world of practical protocols and portable data is
    surprisingly monomorphic (with sequences being the only exception). This, in
    turn, means that the absence of polymorphism at the data definition level is
    unlikely to ever become a serious practical limitation.

