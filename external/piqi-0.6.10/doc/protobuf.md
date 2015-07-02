This document describes compatibility and mappings between Piqi and Google
Protocol Buffers.

Piqi is specially designed to be largely compatible with Google Protocol
Buffers:

-   Piqi modules and types (defined in `.piqi` files) can be converted to Google
    Protocol Buffers type specifications (`.proto` files) and vice versa using
    `piqi to-proto` and `piqi of-proto` commands.

-   Piqi uses the same binary encoding as the one used by Protocol Buffers.

*NOTE: information below applies to the latest version of Piqi and Protocol
Buffers \>= 2.3.0*

Comparison between Piqi and Google Protocol Buffers
---------------------------------------------------

### Data serialization

Unlike Protocol Buffers, where only messages (i.e. records) can be serialized,
Piqi support serializing values of any user-defined or primitive type.

Top-level primitive values, such as integers, strings, booleans are serialized
as if they were wrapped in a record containing the only element of the
correspondent type. For natively supported languages, Piqi provides such
wrapping implicitly. But when top-level primitive Piqi definitions are mapped to
Protobuf definitions, explicit Protobuf `message` definitions get generated for
the wrappers.

### User-defined types

In addition to `message` and `enum` types supported by Protobuf, Piqi has
`variant` (also known as [tagged
unions](http://en.wikipedia.org/wiki/Tagged_union)), `list` and `alias` types.

#### Nested type definitions

Protocol Buffers allow to define types inside the scope of another definition.
For example, enum or message `bar` can be defined inside message `foo`.

Piqi doesn't support nested definitions. Each definition must be defined
separately. This rule enforces flat scope for type names within a module, which
makes Piqi definitions more explicit and allows easier mapping to programming
languages that don't support nested type definitions.

Proto to Piqi converter (`piqi of-proto` command) converts nested Proto
definitions to top-level Piqi definitions by prefixing their names with the
parent's name.

For example, if a Protobuf type `bar` is defined inside message `foo`, the name
of the correspondent Piqi type will become `foo-bar` and the definition will be
moved to the top-level.

If there are several nesting levels, the same top-level name construction
("un-nesting") rule is applied recursively.

#### Structured default values for optional fields

Protocol Buffers support defaults only for primitive types. In Piqi, it is
possible to have a default value for an optional field of any type including
`record`, `variant` and `list`.

#### Optional field and enum constant codes (numbers).

Piqi does support numeric codes for `record` fields and `enums` constants but,
unlike in Protocol Buffers, they are optional.

If no codes are specified they will be assigned automatically for each
field/enum constant by enumerating them in the order in which they are defined
starting from 1.

Note that when the codes are assigned automatically, there is no guarantee of
portability between binary representations when fields/constants are reordered
in Piqi definitions. But it is still convenient not to specify them explicitly
in some cases, for example, during prototyping stage.

#### Optional names and types for fields and options

*TODO*

#### Custom type mappings

For natively supported languages Piqi provides a way for a user to map any
user-defined type to an arbitrary language type by providing conversion
functions.

Protobuf Buffers do not provide such ability.

*TODO: example*

### Primitive types

Primitive types supported by Piqi are similar to Protobuf primitive types, but
they have different names. Piqi names are meant to be more conventional and
intuitive.

The table below represents correspondence between Piqi and Protocol Buffers
primitive types.

  Piqi type(s)     Protobuf type
  ---------------- ---------------
  bool             bool
  string           string
  binary           bytes
  int, int32       sint32
  uint, uint32     uint32
  int64            sint64
  uint64           uint64
  int32-fixed      sfixed32
  uint32-fixed     fixed32
  int64-fixed      sfixed64
  uint64-fixed     fixed64
  protobuf-int32   int32
  protobuf-int64   int64
  float, float64   double
  float32          float

Note that mapping of Piqi primitive types to Protobuf types is not hard-coded.
It it specified in the Piqi self-specification file:
[piqi.protobuf.piqi](http://github.com/alavrik/piqi/blob/master/piqi/piqi.protobuf.piqi).

### Extensions

Protocol Buffers approach to extensions works well for object-oriented languages
where fields and their access logic are hidden behind access functions (i.e.
"setters", "getters", etc).

However, for non-OO languages, it is much harder to support extensions in the
same way they work in Protocol Buffers in an elegant manner. Especially it is
true for extensions of types defined in a different module (i.e. extensions of
imported types).

There are also some problems with the way how extensions are implemented in
Protocol Buffers (at least for C++ and Python). For example, all extensions
defined in a single Protobuf `.proto` module share a single namespace even if
they extend different types. As a workaround, it is possible to put extensions
inside some message definition. But doing it just for the sake of avoiding name
conflicts is ugly.

Another problem with Protobuf extensions is that access interface for extension
fields is different from regular fields. Such property is dictated by the way
how Protobuf extensions are implemented and used, but it doesn't look very
elegant in general.

Considering these problems and implementation difficulties for non-OO languages,
Piqi takes a different approach to extensions. It is compatible with Protocol
Buffers way of representing extension fields in binary format and extensions
definitions can be converted between Piqi and Protocol Buffer formats gaining
equivalent types at run-time.

Basically, Piqi extensions are applied and resolved at compile time rather than
at run-time as in Protobuf. For example, if a Piqi extension defines an extra
field `f` for a record `r`, at the time when this specification is mapped to a
target programming language, record `r` will contain a definition of field `f`
as if was defined in `r` natively. In other words, in Piqi, there is no
difference between natively defined fields and extended fields.

`piqi expand` command can transform a given `.piqi` module by applying all
extensions to their correspondent definitions.

There are several other key differences:

Piqi extensions can be applied to any type definitions, not only to records.
:   Unlike in Protocol buffers, Piqi extensions can be applied to every type
    definition such as record, variant, enum, alias and even lists.

    For `records`, extensions specify extra `fields`, while for `variants` and
    `enums`, they specify additional `options`.

    For `aliases`, extensions specify additional properties.

One Piqi extension can be applied to several data definitions at a time.
:   In Piqi, it is possible to apply one extension to several data definitions
    at a time by listing names of all definitions that need to be extended.

    This feature turned out to be extremely useful. For instance, Piqi
    implementation relies heavily on it.

    This mechanism can be used for defining common properties of various data
    types. In certain way, Piqi extensions is the inversion of structured
    inheritance: instead of having common properties defined in a parent record,
    which is then "inherited" from other records, Piqi extensions explicitly
    specify which properties are shared among several records.

*That said, we recognize that Protobuf-style dynamic record extensions can still
be useful for certain applications and Piqi may support them eventually.*


#### Extensions of imported definitions

At the moment, it is not clear whether this feature will be supported by Piqi
directly.

There is, however, a manual way to achieve the desired effect of extending
imported definitions using Piqi extensions. It works as follows.

1.  For the module `m` which definitions are extended in other modules, create
    an "extension" module `m-extended` that includes module `m` using `include`
    directive.

2.  Move all extensions that extend types defined in module `m` from their
    modules into module `m-extended`.

3.  In those modules that contained extensions of types defined in module `m`,
    import module `m-extended` instead of `m`.

*TODO: example*


#### Nested extensions

Nested Protocol Buffers extensions are "un-nested", i.e. converted to Piqi
top-level extensions in the same manner as messages and enums (see above).

### Protocol Buffers packages and Piqi modules

Piqi has its own naming scheme for modules which is not directly compatible with
Protobuf packages. However it is still possible to convert `.piqi` modules to
and from `.proto` modules in a consistent manner.

Each Piqi module has a name which corresponds to its location. Location can be
global, associated with a domain name, or local, resolved within a local
filesystem hierarchy.

Piqi module defines a unique namespace which is flat inside the module -- nested
definitions are not allowed.

Definitions from other modules can be imported via import mechanism, and each
imported module is assigned a name inside the module where it is imported to.

In addition to imports, Piqi support another mechanism for reusing type
definition from external Piqi modules -- includes. `include` directive tells
Piqi to reuse all piqi definitions, imports and extensions from an external
module as if they were defined locally. In certain way it is similar to C
preprocessor "\#include" directive, but unlike it, Piqi "include" automatically
handles duplicate includes, drops properties that shouldn't be included (like
module names) and performs some other operations and checks.

### Service definitions

There's no direct support for service definitions in Piqi, but it is possible to
define functions. The way functions are defined in Piqi makes them incompatible
with Protobuf service definitions:

-   Unlike Protocol Buffers functions, Piqi functions are not grouped in
    services. This way functions defined in a Piqi module share common
    namespace.

-   Piqi function parameters can be of any primitive or user-defined type,
    whereas only `messages` (i.e. records) are allowed as parameters in Protobuf
    functions.

-   In addition to Input and Output parameters supported in Protocol Buffers,
    Piqi functions can specify an additional Error parameter for returning
    errors.

Piqi to Proto mapping
---------------------

The following mapping rules are applied during conversion of Piqi type
specification modules (`.piqi`) to Protocol Buffers specifications (`.proto`)
using `piqi to-proto` utility.

### Modules

During conversion each `<path>/<x>.piqi` file is converted to
`<path>/<x>.piqi.proto` file.

Names of all type definitions are resolved to fully-qualified Protocol Buffers
names respecting Protocol Buffers package which is defined in
'.protobuf-package' top-level field.

Optional `.protobuf-package <package-name>` top-level string field will be
converted to Protobuf package name.

Repeated `.protobuf-custom <package-name>` top-level `text` field will be copied
to the output `.proto` file without modification. This field can be used to
specify Protobuf-specific options such as "java\_package" or "optimize\_for".

Examples:

    .protobuf-custom # option java_package = "com.example.foo";

    % the same definition that uses string instead of verbatim text syntax:
    .protobuf-custom "option java_package = \"com.example.foo\";"

    .protobuf-custom # option optimize_for = SPEED;

Several `.protobuf-custom` fields can be defined in one Piqi module.

#### Includes

If a Piqi module includes other Piqi modules using `include` directive, all
their definitions and imports will be included in the resulting Protobuf
specification.

All extensions from included modules will be applied before generating the
Protobuf specification.

#### Imports

Piqi `imports` are converted to Protobuf `imports` directly.

### Primitive types

See the type mapping table above.

### User-defined types

-   Type names

    Sometimes it is necessary to override Piqi names and specify a custom Proto
    name for a type. For example, Piqi type name can conflict with one of Proto
    keywords. In such case, a custom Proto name can be specified using
    `.protobuf-name "< name>"` field next to the original `.name <name>`
    entry. (This feature also works for field names and option names.)

    For those Piqi fields or options which do not specify names, Proto name is
    derived from Piqi type name for that field.

-   Records are mapped to Protobuf messages

    Optional `.protobuf-packed` property is used to specify that *packed*
    encoding should be used for fields with primitive types.

    There are several nuances.

    Piqi `flags` (i.e. `options` or `fields` without types) are mapped to
    `optional bool` Proto fields.

    Since Protobuf doesn't structured default values for optional fields, such
    default values will be dropped during conversion and a warning message will
    be printed.

-   Enums are mapped to Protobuf enum definitions.

-   Variants mapped to Protobuf messages. Each variant `option` is mapped to the
    correspondent `optional field`.

-   List type is mapped to Protobuf message containing a single repeated `elem`
    field: `repeated elem = 1`.

    Optional `.protobuf-packed` property is used to specify that *packed*
    encoding should be used for list elements of primitive types.

-   Since there are no type aliases in Protocol Buffers, Piqi aliases are
    unwound to their original types which then mapped to related Protobuf types.

### Extensions

Piqi extensions are not converted to Protobuf extensions since Piqi data
structures are extended before conversion.

Although it is possible to map Piqi extensions for records and variants to
message extensions in Protocol Buffers, such mapping hasn't been implemented
yet.

Proto to Piqi mapping
---------------------

The following mapping rules are applied during conversion of Protocol Buffers
specifications (`.piqi`) to Piqi type modules (`.piqi`) using `piqi of-proto`
utility.

### Modules

During conversion each `<x>.proto` file is converted to `<x>.proto.piqi` file,
where is `<x>` is a path name.

Protobuf imports are converted to Piqi imports and each import is given a name
which is derived from imported file name.

Protobuf package name is converted to `.protobuf-package` top-level field.

Names of all Proto type definitions are converted to valid Piqi names:
underscores are converted to `-` and each external definition's name is
prepended with import name. Optionally it is possible to convert "CamelCase"
identifiers to "camel-case" by specifying `--normalize` command-line flag.

Nested definitions are converted to top-level definitions. While conversion,
each nested definition name is prefixed with container's name followed by `'-'`
character.

### Primitive types

See the type mapping table above.

### User-defined types

-   Protocol Buffers `messages` are mapped to Piqi `records` directly.

    While converting fields, all field options are dropped since Piqi doesn't
    support them.

    (Field options include, for example, `deprecated`, uninterpreted options,
    min/max code constraints for extensions, etc.)

-   Enums are mapped to Piqi `enum` definitions directly.

    For extra convenience, Piqi enum definitions support an optional
    `.protobuf-prefix <prefix>`. This property defines a prefix that is
    automatically added to each enum option's name in `.proto` modules converted
    from `.piqi` using `piqi to-proto` command. This mechanism helps to deal
    with the fact that Protobuf-generated enum definitions do not form a C++
    namespace meaning that enum constants are defined directly in the outer
    namespace (it was announced that this problem will be fixed in
    protobuf-2.5).

-   Groups

    Conversion will fail with error if a `.proto` specification contains groups.
    However groups can be converted to messages by specifying `--convert-groups`
    flag for `piqi of-proto` command.

### Extensions

Protobuf extensions are converted to Piqi extensions directly.

Examples
--------

-   This example is based on "addressbook.proto" from Protocol Buffers source
    distribution. It was converted to Piqi ("addressbook.proto.piqi") using
    `piqi of-proto` command.

    [addressbook.proto](/examples/#addressbook_proto)

-   This example shows how Piqi type definitions are mapped to Protocol Buffers
    type definitions.

    [def.piqi](/examples/#def_piqi)

-   Advanced example demonstrating Piqi--Protobuf--C++ mapping.

    `piqi to-proto` command produces Protocol Buffers specification (`.proto`)
    from Piqi self-specification
    ([piqi.piqi](/self-spec/#piqi_piqi)). After that, a C++
    program reads Piqi self-specification represented as a binary object and
    prints it out in Protocol Buffers text format.

    [tests/cpp/](http://github.com/alavrik/piqi/tree/master/tests/cpp/).


