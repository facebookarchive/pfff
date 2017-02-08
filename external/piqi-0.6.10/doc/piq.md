Overview
--------

Piq is a language for representing structured data. It allows people to
conveniently read, write and edit structured data. Piq offers the following
features:

-   Minimalist and powerful syntax:

    -   no syntax noise compared to XML

    -   much lighter than even JSON especially for editing

    -   reasonable amount of parenthesis compared to S-expressions

    -   comments

-   Rich set of data literals including:

    -   Unicode strings, booleans, integer and floating point numbers (including
        "infinity" and "NaN")

    -   binary strings (byte arrays)

    -   verbatim text

    -   lists

Piq is statically typed and meant to be used together with the [Piqi data
definition language](/doc/piqi/) or Google Protocol Buffers. A lot of Piq
features heavily rely on knowing data types. For example, in certain cases, it
is possible to omit field names in record representation.

Primitive values such as integers, booleans and strings are first-class values
in Piq. Such treatment of data makes Piq closer to programming languages rather
than to XML or JSON that were designed for representing "documents" or
"structured objects".

Piq is a stream format: one Piq file (stream) can contain multiple typed values.

Although Piq is a text language, the actual *data* represented in Piq files can
be converted to and from [JSON, XML and Protocol Buffers](/doc/encodings/).

If you are not familiar with Piq and want to get basic feeling of what it looks
like you can click though some real [examples](/examples/) and/or skim through
the rest of the page.

After getting familiar with the basics, refer to [Piqi
documentation](/doc/piqi/) to get information about what data structures can be
represented in Piq.

At the moment, there is no comprehensive reference manual, so a lot of concepts
below will be introduced and explained by example.


Lexical conventions
-------------------

Piq is a text-based language. Piq is represented as valid Unicode text encoded
in UTF-8. Other Unicode encodings are not supported.


### Ignored whitespace

Whitespace is one or more of the following characters sequences:
`\x20` (space), `\t` (tab), `\n`, `\r\n`.

Whitespace serves as a token separator.


#### End of line ("EOL")

`\n` and `\r\n` are recognized as *end of line*.

`\r` without following `\n` is not allowed.


### Comments

Comment is any sequence of characters beginning with `%` and continuing till the
end of the line, excluding cases in which `%` character occurs in *string* or
*verbatim text* literals.

Currently, comments are not recognized as tokens and skipped during parsing
stage.


### Boolean literals

There are only two valid values of boolean literals (they are also the only
keywords in Piq):

    true

    false


### Number literals

#### Integer literals

**Examples:**

    % base 10 integers

    0 -1 100 1_000_000_000


    % conventional base 16 integers

    0xffff -0xffff_0000


    % base 2 integer literal

    0b1111_1111_1111_1111

Integer *literals* also represent valid floating point *values*.


#### Floating point literals

**Examples:**

    0.0 -10.0

    3.14159

    -2e15
    5.6e-10


    % not a number, positive and negative infinity

    0.nan 0.inf -0.inf


### String literals

**Examples:**

    "this is a string\n"

    % "hi" in Russian (utf-8 encoded Cyrillic characters)
    :string "привет"

    % these are supported string escape sequences
    "\" \t\n\r  \x20 \u0020 \U00000020"

    "\x74\x79\x70\x65"

    % NOTE: octal escape codes are unsupported (e.g. "\000\123")

    % binary literal must not contain Unicode characters with codes > 127, any string
    % literal containing ASCII-characters represents a valid binary value

    "this is a binary literal"

    "binary literals may contain bytes encoded like this: \xfe"

    "non-unicode escapes are also allowed in binary literals: \" \t\r\n"

String *literals* are used for representing both Unicode `string` *values* and
`binary` (i.e. byte array) *values*.

There are several simple rules that restrict usage of some string sequences
depending on the type of the value.

-   ASCII strings represent valid values of both `string` and `binary` types.

-   Unicode characters and escape sequences for unicode characters (those
    starting with `\U` or `\u`) are prohibited in binaries.

-   Hex escape sequences (those that start with `\x`) with codes greater than
    127 are prohibited in strings.

Note, that there is no character literal in Piq (and no character type in Piqi).


### Verbatim text literal

Verbatim text literal is represented by one line or several contiguous lines of
text that start with `#`, followed by a single space character, and continue
until the end of the line.

**Examples:**

    # single-line verbatim text

    # multi-line verbatim test
    # here's another line
    #
    # and another one
    #

    % leading whitespace is ignored:

            # one more line of text

        #
        # that's it

Verbatim text literals represent values of type `string`.


### Word literal

Word is a contiguous sequence of characters, delimited by whitespace or one of
these special characters: `( ) [ ] { } " % #`. Words can not contain
non-printable characters like characters from the lower ASCII range or
non-printable Unicode characters.

NOTE: `true` and `false` words are exceptions, because they are recognized as
boolean literals.

**Examples:**

    abc

    !!!!!

    +

    --

    *0-=+q`~@j\/&


Word literals represent values of type `string`.


### Name literal

Name is a *word* starting with `.` character. Remaining characters form an
`identifier` that is defined as follows.

    <identifier> ::= ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-']*

**NOTE:** use of underscore (`_`) characters in names is not allowed. Hyphens
(`-`) should be used instead.

Two or more consecutive `-` characters are now allowed. Also, identifiers can
not begin and end with `-`.

`true` and `false` are reserved for boolean literals -- they can not be used as
identifiers.

Piq *name* essentially represents the same concept as *atom* in various
programming languages and corresponds to *record flags* and *variant flags
(constants)* in the Piqi data model. Refer to [Piqi documentation](/doc/piqi/)
for more information.

**Examples:**

    .foo
    .bar

    .long-name


### Type name literal

Type name is a *word* starting with `:` character. Remaining characters form a
`typename`.

Type names correspond to valid Piqi types. Typically, they are used as a part of
*Typed values* (see below).

[Piqi documentation](/doc/piqi/) includes detailed information about type names.

**Examples:**

    % names of built-in types
    :int
    :float

    % here the module name is "piqi" and the typename is "record"
    :piqi/record

    % another typename
    :example.com/types/module-foo/type-bar

    % type name of a type defined in local module "foo"
    :foo/type-bar


### Named values

Named value starts with `name` followed by a "non-name" literal.

*("name" literal is one of the following: `name`, `typename`, `named` or
`typed`. This way, "non-name" literal is any Piq literal except for the above
four.)*

Named values are used for representing *record fields* and *variant options*.
Refer to Piqi documentation for more information.

**Examples:**

    .foo true

    .a 10

    .s "abc\n"


### Typed values

Typed value starts with `typename` followed by a "non-name" value.

**Examples:**

    :int 10

    :float 3.14159

    :piqi/record [
        .name foo
        .field [
            .name bar
            .type int
        ]
    ]


### List

**Examples:**

    []    % an empty list

    % list containing integer literals

    [ 1 2 3 4 5 ]

    % list containing lists of integers

    [ [1 2] [3 4 5] ]

    % list containing two named values

    [ .a 0 .b 1 ]

This list represents a real (record) data structure that is taken from [person
example](/examples/#person_piq).

    [
        .name "J. Random Hacker"
        .id 0

        .email "j.r.hacker@example.com"

        .phone [ .number "(111) 123 45 67" ] % phone is "home" by default

        .phone [
            .number "(222) 123 45 67"
            .mobile
        ]

        .phone [
            .number "(333) 123 45 67"
            .work
        ]
    ]

In Piq, list is a universal representation for a composite data type. Depending
on the actual Piqi data type Piq, list may represent a *record* or a *list*.

Such universal composite data representation has certain advantages originating
from the Piqi data model. One of the major advantages is that *record of fields*
can be also viewed as a *list of variant options*.

For example, considering the following Piqi type specification:

    .record [
        .name r
        .field [ .name a .type int ]
        .field [ .name b .type int ]
    ]

    .variant [
        .name v
        .option [ .name a .type int ]
        .option [ .name b .type int ]
    ]

    .list [ .name v-list .type v ]

the same list may be used for representing values of both `r` and `v-list`
types:

    [ .a 0 .b 1 ]


### Associativity control

Parenthesis can be used to override default associativity rules.

Since Piq is not a programming language (at least yet), their practical
application is limited to the cases when we want to define a *name literal* that
otherwise would be parsed as *named value* (the same applies to *type name
literal* and *typed value* as well).

For example, these expression are valid but parenthesis have no effect, because
they are applied to single values without any ambiguous context.

    (10)

    ("foo")

    ( [1 2 3] )

    (.foo)

    (.foo bar)

In these examples, parenthesis are necessary:

    % this is a list of two _name literals_ ".foo" and ".bar" followed by named
    % value ".fum 1"
    [ .foo .bar .fum 1 ]

    % this is a list of one _named value_ with name ".foo" and value ".bar"
    [ .foo (.bar) ]

    % this is a list of one _named value_ with name ".foo" and value
    % ".bar (.fum % 1)"
    [ .foo (.bar (.fum 1)) ]

    % this is a list of one _named value_ with name ".foo" and value ".bar (.fum)
    % followed by _integer literal_ "1".
    [ .foo (.bar (.fum)) 1 ]


### Built-in syntax abbreviation

#### Typed/Named value construction abbreviation

The named value from the previous example:

    [ .foo (.bar) ]

can also be written in a shorter form using built-in syntax abbreviation:

    [ .foo.bar ]        % NOTE: no spaces are allowed around '.'

Abbreviation can be applied several times. For example, this expression

    [ .foo (.bar (.fum 1)) ]

is equivalent to

    [ .foo.bar.fum 1 ]

This mechanism also works with *typed values* and the mix of *typed values*
and *named values*. For example:

    :foo.bar 1


#### Repeated named value construction abbreviation

Suppose we need to define several *list elements* all having the same name:

    [ .foo 1 .foo 2 .foo 3 ]

A built-in macro provides a shorthand for it:

    [ (.foo 1 2 3) ]

The same idea applies to *type names* and abbreviated cons-names described in
the previous section:

    [ (:int 1 2 3) ]

    [ (:type.name a b c) ]

will be expanded to

    [ :int 1 :int 2 :int 3 ]

    [ :type.name a :type.name b :type.name c ]

Parentheses are also reserved for future use as *forms* (i.e. macro and function
calls).


### `json` and `xml` built-in forms

`json` and `xml` built-in forms are used for representing untyped JSON and XML
data. Untyped JSON or XML data is a value of `piqi-any` type.

Both forms must have exactly one *text literal* argument.

In case of `xml` form, the argument must represent a well-formed XML element.

For `json` form, the argument can be an arbitrary JSON value (including values of
primitive types).


**Examples:**

    % untyped JSON (via built-in "json" form)

    (json
        # [{"i": 1}, "foo"]
    )

    (json
        # 1
    )

    (json
        # []
    )


    % untyped XML (via built-in "xml" form)

    (xml
        # <value>
        #   <i>1</i>
        #   <foo/>
        # </value>
    )


Syntax
------

### Record representation and parsing

Piqi record is represented as a *list* which elements correspond to record
fields.

The elements can be *named values* or values without a name. Field names, i.e.
*name* parts of *named values*, can be omitted for convenience.

Example:

```
% records are syntactically indistinguishable from lists...
[
    # values for the first 2 required fields with omitted field names
    10
    20

    .foo 10  # integer field "foo"
    .bar true  # boolean field "bar"

    .fum [  # field "fum" representing some nested composite value
        ...
    ]
]
```

The order in which fields are parsed is significant, because it directly affects
the way how missing fields are handled.

When the Piqi implementation parses a record, it attempts to read and recognize
the *required* fields in the order they are defined in the record specification.
After parsing all *required* fields, Piqi proceeds with parsing optional and
repeated fields.

If Piqi can't find required field by its name, it tries to parse the next
unrecognized record entry using the field's type specification. When parsing
succeeds, Piqi uses the result of parsing as the field's value. If parsing
fails, Piqi tries other entries until it finally succeeds to parse one of them.

Handling optional and repeated "unnamed" fields is similar to the above method,
but it introduces a fair level of uncertainty. For this reason, it is
recommended to use this feature mindfully. In most cases, it means explicitly
specifying names for optional and repeated fields.

Piq has a mechanism for precise control of whether certain fields can be
positional (i.e. "unnamed") or must come with names.

By default, fields of primitive types can be positional whereas fields of
composite types (i.e. records and lists) must be named.

The default behavior can be changed by defining the `.piq-positional` boolean
property. When set to `false`, the field will not be recognized unless it is
properly named. When `true`, even record and list fields will be allowed to omit
their names.

`.piq-positional true|false` can be defined at the field level or at the record
level. Field-level settings override record-level setting.

Examples:

```
.record [
    .name r

    % force all fields to have names unless overridden by individual field
    % settings

    .piq-positional true
    ...

    .field [
        .name f
        .type ...

        % allow to omit the field's name regardless of
        % the record-level setting and the type of the field

        .piq-positional true
    ]
]
```


### Field aliases

Record field has a canonical name defined by the `.name` property or derived
from the field's `.type` name.

Sometimes it is useful to define another, typically a shorter, name for a field.
During parsing, such alias can be used interchangeably with the primary name.

Field aliases are defined by specifying the `.piq-alias` field property.

For example:
```
.field [
    .name foo
    .piq-alias f
    ...
]

```


"Relaxed" parsing mode
----------------------

Piq parser supports so called "relaxed" mode. For instance, in Piqi
[command-line tools](/doc/tools/), it can be enabled by specifying
`--piq-relaxed-parsing true` option.

In this mode, single-word string literals don't have to be quoted.

For example, word literal `foo` will be treated the same way as string literal
`"foo"`.


Piq file format
---------------

Piq data values are stored in files with `.piq` extension. Piq file can contain
multiple values.

`.piq` files don't have specific headers or footers. This property gives the
ability to stream contents of `.piq` files, append `.piq` file to another one,
and so on. Because of this, contents of `.piq` files will be often regarded as
*Piq streams* in the current documentation.

Piq streams can contain four different kinds of entries.

-   Explicitly typed values represented as *typed values* as defined in the
    above sections. They start with `typename` followed by an untyped data
    value. For example,

        :int 1

-   Default type name for implicitly typed values.

    Piq streams allow to specify default name for those data values that don't
    explicitly mention their type.

    `(:<typename>)` is a special directive that sets the default type for
    subsequent implicitly typed values. It can be specified many times at
    different position of one Piq stream. Each subsequent directive will
    override the previous one.

    Use of `(:<typename>)` directive is optional.

-   Implicitly typed values.

    Implicitly typed value is a value that is not a *typed value*. That is, it
    doesn't start with explicit `typename`.

    Type of implicitly typed values is determined by the default type name
    specified by the last `(:<typename>)` directive or it can be provided
    externally (e.g. using `--type` command-line parameter in [Piqi
    tools](/doc/tools/)).

-   Embedded Piqi modules.

    Embedded Piqi modules contain type definitions. Each embedded Piqi module is
    a valid [Piqi language](/doc/piqi/) module, wrapped into `:piqi [ ... ]`
    top-level container.

    Embedded Piqi modules are treated specially. They are expected to represent
    valid Piqi specification. If they include or import other Piqi modules, they
    have to be either embedded earlier in the stream or be available as external
    `.piqi` files.

    When Piq entries that follow embedded Piqi modules are loaded, their type
    names are first searched in embedded Piqi modules.

Piq streams can be represented in three different [formats](/doc/encodings/):
Piq (the native format), Pib and JSON. Streams can be converted between the
formats using [Piqi tools](/doc/tools/) or written and read directly by OCaml
and Erlang programs.

**Examples:**

A simple Piq file containing three integer values:

    (:int)

    1 2 3

This example is equivalent to the previous one. The difference is that type for
each value is specified explicitly:

    :int 1
    :int 2
    :int 3

Piq stream can contain values of different types:

    :int 1
    :float 2.5
    :string "foo"

It is possible to have values with the default type and with explicitly
specified type in the same stream:

    (:int)

    1 2 3

    :float 1.0    % explicitly typed value

    4 5 % some more values with the default "int" type

    :string "bar"

Default type can be overridden by another `(:<typename>)` directive.

    % contents of file1

    (:int)

    1 2 3

    % contents of file2

    (:float)

    1.0 3.14 2.71

Note that all of the above examples are based on built-in types (`int`, `float`,
`string`). Real-world `.piq` files would likely contain values of some
user-defined types.

There are more examples of `.piq` files on [Examples](/examples/) page.


Style guidelines
----------------

There are several code formatting rules that should be used for `.piq` and
`.piqi` files. These rules are chosen for better code readability and easier
code manipulation using text editors.

For cases that are not covered in this section, refer to [Piqi pretty-printing
tool](/doc/tools/) (`piqi pp <.piq | .piqi file>` command).


### Indentation

Four space characters should be used for indentation.


### Parenthesis and bracket placement

If a closing bracket needs to be placed on a different line from the matching
opening bracket, it should be placed in the same column where the *named* or
*typed* or *list* value starts. The same rule applies to parentheses.

**Examples:**

    [
        ...
    ]

    .foo [
        ...
    ]

    :bar [
        ...
    ]


Notes
-----

-   There is no support for representing individual characters in Piq. And
    there's no such Piqi type as well.

-   Integer literals must fit into 64-bit unsigned range for positive values
    into 64-bit signed range for negative values.

-   Floating point literals are internally represented using IEEE 754
    double-precision format.

-   There's no concept of *Null* in Piq/Piqi. In other languages, *Null* is
    often used for representing "no value" or "undefined value". In Piq and Piqi
    data model, missing values are *missing*, meaning they are not present in
    containers and all other values are *defined*, which means that there are no
    undefined values.

-   Piq doesn't have keywords *per se*. However `true` and `false` words are
    reserved for boolean literals and can't be used in other contexts.


TODO list
---------

-   Line wrap for string literals.

    Currently there is no support for wrapping long string/binary literals to
    several lines.

    Once implemented, it will likely look like this:

        "multi-line string \
        "another line \
        "and the last one"

-   Multi-line nested comments.

    Currently only single-line comments are supported. There are some
    implementation ideas, but more experiments and prototypes are needed before
    they become finalized.

