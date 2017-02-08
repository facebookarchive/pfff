This document describes various data encodings supported by Piqi and the mapping
between data structured defined by the [Piqi](/doc/piqi/) language and the
supported encodings.

Piqi supports 5 data formats:

-   [Google Protocol Buffers](http://code.google.com/p/protobuf/)
-   [JSON](http://json.org)
-   XML
-   [Piq](/doc/piq/)

    Piq is a human-friendly data representation language designed as part of the
    Piqi project.

-   pib

    This is a new experimental binary data format based on Protocol Buffers. It
    is [described below](#pib) in this document.


Piqi provides native support for these data formats through language mappings
(currently, available for [OCaml](/doc/ocaml/) and [Erlang](/doc/erlang)).

In addition, Piqi includes command-line [tools](/doc/tools/) for type-based
conversion between these formats. By using these tools, one can easily implement
format conversion functionality for programs written in almost any language that
has the ability to write and read data in at least one of Protocol Buffers, JSON
or XML formats.


Protocol Buffers binary format (pb)
-----------------------------------

Since there is a direct mapping between Piqi types and Protocol Buffers types,
Protocol Buffers encoding rules can be described in two stages:

1.  Mapping from Piqi types to Protocol Buffer types.

    It is described in [Piqi -- Protocol Buffers compatibility](/doc/protobuf/)
    section of the current documentation.

2.  Rules for serializing Protocol Buffer data objects (values) into binary
    format.

    This part is described in Google Protocol Buffers documentation:
    <http://code.google.com/apis/protocolbuffers/docs/encoding.html>

-   This encoding can be used for serializing one value at a time. It is not
    suitable for encoding streams of values.

-   Information about value type is not included with the encoded representation
    of the value.

All composite Piqi values are mapped to Protocol Buffers `messages` containing
zero or more fields. For example, in case of `variant`, it is exactly one field
that represents one of the variant's options. A `message` for a Piqi `record`
will contain as many fields as the original record. Piqi `list` will be mapped
to a `message` with a single *repeated* field representing elements of the list.

Values of Piqi primitive types, including integers, floats, strings, binaries and
enums are wrapped in a Protobuf message with a single field representing the
value.

In Protocol Buffers terms, `messages` are encoded without any leading headers or
tags. In other words, `message` is actually represented as a sequence of encoded
fields.

Each field has a small header that contains field's unique integer identifier
(`number`) and a `wire type` that corresponds to the low-level representation
type used for encoding the field. By knowing field's `wire type` it is possible
to calculate the length of the field's encoded representation.

More information about details this encoding format can be found in Protocol
Buffers documentation.


JSON
----

For examples of how various Piqi values are represented in JSON, see the
[examples page](/examples/).

Primitive Piqi values, such as integers, floats and strings are represented using
correspondent JSON literals. Floating point NaN, positive and negative
infinities are represented as `"NaN`, `"Infinity"`, `"-Infinity"` JSON strings.

The only supported JSON encoding is UTF-8.

Piqi binaries are represented as JSON strings containing Base64-encoded binary
values.

Piqi records and variants are represented as JSON associative arrays (objects).

Repeated fields can represented as JSON arrays or, alternatively, as a single
field if only one instance of the field is defined. Such single-field
alternative representation helps to maintain backward compatibility when field
definition is turned from `optional` into `repeated`.

Optional Piqi fields that are missing are encoded as JSON fields having `null`
values.

JSON field names are derived from Piqi field names by replacing all `-`
characters with `_`. Sometimes, it is useful to define JSON field names
explicitly. One can do that by adding the `.json-name "<desired JSON name>"`
property to the Piqi field definition.

It is possible to remove missing optional fields (which values are represented
as `null`) and empty repeated fields (which values are represented as `[]`) from
JSON output entirely by using a special `json-missing-null-fields` conversion
option. For usage details see correspondent sections of documentation (it is
available in OCaml and Erlang serialization, command-line tools and Piqi-RPC).

In addition to the `json-omit-missing-fields` run-time setting, it it possible to
control this behavior for individual optional and repeated fields by specifying
the `.json-omit-missing true|false` field property in the schema definition. The
schema-level setting, when specified, takes precedence over the run-time
setting.

Piqi flags, i.e. optional fields without associated values, are represented as a
JSON boolean field set to `true` when the flag is present and excluded from the
output when the flag is missing.

Piqi lists are mapped to JSON arrays.

[JSON RFC](http://tools.ietf.org/html/rfc4627) states that "A JSON text is a
serialized object or array". In other words, primitive values such as integers,
strings or booleans do not make valid top-level JSON. Because of that, all
top-level Piqi values of primitive types are wrapped into a special top-level
object:

    {"value": <value>}

For example:

    {"value": "foo"}
    {"value": 10}
    {"value": false}

In addition, there are a couple of custom extensions. At the movement, they are
implemented and used only by [`piqi convert`](/doc/tools/#piqiconvert)
command-line tool. In all other places (including OCaml, Erlang and Piqi-RPC
serialization) these features are supported when reading from JSON but
never used when writing JSON.

1.  Including name of the Piqi type in serialized value representation

    Type names are included as a special field of serialized JSON objects. It is
    named "piqi_type" and always expected to be the first field. For example:

        {"piqi_type": "int", "value": 1}

2.  Wrapping top-level `list` values into JSON objects.

    Piqi lists are normally represented as JSON arrays. However, order to
    support the above extension that adds information about the type in the
    "piqi_type" field, Piqi wraps top-level JSON array into a JSON object. For
    example:

        {"piqi_type": "foo-list", "value": [...]}

One of the purposes of these extensions is to make JSON representation reversible
when converting from Piq stream to JSON. By including type information with each
encoded JSON value, `piqi convert` can reliably turn generated JSON values back
into Piq (some details of Piq representation can still be lost during Piq ->
JSON -> Piq conversion, but not the data).


XML
---

Support for XML format further extends Piqi's ability to interoperate with
various systems. For instance, it is possible to use existing XML tools such as
XPath, XSLT and XQuery for analyzing, querying and transforming Piqi data.

The only supported XML encoding is UTF-8.

Encoded values are placed inside `<value> ... </value>` top-level XML element.

Floating point NaN, positive and negative infinities are represented as `NaN`,
`Infinity`, `-Infinity` text nodes.

Piqi binaries are represented as XML text nodes containing Base64-encoded binary
values.

Whitespace in text XML nodes is significant.

XML attributes and namespaces are not allowed.

DTD and XML Schema(s) are not supported.

In theory, Piqi definitions can be converted to XML Schema .xsd definitions (but
not DTDs) as they are roughly equivalent and serve the same goal. However, it is
hard to foresee all the difficulties and corner cases without actually
implementing it.


Pib
---

Pib is an experimental binary format that mirrors Piq features. Effectively,
there is 1-to-1 mapping between Piq and Pib.

Unlike Piq, which main goal is to be as convenient as possible for people to
edit and read structured data, Pib is focused on being portable and fast to
process while offering a more compact encoding.

Being 100% compatible with Piq, Pib is a stream format supporting the same 3
types of entries.

- type hints
- explicitly typed values
- implicitly typed values

As a special type of values, Pib streams can include embedded Piqi modules
containing type definitions. This way, Pib streams can maintain full information
about types of data encoded in the stream.

At a high level, encoded Pib value is a pair of `(<typename>,
<encoded-value>)`, where `<typename>` is a fully-qualified Piqi name of the
value's type and `encoded-value` is a Piq value encoded in Protocol Buffers
format.

The actual stream encoding separates type names from encoded values by
introducing two separate low-level stream entries: type hints  and encoded
values. This method allows to associate one `<typename>` entry with many value
entries. If a stream contains several values of the same type, the name of the
type can be included in the stream only once. This helps to preserve spaces and
also simplifies programs that write streams containing values of only one type.

To summarize, encoded Pib streams contain two types of entries.

-   Type hints

    Type hint entry defines an association between a fully-qualified Piqi type
    name and an unsigned integer `<type-code>`.

    Type hint with code `1` has a special meaning. It represents Piq type hint
    (also known as *default type name*). In Piq stream, default type names are
    specified by `(:<typename>)` directive.

    Subsequent type hint entries in the stream override previous entries with
    the same type code.

    Type hints for embedded Piqi modules have `typename` set to `piqi`.

-   Encoded values

    Encoded value entry is represented as a pair of `(<type-code>,
    <encoded-value>)`. `<type-code>` must be previously defined by a *type hint*
    entry.

    Values with type code `1` correspond to *implicitly typed values* in Piq
    stream.


For example, if we have two values `v1` and `v2` of the same type `t`, they can
be encoded in a Pib stream as:

    (type-hint: typename = t, code = 2)
    (value: code = 2, value = v1)
    (value: code = 2, value = v2)

or in a more excessive way, in which each encoded value is prepended with a type
hint:

    (type-hint: typename = t, code = 2)
    (value: code = 2, value = v1)
    (type-hint: typename = t, code = 2)
    (value: code = 2, value = v2)

or even like that:

    (type-hint: typename = t, code = 2)
    (value: code = 2, value = v1)
    (type-hint: typename = t, code = 3)
    (value: code = 3, value = v2)

The first encoding is more compact compared to the other two. The last encoding
would be typically used if `v1` and `v2` had different types.

Stream values logically represented as `(code, value)` pairs are encoded using
Protocol Buffers encoding used for message fields.

Type hints are encoded the same way as values using a special 1073741823 (2\^29
- 1) code which is the maximum possible constant that can be used as a field
code in Protocol Buffers. Values of encoded type hints are defined as follows.

```
.record [
    .name pib-typehint

    .field [
        % this field should be always present in the first position and contain
        % the string "piqi-type"; if it is present, but has a different value,
        % it will be ignored

        .name piqi-type
        .type string

        .code 1
    ]

    .field [
        % fully-qualified piqi type name
        .type type

        .code 2
    ]

    .field [
        % wire code associated with the type; this code is used for identifying
        % elements in .pib stream; the range of the values is the same as the
        % range of valid protobuf wire codes

        .name code
        .type uint

        .code 3
    ]
]
```

One of the properties of the Pib format is that a Pib stream can be viewed as a
single Protocol Buffers message which fields correspond to individual entries in
the Pib stream. This has at least one practical effect: Pib streams can be
decoded and inspected by a standard `protoc --decode_raw` Protocol Buffers tool.

