Overview
--------

Piqi getopt is a system for parsing command-line arguments. It comes in two
flavors:

-   OCaml library, which functionality is used by several tools such as
    `piqi getopt` and `piqi call`.

-   The `piqi getopt` command-line tool which can be used from non-OCaml
    programs and interactively. Being able to call it manually is useful for
    debugging and understanding how Piqi getopt works. The `piqi getopt` command
    is documented [here](/doc/tools/#piqigetopt).

Major difference between Piqi getopt and all other "getopt" libraries and
techniques, is that Piqi getopt uses type definition to validate command-line
arguments and convert them into a data object instead of using ad-hoc parsing
specifications and manually-written parsing rules.

Piqi getopt can be applied to any user-defined or primitive Piqi type. That is
Piq data structure of any type, no matter how complex or deeply-nested the data
structure is, can be specified using command-line arguments.

The syntax of command-line arguments supported by Piqi getopt is roughly
equivalent to [Piq syntax](/doc/piq/).

Unlike Piq, Piqi getopt uses conventional `--<long-name>`, and `-<short-name>`
notations for field and option names (labels). Also, it allows to specify
strings and binaries as unquoted literals in addition to standard Piq quoted
strings. There are some other useful features such as being able to take the
value of an argument from a file using `@<file-name>` entries.

As in Piq, it is possible to omit field names and just specify field values
alone. In such case, Piqi getopt will attempt to guess which field it is based
on the given value and its position.

Basic syntax rules for command-line arguments
---------------------------------------------

In Piqi getopt, both short and long options are supported. Option names must be
separated from a value by whitespace, e.g.

    -c 10
    --c-long 10

Support for "glueing" long options with their values using `=` character (e.g.
`--long-option=10`) will be added in future versions.

Short options start with `-` character followed by one letter. When short
options are grouped together, each option is treated as if it was specified
separately. For example,

    -abc 10

is equivalent to

    -a -b -c 10

However, character `-` followed by a `<number>` is normally treated as a
negative number, e.g.

    -10
    -0.nan
    -0.0
    -0.infinity

Words are treated either as Piq strings, binaries or words, depending on the
expected type. Examples of words:

    a

    foo

Strings or binaries can also be specified explicitly using Piq string syntax.

    '"a"'

    '"foo\u0000"'

    '"\x00\n\r"'

Lists (and records) can be specified using regular Piq syntax, but `[` and `]`
characters must be specified as separate arguments and not as a part of other
arguments. Examples:

    [] // special case, equivalent to "[ ]"

    [ a b ] // this is correct
    [a b]   // this is incorrect

    [ a b 10 -1 ]

    [ a b [ c d ] ]

Values for arguments that start with `@` character will be loaded from a file
which name follows the `@` character. For example:

    @foo  // string or binary value will be loaded from file with the name "foo"

Examples
--------

Parsing values of built-in types:

    piqi getopt --type bool -- true
    piqi getopt --type int -- -10
    piqi getopt --type float -- -0.inf

    piqi getopt --type string -- foo     # parsing words as strings
    piqi getopt --type string -- foo\ bar
    piqi getopt --type string -- '"\tfoo\x20\u0045"'
    piqi getopt --type string -- "привет"
    piqi getopt --type binary -- '"\x00ab\tcd\xff\xfe"'

This example shows a `piqi getopt` shell command invocation for parsing the
object of type `person/person`. This type is defined in Piqi examples
[here](/examples/#person_piqi).

    piqi getopt -t json --add-defaults --type person/person -- \
        --name "J. Random Hacker" \
        --id 0 \
        --email "j.r.hacker@example.com" \
        --phone [ --number "(111) 123 45 67" ] \
        --phone [ \
            --number "(222) 123 45 67" \
            --mobile \
        ] \
        --phone [ \
            --number "(333) 123 45 67" \
            --work \
        ]

Below is the output of the command. Note that output format, specified by `-t`
option, can be any of JSON, XML, Protocol Buffers or Piq.

    {
      "name": "J. Random Hacker",
      "id": 0,
      "email": "j.r.hacker@example.com",
      "phone": [
        { "number": "(111) 123 45 67", "type": "home" },
        { "number": "(222) 123 45 67", "type": "mobile" },
        { "number": "(333) 123 45 67", "type": "work" }
      ]
    }

Alternatively, a standard Piq syntax can be used for field and option names. It
is can be shorter and slightly more readable in some case.

    piqi getopt --type person/person -- \
        .name "Joe User" \
        .id 1 \
        .phone [ "(444) 123 45 67" ] \
        .phone [ "(555) 123 45 67" .work ]

More `piqi getopt` examples are available in Piqi sources:
[](https://github.com/alavrik/piqi/blob/master/examples/test_getopt)

Using short options
-------------------

Piqi getopt support both long and short notation for label names. Long label
name start with `--` followed by more than one letters. Short name is a single
`-` followed by exactly one letter. Examples:

    # these are short names
    -a
    -x

    # these are long names
    --long-name
    --another-one
    --ab

Field and option names from Piqi specifications are represented as getopt long
names, unless Piqi name consists of one letter. In such case, it will be treated
as both short and long name.

Piqi provides a method for defining getopt short name for any long name in
`.piqi` specification. In order to do that, one needs to specify an additional
`.getopt-letter` property for a desired field or option. For example:

    .field [
            .name foo
            .type int

            .getopt-letter f
    ]

As a result, Piqi getopt will be recognizing `-f 10` as well as `--foo 10`.

In Piqi getopt, short names can be grouped together like in many other getopt
libraries. For example,

    -abc 10

is equivalent to

    -a -b -c 10

Comparison with Posix/GNU getopt
--------------------------------

Piqi getopt uses slightly different command-line argument syntax than Posix/GNU
getopt, because their syntax is way too relaxed and imprecise. These are
examples of GNU getopt options and their possible meanings:

    --c-long=10 // c-long = 10
    -c 10 // c, 10
    -c10 // c = 10
    -ac10 // a, c = 10
    -ac10 // c = a10

Equivalent Piqi getopt arguments would be:

    --c-long 10 // note there's no '=' sign between "--c-long" and "10"
    10 -c  // because "-c" 10 is always treated as "c = 10"
    -c 10 // because "-c10" means "-c10" in Piqi getopt and not "c = 10"
    -ac 10  // or -a -c 10
    -c a10
