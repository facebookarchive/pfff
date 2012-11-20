<?php

// another HPHP extension: user attributes.
// This is similar to http://en.wikipedia.org/wiki/Java_annotation

final class A { final public function foo() { /* .. */ } }
<< __MockClass >>
class MockD extends A { public function foo() { /* .. */ } }

<< __attribute >>
function foo() { }

// User attributes are denoted by double angle brackets (<< >>) before a
// function or class definition. One or more attributes can be listed between
// the angle brackets, separated by commas. An attribute is a name followed
// optionally by a list of scalar values

<< Foo, Bar(), Baz('hello',array(1,2)) >>
function f() { /* .. */ }

// Multiple attributes with the same name are not allowed. The scalar values
// in attribute expressions are not allowed to contain user defined constants.
// User attributes can be applied to classes and functions and methods, but
// they cannot be applied to closures.

// you can also have attribute on parameters
function g(<<Data>> $a, <<Data>> String $b, $c) {
}