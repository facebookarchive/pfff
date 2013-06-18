<?php

// Another HPHP extension, implicit fields.

// Scala has also this feature, see point 2 in
// http://codemonkeyism.com/top-5-things-to-know-about-constructors-in-scala/
// See also section 5.3 of the scala reference manual.

class A {
  // this is desugar as
  // public $x; __construct($x) { $this->x = $x; }
  public function __constuct(public $x) {
  }
  
}
