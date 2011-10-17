<?php

class Bar {
}

$o = new Bar();

if($o instanceof Foo) {
  echo "instance of Foo\n";
}
