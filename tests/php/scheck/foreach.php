<?php

function test_foreach_pattern() {
  $xs = array(Pair {'a', 1}, Pair{'b',2});

  foreach($xs as list($a, $b)) {
    var_dump($a);
  }
}