<?php

function test1($x, $unused_parameter) {
  echo $x;

  $z = 1;
  echo $z;

  $unused_local = 1;
  $a = $unused_intermediate = 1;
  echo $a;

  $b = 1;
  echo "$b";

  echo $use_of_undeclared;


  $matches = array();
  foreach($matches as $k) {
    echo $k;
  }
  // $k2 is not used but it's ok, as long as one of the key/value is used
  foreach($matches as $k2 => $v) {
    echo $v;
  }

}


/* TODO

<?php

function test_list_var() {
  // this is parsed as (list(...) = false) or true; which
  // make list(...) to not be the toplevel expression in the statement
  // which does not work well with the (ugly) code I have in 
  // check_variables_php.ml
  list($a, $b) = false or true;
  echo $b;

  // this does not generate any warning
  list($a2, $b2) = (false or true);
  echo $b2;
}
*/
