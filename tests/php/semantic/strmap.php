<?hh

// some facebook extensions around arrays: strmap and intmap are new keywords

function __sm_strmap($arr) {
  echo 'in __sm_strmap';
  print_r($arr);
}

function __sm_intmap($arr) {
  echo 'in __sm_intmap';
  print_r($arr);
}

$x = strmap('a' => 1, 'b' => 2);
$y = intmap(1 => 1, 3 => 2);
