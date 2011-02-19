<?php
// false positives when in non strict mode
// thx to miaubiz for the bug reports

foreach(array(1,2) as $y)
{
  $x = $y;
} 
// this is ok in non-strict mode
error_log("ok: ".$x);

for($i=0;$i<=2;$i++)
{
  $z = $i;
} 
error_log("ok: ".$z);

//pad: old syntax is bad
//foreach(array(1,2) as $k):
//$p = $k;
//endforeach;
//error_log("ok: ".$p);

?>