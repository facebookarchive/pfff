<?php

// ./scheck -dataflow_pil tests/php/dataflow/dead_assign.php
function analysis4() {
  $a = 1;
  $a = 2;
  echo $a;
}
